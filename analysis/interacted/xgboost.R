#Libraries and data
library(data.table)
library(xgboost)
library(kgRainPredictR)

tr <- readRDS("data/train_grpd.rds")

#Features to use
feature_cols <- c("ref_im", "refdist", "refdistsq",
                  "refc_i", "rhohv_i", "zdr_i", "kdp_i",
                  "mm_i", "mmc_i", "mmdist", "mmdistsq",
                  "radardist", "radardistsq",
                  "records", "ref_na")


#Train a subset of the data and save the rest to test against
ids <- tr$Id[!is.na(tr$target30)]
s_tr <- sort(sample(ids, 350000))
s_te <- ids[!(ids %in% s_tr)]
trtr <- tr[Id %in% s_tr]
trte <- tr[Id %in% s_te]
xgb_tr <- xgb.DMatrix(as.matrix(trtr[, feature_cols, with = FALSE]),
                      label = trtr[, target30], missing = NA)
xgb_te <- xgb.DMatrix(as.matrix(trte[, feature_cols, with = FALSE]),
                      label = trte[, target30], missing = NA)

bst <- xgboost(xgb_tr,
               maximize = FALSE,
               missing = NA,
               nrounds = 2,
               verbose = 1,
               num_class = 31,
               feval = mae,
               params = list(
                 base_score = 0,
                 objective = "multi:softmax",
                 eta = .7,
                 max.depth = 6,
                 nthread = 2,
                 eval_metric = mae,
                 subsample = 0.5
               )
)

imp <- xgb.importance(feature_cols, model = bst)
xgb.plot.importance(imp)

#test against the data
res <- data.frame(
  Id = trte$Id,
  Expected = predict(bst, xgb_te, missing = NA)
)
mae(trte$targetNA - res$Expected)

#################
#parameter tuning
#So let's select just a subset of features that worked best
feature_cols <- c("refc_i", "rhohv_i", "mm_i", "mmdist", "mmdistsq", "ref_im", "radardist")
xgb_tr <- xgb.DMatrix(as.matrix(tr[!is.na(target30), feature_cols, with = FALSE]),
                      label = tr[!is.na(target30), target30], missing = NA)

history <- xgb.cv(data = xgb_tr, missing = NA, maximize = FALSE, nthread = 2,
                  num_class = 31, nrounds = 15, nfold = 2,
                  feval = mae,
                  params = list(objective = "multi:softmax",
                    eta = .5, gamma = .4, max_depth = 2,
                    #min_child = 1,
                    subsample = 0.25, base_score = 0, eval_metric = mae
                  ))




#Submission
rm(trtr)
rm(trte)

feature_cols <- c("refc_i", "rhohv_i", "mm_i", "mmdist", "mmdistsq", "ref_im", "radardist")
xgb_tr <- xgb.DMatrix(as.matrix(tr[!is.na(target30), feature_cols, with = FALSE]),
                      label = tr[!is.na(target30), target30], missing = NA)
bst <- xgboost(data = xgb_tr, missing = NA, maximize = FALSE, nthread = 2,
               num_class = 31, nrounds = 25,
               feval = mae,
               params = list(objective = "multi:softmax",
                             eta = .5, gamma = .4, max_depth = 2,
                             #min_child = 1,
                             subsample = 0.75, base_score = 0, eval_metric = mae
               ))
rm(tr)
xgb.save(bst, 'inst/interacted/xgboost_model.save')

imp <- xgb.importance(feature_cols, model = bst)
xgb.plot.importance(imp)

te <- readRDS("data/test_grpd.rds")
feature_cols <- c("refc_i", "rhohv_i", "mm_i", "mmdist", "mmdistsq", "ref_im", "radardist")
xgb_te <- xgb.DMatrix(as.matrix(te[, feature_cols, with = FALSE]), missing = TRUE)
res <- data.frame(
  Id = te$Id,
  Expected = predict(bst, xgb_te, missing = NA)
)

sam <- fread("data-raw/sample_solution.csv")

res$Expected <- 0.5 * res$Expected + 0.5 * sam$Expected
createSubmission(res, "inst/interacted/submission.csv")

