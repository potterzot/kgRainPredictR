#Libraries and data
library(xgboost)
library(data.table)
library(kgRainPredictR)


tr <- readRDS("data/train_grpd.rds")

#Baseline Marshall Palmer MAE for comparison
n_samples <- 100000
n_rounds <- 10
sum_res <- 0
for (i in 1:n_rounds) {
  s <- sample(tr$Id, n_samples)
  s_err <- tr[Id %in% s, .(err = targetNA - mm), Id]
  res <- mae(s_err$err, na.rm = T)$value
  sum_res <- sum_res + res
}
print(paste("Average MAE:", sum_res/n_rounds))

feature_cols <- c("ref_i", "mm_i", "mm_s", "radardist", "records", "ref_na")

xgb_tr <- xgb.DMatrix(as.matrix(tr[!is.na(target30), feature_cols, with = FALSE]),
                      label = tr[!is.na(target30), target30], missing = NA)

history <- xgb.cv(data = xgb_tr,
                  nrounds = 2,
                  nthread = 2,
                  nfold = 3,
                  feval = mae,
                  num_class = 31,
                  params = list(
                    #eta = .5,
                    #gamma = .3,
                    #max_depth = 6,
                    #min_child = 1,
                    #subsample = 0.5,
                    objective = "multi:softmax",
                    base_score = 0
                  ))

bst <- xgboost(xgb_tr,
               maximize = FALSE,
               missing = NA,
               nrounds = 5,
               verbose = 2,
               num_class = 31,
               feval = mae,
               params = list(
                 base_score = 0,
                 objective = "multi:softmax",
                 eta = .7,
                 max.depth = 6,
                 nthread = 2,
                 eval_metric = mae
               )
)
rm(tr)

xgb.save(bst, 'inst/xgboost_simple/xgb_model.save')

###Load the test data
te <- readRDS("data/test_grpd.rds")
xgb_te <- xgb.DMatrix(as.matrix(te[, feature_cols, with = FALSE]), missing = NA)

res <- data.frame(
  Id = te$Id,
  Expected = predict(bst, xgb_te, missing = NA)
)

createSubmission(res, "inst/xgboost_simple/submission.csv")

