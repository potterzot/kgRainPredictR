#Libraries and data
library(gbm)
library(caret)
library(data.table)
library(kgRainPredictR)

tr <- readRDS("data/train_grpd.rds")

#XGB is about 1.1 right now.
feature_cols <- c("ref_i", "radardist", "records", "ref_na")
in_tr <- createDataPartition(tr$target30, p = 0.75)

fit_ctrl <- trainControl(method = "repeatedcv",
                         number = 2,
                         repeats = 2)

set.seed(8250)
gbm_f1 <- train(Class ~ ., data = tr,
                 method = "gbm",
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)


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

