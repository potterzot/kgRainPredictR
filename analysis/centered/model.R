#Libraries and data
library(data.table)
library(MASS)
library(xgboost)
library(kgRainPredictR)

dtr <- readRDS("data/train_grpd.rds")[!is.na(targetNA)]
non_features <- c("Id", "dt", "ref_na", "centered",
                  "target", "targetNA", "target30", "target_adj", "targetlg")
features <- names(dtr)[!(names(dtr) %in% non_features)]

form <- formula(target_adj ~ mm_i_adj + rd + rdsq + rd_inv + rdsq_inv + rhohv_im + mmdist + mmdistsq)
m_lm <- lm(form, data=dtr)
m_glm <- glm(form, family = "gaussian", data=dtr)

feature_cols <- c("mm_i_adj", "rd", "rdsq", "rd_inv", "rdsq_inv",
                  "mmdist", "mmdistsq",
                  "rhohv_im", "zdr_im", "kdp_im")
classes <- seq(0,30, 0.25)
dtr$target_class <- dtr[, .(target_class = target_adj * 4), Id]$target_class

xgb_tr <- xgb.DMatrix(as.matrix(dtr[target_adj<=30, feature_cols, with = FALSE]),
                      label = dtr[target_adj<=30, target_adj], missing = NA)

m_xgb <- xgboost(data = xgb_tr,
                  maximize = FALSE,
                  missing = NA,
                  nrounds = 30,
                  #num_parallel_tree = 10,
                  #colsample_bytree = .75,
                  verbose = 1,
                  params = list(
                    #objective = "multi:softmax",
                    #num_class = 120,
                    base_score = 0,
                    eta = .5,
                    max.depth = 4,
                    nthread = 3,
                    eval_metric = mae,
                    subsample = 0.5
                  )
)

xgb_te <- xgb.DMatrix(as.matrix(dte[, feature_cols, with = FALSE]), missing = TRUE)
sam <- fread("data-raw/sample_solution.csv")

res <- data.frame(
  mlm = predict(m_lm2),
  mglm = predict(m_glm2),
  mm = dtr$mm_i_adj
)
p <- ggplot(data=res) +
  geom_bar(aes(x=tar), alpha = 0.5, fill = "black", binwidth = 0.25) +
  geom_bar(aes(x=mlm), alpha = 0.5, fill = "red", binwidth = 0.25) +
  geom_bar(aes(x=mglm), alpha = 0.5, fill = "yellow", binwidth = 0.25) +
  xlim(c(-10,10))
p





#Gamma model


#xgboost
dte <- readRDS("data/test_grpd.rds")
dte$mm_i_adj <- dte[, .(mm_i_adj = adjust_target(mm_i, v_max = NULL)), Id]$mm_i_adj
xgb_te <- xgb.DMatrix(as.matrix(dte[, feature_cols, with = FALSE]), missing = TRUE)
sam <- fread("data-raw/sample_solution.csv")

res <- data.frame(
  mlm  = predict(m_lm, newdata=dte),
  mglm = predict(m_glm, newdata=dte),
  mm = dte$mm_i_adj,
  sam = sam,
  xgb = predict(m_xgb, xgb_te, missing = NA)
)

p <- ggplot(data=res) +
  geom_density(aes(x=mlm), color = "red") +
  geom_density(aes(x=mglm), color = "yellow") +
  geom_density(aes(x=mm), color = "blue") +
  geom_density(aes(x=xgb), color = "black") +
  xlim(c(0,10))
p

res$Expected <- 0.5 * res$Expected + 0.5 * sam$Expected
createSubmission(res, "inst/interacted/submission.csv")

