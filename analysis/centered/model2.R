dtr <- readRDS("data/train_grpd.rds")[!is.na(targetNA)]
non_features <- c("Id", "dt", "ref_na", "centered",
                  "target", "targetNA", "target30", "target_adj", "targetlg")
features <- names(dtr)[!(names(dtr) %in% non_features)]

#Centered output, so the submission will be mm_i + predicted.
#By centering, we end up with a normal distribution around 0.
#correlation of features with target
cor(dtr[!is.na(centered),features, with = FALSE], y=dtr[!is.na(centered)]$centered)

#Linear and GLM models
form <- formula(centered ~ rd + rdsq + rd_inv + rdsq_inv + rhohv_im + mmdist + mmdistsq)
m_lm <- lm(form, data=dtr)
m_glm <- glm(form, family = gaussian, data=dtr)

#CV to check model fit

#Gradiant Boost
feature_cols <- c("rd", "rdsq", "rd_inv", "rdsq_inv",
                  "mmdist", "mmdistsq",
                  "rhohv_im", "zdr_im", "kdp_im")
xgb_tr <- xgb.DMatrix(as.matrix(dtr[, feature_cols, with = FALSE]),
                      label = dtr[, centered], missing = NA)

m_xgb_centered <- xgboost(data = xgb_tr,
                          maximize = FALSE,
                          missing = NA,
                          nrounds = 30,
                          #num_parallel_tree = 10,
                          #colsample_bytree = .75,
                          verbose = 1,
                          params = list(
                            #num_class =,
                            base_score = 0,
                            eta = .5,
                            max.depth = 5,
                            nthread = 3,
                            eval_metric = mae,
                            subsample = 0.5
                          )
)

imp <- xgb.importance(feature_cols, model = m_xgb)
xgb.plot.importance(imp)

#Graph the different models
res <- data.frame(
  mlm  = predict(m_lm),
  mglm = predict(m_glm),
  tar = dtr$centered
)
p <- ggplot(data=res) +
  geom_bar(aes(x=mlm), alpha = 0.5, fill = "red", binwidth = 0.25) +
  geom_bar(aes(x=mglm), alpha = 0.5, fill = "yellow", binwidth = 0.25) +
  geom_bar(aes(x=tar), alpha = 0.5, fill = "black", binwidth = 0.25) +
  xlim(c(-10,10))
p

p2 <- ggplot(data=res) +
  geom_point(aes(x=tar, y=mlm), alpha = 0.5, fill = "red") +
  geom_point(aes(x=tar, y=mglm), alpha = 0.5, color = "black", shape = 1)
p2

#Generate Prediction
dte <- readRDS("data/test_grpd.rds")
dte$mm_i_adj <- dte[, .(mm_i_adj = adjust_target(mm_i, v_max = NULL)), Id]$mm_i_adj
dte$centered <- dte[, .(centered = target_adj - mm_i_adj), Id]$centered
xgb_te <- xgb.DMatrix(as.matrix(dte[, feature_cols, with = FALSE]), missing = TRUE)

res <- data.frame(
  Id = dte$Id,
  mm = dte$mm_i_adj,
  mlm = dte$mm_i_adj + predict(m_lm, newdata = dte),
  mglm = dte$mm_i_adj + predict(m_glm, newdata = dte),
  mxgb = dte$mm_i_adj + predict(m_xgb_centered, newdata = xgb_te, missing = NA)
)
p <- ggplot(data = res) +
  geom_bar(aes(x=mm), fill = "red", alpha = 0.5) +
  geom_bar(aes(x=mlm), fill = "yellow", alpha = 0.5) +
  geom_bar(aes(x=mglm), fill = "green", alpha = 0.5) +
  geom_bar(aes(x=mxgb), fill = "blue", alpha = 0.5) +
  xlim(c(-10,10))
p
