#Libraries and data
library(data.table)
library(MASS)
library(kgRainPredictR)

dtr <- readRDS("data/train_grpd.rds")[!is.na(targetNA)]
non_features <- c("Id", "dt", "ref_na", "centered",
                  "target", "targetNA", "target30", "target_adj", "targetlg")
features <- names(dtr)[!(names(dtr) %in% non_features)]

#correlation of features with target
cor(dtr[,features, with = FALSE], y=dtr$target_adj)

#First let's get a sense of the overall distribution
gamma_dist <- fitdistr(dtr[, targetlg], 'gamma')
wei_dist <- fitdistr(dtr[, targetlg], 'weibull')

#Plot some fake and the actual data
data_size = 100000
df <- data.frame(
  gamma = rgamma(data_size,
                 shape = gamma_dist$estimate[1],
                 rate = gamma_dist$estimate[2]),
  weibull = rweibull(data_size,
                     shape = wei_dist$estimate[1],
                     scale = wei_dist$estimate[2]),
  real = sample(dtr[, targetlg], data_size))
p <- ggplot(data = df) +
  geom_density(aes(x=gamma), color = "yellow") +
  geom_density(aes(x=weibull), color = "green") +
  geom_density(aes(x=real)) +
  ylim(c(0,2)) + xlim(c(0,5))
p

#Separate the training data so that we can try out GLM Gamma
feature_cols <- c("mm_i", "mmdist", "mmdistsq", "rhohv_i", "zdr_i", "kdp_i")
sample_size = 350000
ids <- dtr$Id
s_tr <- sort(sample(ids, sample_size))
s_te <- ids[!(ids %in% s_tr)]
trtr <- dtr[Id %in% s_tr, c("Id", "targetlg", "target_adj", feature_cols), with = FALSE]
trte <- dtr[Id %in% s_te, c("Id", "targetlg", "target_adj", feature_cols), with = FALSE]

#formula
form <- formula(target_adj ~ mm_i + mmdist + mmdistsq + rhohv_i + zdr_i + kdp_i)

glm_gamma <- glm(form, family = Gamma(link = "log"), data = trtr)
gamma_shape <- gamma.shape(glm_gamma)
res <- predict(glm_gamma, newdata = trte, type = "link",
               se = T, dispersion = gamma_shape$alpha)
summary(glm_gamma, dispersion = 1/gamma_shape$alpha)

#Plot actual and predicted
resdf <- data.frame(
  mp = trtr[, mm_i],
  actual = trtr[, target_adj],
  pred = res$fit[1:sample_size]
)
p <- ggplot(data = resdf) +
  geom_density(aes(x=pred), color = "yellow") +
  geom_density(aes(x=mp), color = "green") +
  geom_density(aes(x=actual)) +
  ylim(c(0,2)) + xlim(c(0,4))
p

#Calculate MAE
mae(resdf$actual - resdf$mp)
err <- mae(resdf$actual - resdf$pred)

####
#Train the distribution on the full data
glm_gamma <- glm(form, family = Gamma(link = "log"), data = dtr)
gamma_shape <- gamma.shape(glm_gamma)
pred <- predict(glm_gamma, type = "link",
                se = T, dispersion = gamma_shape$alpha)
mae(dtr$target_adj - expm1(pred))
res <- data.frame(
  Id = te[,Id],
  Expected = predict(glm_gamma, newdata = te, type = "link",
               se = T, dispersion = gamma_shape$alpha)
)

sam <- fread("data-raw/sample_solution.csv")

res$Expected <- 0.5 * res$Expected + 0.5 * sam$Expected
createSubmission(res, "inst/interacted/submission.csv")

