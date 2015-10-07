library(data.table)
library(ff)
library(bit)
library(dplyr)
library(zoo)
library(ggplot2)
library(kgRainPredictR)

#Assumes working directory is in package root.
ffload("analysis/data/train", overwrite = TRUE, rootpath = "data/")

###Create features for training data
#Load the initial columns
dat <- data.table(train[,c("Id", "radardist_km", "Expected")])

#collapse by Id
tr <- dat[,.(
  records = .N,
  rd = max(radardist_km),
  target = max(Expected)
), Id]
setkey(tr, Id)

#create adjusted target values
tr$targetNA <- tr$target
tr[target>69]$targetNA <- NA

classes <- seq(0,69,0.25)
tr <- tr[tr[, .(
  rdsq = rd**2,
  rdlg = log1p(rd),
  rd_inv =  1/(rd + 1),
  rdsq_inv = 1/(rd**2 + 1),
  target_adj = adjust_target(target),
  target_class = nearest_neighbor(targetNA, classes)
), Id]]
tr$targetlg <- log1p(tr$target_adj) + 0.00001

dat <- data.table(train[,c("Id", "minutes_past", "Ref")])

ref_feat <- dat[, .(
  dt = mean(time_difference(minutes_past)),
  ref = sum(time_difference(minutes_past) * Ref, na.rm = T),
  ref_i = sum(time_difference(minutes_past) * interpolate(Ref), na.rm = T),
  ref_s = sum(time_difference(minutes_past) *
                smooth(interpolate(Ref)), na.rm = T),
  mm = sum(time_difference(minutes_past) * Ref, na.rm = T),
  mm_i = sum(time_difference(minutes_past) *
               marshall_palmer(interpolate(Ref)), na.rm = T),
  mm_s = sum(time_difference(minutes_past) *
               marshall_palmer(smooth(interpolate(Ref))), na.rm = T),
  ref_na = sum(is.na(Ref))
), Id]


#Add the features to the collapsed training dataset
tr <- tr[ref_feat]
tr$mm_i_adj <- tr[, .(mm_i_adj = adjust_target(mm_i, v_max = NULL)), Id]$mm_i_adj
tr$centered <- tr[, .(centered = target_adj - mm_i_adj), Id]$centered

#Add distance squared measures
tr <- tr[tr[, .(
  refdist = ref_i * rd,
  refdistsq = ref_i * rd**2,
  rdsq = rd**2,
  mmdist = mm_i_adj * rd,
  mmdistsq = mm_i_adj * rd**2
),Id]
]

##Create averages of other time-weighted variables
dat <- data.table(train[,c("Id", "minutes_past",
                           "RefComposite", "RhoHV", "Zdr", "Kdp")])
tr <- tr[dat[, .(
  refc_i = sum(time_difference(minutes_past) * interpolate(RefComposite), na.rm = T),
  mmc_i = sum(time_difference(minutes_past) * marshall_palmer(interpolate(RefComposite)), na.rm = T),
  rhohv_im = mean(time_difference(minutes_past) * interpolate(RhoHV), na.rm = T),
  zdr_im = mean(time_difference(minutes_past) * interpolate(Zdr), na.rm = T),
  kdp_im = mean(time_difference(minutes_past) * interpolate(Kdp), na.rm = T)
), Id]]

#Save the training data.
saveRDS(tr, "analysis/data/train_grpd.rds")





