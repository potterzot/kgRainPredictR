#If 1.5GB is too much to read into RAM, you can first convert to ffdf format
#Assumes you are starting in the package root directory

library(ff)
library(ffbase)
library(bit)
library(data.table)

setwd("data-raw/")

#tempdir should be something local
fftempdir <- options("fftempdir")
options("fftempdir" = "./")

#train data
system("unzip -n train.zip")
train <- read.csv.ffdf(file = "train.csv",
                       first.rows = 10000,
                       next.rows = 50000)
idx <- !is.na(train$Ref)
idx <- ffwhich(idx, idx == TRUE)
train <- as.ffdf(train[idx,])

ffsave(train,
       file = "../analysis/data/train",
       compress = TRUE)
rm(train)
file.remove("train.csv")

#Test file
system("unzip -n test.zip")
test <- read.csv.ffdf(file = "test.csv",
                       first.rows = 10000,
                       next.rows = 50000)

ffsave(test,
       file = "../analysis/data/test",
       compress = TRUE)
rm(test)
file.remove("test.csv")

options("fftempdir" = fftempdir)
setwd("../")





