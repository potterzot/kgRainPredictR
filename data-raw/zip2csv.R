#Unzip the raw data and save to the data/ directory,
#removing from training the records where an entire ID is missing Ref

#NOTE: You need at least 1.5GB of available RAM to run this.

setwd("data-raw/")

system("unzip -n train.zip")

#First find the IDs that have no valid Ref
d <- fread("train.csv", select=c("Id", "Ref"))
setkey(d, Id)
nnas <- as.bit(d[d[,!all(is.na(Ref)), by=Id], V1])
rm(d)

dat <- fread("train.csv")[nass,]
write.csv(dat, "../data/train.csv", append = FALSE)
file.remove("train.csv")

system("unzip -n test.zip")
system("mv test.csv ../data/test.csv")

setwd("../")
