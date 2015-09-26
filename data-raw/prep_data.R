#Script to unzip the raw data and turn it into R ff files
library(rain.prediction)

dir_in = paste(getwd(), "data-raw", sep="/")
dir_out = paste(getwd(), "data", sep="/")

files = c("train") #, "train")

zip2rds(files, dir_in, dir_out)
