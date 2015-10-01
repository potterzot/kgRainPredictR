#If 1.5GB is too much to read into RAM, you can first convert to ffdf format

library(ff)
library(ffbase)
library(bit)
library(data.table)


#tempdir should be something local
fftempdir <- options("fftempdir")
options("fftempdir" = "./")

setwd("data-raw/")

#train data
system("unzip -n train.zip")
train <- read.csv.ffdf(file = "train.csv",
                       first.rows = 10000,
                       next.rows = 50000)

d <- data.table(train[,c("Id", "Ref")])
setkey(d, Id)
nnas <- as.bit(d[d[,!all(is.na(Ref)), by=Id], V1])
rm(d)

N <- length(nnas)
n_chunks <- 1400
chunk_size <- ceiling(N / n_chunks)
for (i in 1:n_chunks) {
  idx <- 1:chunk_size + (i-1)*chunk_size
  if (i == n_chunks) idx <- (1+(n_chunks-1)*chunk_size):N
  dat <- data.table(train[idx,])[nnas[idx],]
  if (i == 1) write.csv(dat, "../data/train.csv", row.names = FALSE)
  else write.csv(dat, "../data/train.csv",
                 row.names = FALSE,
                 append = TRUE)
}
rm(dat)
rm(train)

#Now read in the csv and save it as an ff object
train <- read.csv.ffdf(file = "../data/train.csv",
                       first.rows = 10000,
                       next.rows = 50000)
ffsave(train,
       file = "../data/train",
       compress = FALSE,
       rootpath = ".")
rm(train)

file.remove("train.csv")
options("fftempdir" = fftempdir)

#Test file
system("unzip -n test.zip")
test <- read.csv.ffdf(file = "data-raw/test.csv",
                       first.rows = 10000,
                       next.rows = 50000)

ffsave(test,
       file = "data/test",
       compress = FALSE,
       rootpath = ".")
rm(test)

setwd("../")





