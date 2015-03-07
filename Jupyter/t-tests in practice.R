# http://genomicsclass.github.io/book/pages/t-tests_in_practice.html

library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- tempfile()
download(url,destfile=filename)
dat <- read.csv(filename)
head(dat)
controlIndex <- which(dat$Diet=="chow")
treatmentIndex <- which(dat$Diet=="hf")

control <- dat[controlIndex,2]
treatment <- dat[treatmentIndex,2]
diff <- mean(treatment)-mean(control)
print(diff)
