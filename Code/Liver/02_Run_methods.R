#####################
#### Run methods ####
#####################

rm(list = ls())

library(foreach)
library(doParallel)
library(nsprcomp)
library(glmnet)
library(Rfast)

nb.cores <- 5
data.name <- "Liver"
real.data <- T
bin.y <- F
file.prefix <- NULL

K.vals <- seq(10, 60, by = 10)
#K.vals <- 2^seq(1:5)
#K.vals <- c(2, 4)

source("../../Scripts/Set_file_paths.R")
source("../../Scripts/Load_all_functions.R")


load(file = paste0(data.path,"exp_data.RData"))
load(file = paste0(data.path,"exp_params.RData"))

folds.inner <- 5
folds.outer <- 10



#### Run Methods ####

#### VC-PCR-Identity ####

method.name <- "VC-PCR-Identity"
init.type <- "rand"
n.starts <- 5


delta.vals <- NA
lambda.vals <- seq(0.1, 1, by = 0.1)
lambda.vals <-  1 - 10^seq(log10(0.05), log10(0.8), length = 10)
lambda.vals <- seq((0.9), (1), length = 10)
alpha.vals <- NA


beg <- Sys.time()
source(paste0(script.path, "Run_method.R"))
end <- Sys.time()
end - beg
#Time difference of 6.271185 mins

rm(res.cv.list)
rm(inner.cv)
rm(outer.cv)

#### VC-PCR-Lasso ####

method.name <- "VC-PCR-Lasso"
init.type <- "rand"
n.starts <- 5

delta.vals <-  10^seq(log10(0.001), log10(1), length = 10)
#delta.vals <-  10^seq(log10(0.001), log10(1), length = 10)
lambda.vals <- 10^seq(log10(0.01), log10(1), length = 10)
lambda.vals <- seq((0.01), (1), length = 10)
#lambda.vals <- 10^seq(log10(0.01), log10(1), length = 10)
alpha.vals <- 1


beg <- Sys.time()
source(paste0(script.path, "Run_method.R"))
end <- Sys.time()
end - beg
#Time difference of  58.76463 mins

rm(res.cv.list)
rm(inner.cv)
rm(outer.cv)

#### VC-PCR-Ridge ####
gc()

method.name <- "VC-PCR-Ridge"
init.type <- "rand"
n.starts <- 5

delta.vals <- 10^seq(log10(0.05), log10(10), length = 10)
#lambda.vals <- 10^seq(log10(0.01), log10(1), length = 10)
lambda.vals <- seq((0.25), (1), length = 10)
# delta.vals <- 10^seq(log10(0.001), log10(10), length = 10)
# lambda.vals <- 10^seq(log10(0.2), log10(1), length = 10)
alpha.vals <- 0


beg <- Sys.time()
source(paste0(script.path, "Run_method.R"))
end <- Sys.time()
end - beg
#Time difference of 1.157162 hours





#### CRL kmeans ####
method.name <- "CRL-kmeans"
init.type <- "rand"
n.starts <- 5



alpha.vals <- NA
lambda.vals <- NA
delta.vals <- seq(0.02^(1/3), 1, length = 10)^3

beg <- Sys.time()
source(paste0(script.path, "Run_method.R"))
end <- Sys.time()
end - beg
#Time difference of 23.24481 secs

rm(res.cv.list)
rm(inner.cv)
rm(outer.cv)

#### CRL hclust ####
gc()
method.name <- "CRL-hclust"
init.type <- "rand"
n.starts <- 1

alpha.vals <- NA
lambda.vals <- NA
delta.vals <- seq(0.02^(1/3), 1, length = 10)^3

beg <- Sys.time()
source(paste0(script.path, "Run_method.R"))
end <- Sys.time()
end - beg
#Time difference of 9.145968 secs

rm(res.cv.list)
rm(inner.cv)
rm(outer.cv)

#### CRL hclust canonical correlation (agglomeration based on squared cancor) ####

gc()
method.name <- "CRL-hclustCC"
init.type <- "rand"
n.starts <- 1


alpha.vals <- NA
lambda.vals <- NA
delta.vals <- seq(0.02^(1/3), 1, length = 10)^3

beg <- Sys.time()
source(paste0(script.path, "Run_method.R"))
end <- Sys.time()
end - beg
#Time difference of 12.45662 mins

rm(res.cv.list)
rm(inner.cv)
rm(outer.cv)

#### CEN ####

method.name <- "CEN"
init.type <- "rand"
n.starts <- 5


alpha.vals <- NA
lambda.vals <- 10^seq(log10(0.001), log10(1), length = 10)
delta.vals <- 10^seq(log10(1), log10(100), length = 10)
# lambda.vals <- 10^seq(log10(0.01), log10(10), length = 10)
# delta.vals <- 10^seq(log10(2), log10(100), length = 10)

beg <- Sys.time()
source(paste0(script.path, "Run_method.R"))
end <- Sys.time()
print(end - beg)
#Time difference of 1.704619 hours

#### SRR ####

method.name <- "SRR"
init.type <- "FixedK"
n.starts <- 1

alpha.vals <- NA
lambda.vals <- 10^seq(log10(0.0001), log10(10), length = 10)
delta.vals <- 10^seq(log10(0.0001), log10(1), length = 10)


beg <- Sys.time()
source(paste0(script.path, "Run_method.R"))
end <- Sys.time()
print(end - beg)
#Time difference of 5.89417 hours