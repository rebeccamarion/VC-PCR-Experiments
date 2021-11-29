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
data.name <- "InactiveVarExp"
real.data <- F
bin.y <- F

source("../../Scripts/Set_file_paths.R")
source("../../Scripts/Load_all_functions.R")

data.files <- list.files(data.path)
data.files <- data.files[grep("exp_data", data.files)]
file.prefixes <- unlist(strsplit(data.files, "exp_data.RData"))

settings.index <- 1
for (settings.index in 1:(length(data.files))){
  
file.prefix <- file.prefixes[settings.index]

load(file = paste0(data.path, file.prefix, "exp_data.RData"))
load(file = paste0(data.path, file.prefix, "exp_params.RData"))

folds.inner <- 5
folds.outer <- 10


#### Run Methods ####

#### VC-PCR-Identity ####

method.name <- "VC-PCR-Identity"
init.type <- "rand"
n.starts <- 5


delta.vals <- NA
lambda.vals <- seq(0, 0.9, by = 0.1)
alpha.vals <- NA


print(paste0("Beginning VC-PCR-Identity for setting ", settings.index, " of ", length(data.files)))
beg <- Sys.time()
source(paste0(script.path, "Run_method.R"))
end <- Sys.time()
print(end - beg)
#Time difference of 9.741651 mins

#### VC-PCR-Lasso ####

method.name <- "VC-PCR-Lasso"
init.type <- "rand"
n.starts <- 5

delta.vals <-  10^seq(log10(0.00001), log10(0.2), length = 10)
lambda.vals <- 10^seq(log10(0.001), log10(1), length = 10)
alpha.vals <- 1


print(paste0("Beginning VC-PCR-Lasso for setting ", settings.index, " of ", length(data.files)))
beg <- Sys.time()
source(paste0(script.path, "Run_method.R"))
end <- Sys.time()
print(end - beg)
#Time difference of 1.386004 hours

#### VC-PCR-Ridge ####
gc()
method.name <- "VC-PCR-Ridge"
init.type <- "rand"
n.starts <- 5


delta.vals <- 10^seq(log10(0.0001), log10(0.01), length = 10)
lambda.vals <- 10^seq(log10(0.001), log10(1), length = 10)
alpha.vals <- 0

print(paste0("Beginning VC-PCR-Ridge for setting ", settings.index, " of ", length(data.files)))
beg <- Sys.time()
source(paste0(script.path, "Run_method.R"))
end <- Sys.time()
print(end - beg)
#Time difference of 1.502654 hours


#### CRL kmeans ####
method.name <- "CRL-kmeans"
init.type <- "rand"
n.starts <- 5



alpha.vals <- NA
lambda.vals <- NA
delta.vals <- seq(0, 1, length = 10)^2

print(paste0("Beginning CRL-kmeans for setting ", settings.index, " of ", length(data.files)))
beg <- Sys.time()
source(paste0(script.path, "Run_method.R"))
end <- Sys.time()
print(end - beg)
#Time difference of 34.15229 secs

#### CRL hclust ####
gc()
method.name <- "CRL-hclust"
init.type <- "rand"
n.starts <- 1


alpha.vals <- NA
lambda.vals <- NA
delta.vals <- seq(0, 1, length = 10)^2

print(paste0("Beginning CRL-hclust for setting ", settings.index, " of ", length(data.files)))
beg <- Sys.time()
source(paste0(script.path, "Run_method.R"))
end <- Sys.time()
print(end - beg)
#Time difference of 11.80903 secs

#### CRL hclust canonical correlation (agglomeration based on squared cancor) ####

gc()
method.name <- "CRL-hclustCC"
init.type <- "rand"
n.starts <- 1


alpha.vals <- NA
lambda.vals <- NA
delta.vals <- seq(0, 1, length = 10)^2

print(paste0("Beginning CRL-hclustCC for setting ", settings.index, " of ", length(data.files)))
beg <- Sys.time()
source(paste0(script.path, "Run_method.R"))
end <- Sys.time()
print(end - beg)
#Time difference of 16.98935 mins


#### CEN ####

method.name <- "CEN"
init.type <- "rand"
n.starts <- 5


alpha.vals <- NA
lambda.vals <- 10^seq(log10(0.01), log10(10), length = 10)
delta.vals <- 10^seq(log10(0.01), log10(100), length = 10)

print(paste0("Beginning CEN for setting ", settings.index, " of ", length(data.files)))
beg <- Sys.time()
source(paste0(script.path, "Run_method.R"))
end <- Sys.time()
print(end - beg)
#Time difference of 6.323578 hours



#### SRR ####

method.name <- "SRR"
init.type <- "FixedK"
n.starts <- 1

alpha.vals <- NA
lambda.vals <- 10^seq(log10(0.0001), log10(10), length = 10)
delta.vals <- 10^seq(log10(0.0001), log10(1), length = 10)

print(paste0("Beginning SRR for setting ", settings.index, " of ", length(data.files)))
beg <- Sys.time()
source(paste0(script.path, "Run_method.R"))
end <- Sys.time()
print(end - beg)
#Time difference of 2.639498 hours

}
