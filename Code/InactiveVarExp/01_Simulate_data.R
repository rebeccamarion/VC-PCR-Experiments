#######################
#### Simulate Data ####
#######################

rm(list = ls())

library(MASS)

#### Inactive variable experiment ####

data.name <- "InactiveVarExp"
source("../../Scripts/Set_file_paths.R")
source("../../Scripts/Load_all_functions.R")


settings <- expand.grid(n.p.ratio = c(0.25, 0.125), 
                        cor.x = c(0.6, 0.3))

settings.index <- 1
for (settings.index in 1:nrow(settings)){
  n.p.ratio <- settings[settings.index, "n.p.ratio"]
  cor.x <- settings[settings.index, "cor.x"]
  

## Fixed params

p <- 200
pk.active <- c(5, 5, 5, 5) # size of blocks for active variabes
pk.inactive <- c(5, 5, 5, 5, 5, 5) # size of blocks for inactive variables
p.inactive <- p - sum(pk.active)
s0 <- sum(pk.active)
betas <- c(1, -1, 1, -1)
sign.effects <- ifelse(abs(sum(sign(betas))) > 0, "same", "diff")
G0 <- sum(betas != 0) # number of active groups
SNR.model <- 10
n.rep <- 5
n <- p*n.rep*n.p.ratio
order.types <- c(1, 0, 1, 0, 1, 0, 1, 0, 0, 0)

## Varying params

inactive.type <- c("corr active and inactive", 
                   "corr inactive",
                   "uncorrelated")

sim.params <- cbind.data.frame(type = inactive.type, n = n/n.rep, p = p, p.inactive = p.inactive, cor.block = cor.x, G0 = G0, 
                    sign.effects = sign.effects, SNR.model = SNR.model, s0 = s0)

# All param settings
output <- lapply(1:nrow(sim.params), function(z) 
  GenInactiveVarExpData(n = sim.params$n[z]*n.rep, 
                        p = sim.params$p[z], 
                        pk.inactive = pk.inactive, 
                        pk.active = pk.active, 
                        betas = betas, 
                        SNR.model = sim.params$SNR.model[z], 
                        n.rep = n.rep, 
                        cor.x = sim.params$cor.block[z], 
                        inactive.type = sim.params$type[z], 
                        order.types = order.types))

file.prefix <- paste0("nsamples_", n.p.ratio*p, "_corx_", round(cor.x*100, 2), "_")
                
exp.data <- output
# Save simulation parameters
save(exp.data, file = paste0(data.path, file.prefix, "exp_data.RData"))

sds.y <- sapply(exp.data, function(x) x$sd.y)
exp.params <- cbind(sim.params, sd.y = sds.y)
# Save simulation parameters
save(exp.params,file = paste0(data.path, file.prefix, "exp_params.RData"))

}