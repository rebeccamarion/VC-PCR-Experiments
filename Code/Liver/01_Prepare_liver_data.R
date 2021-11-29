##########################
### Prepare Liver Data ###
##########################

rm(list = ls())
library(pbapply)
library(WGCNA)
library(pls)
data.name <- "Liver"


source("../../Scripts/Set_file_paths.R")
source("../../Scripts/Load_all_functions.R")

# Params
p.vary <- 1000
p.keep <- 200
n <- 50
n.comp <- 5

# Load data
exp <- read.csv(paste0(data.path, "LiverFemale3600.csv"))
clin <- read.csv(paste0(data.path, "ClinicalTraits.csv"))

# Format X (gene expression data)
gene.names <- exp[, "substanceBXH"]
gene.exp <- exp[, 9:ncol(exp)]
X <- t(gene.exp)
colnames(X) <- gene.names
has.NA <- which(apply(X, 2, function(x) sum(is.na(x)) > 0))
X <- X[, -has.NA]

# Format y
wh <- match(rownames(X), clin$Mice)
 y <- clin$weight_g[wh]/clin$length_cm[wh]
# y <- clin$Total_Chol[wh]
# y <- clin$HDL_Chol[wh]
#y <- clin$Glucose_Insulin[wh]
y.norm <- scale(y)
X.norm <- scale(X)



# # Remove outliers
# sampleTree <- hclust(dist(X), method = "average");
# # Plot the sample tree: Open a graphic output window of size 12 by 9 inches
# # The user should change the dimensions if the window is too large or too small.
# sizeGrWindow(12,9)
# par(cex = 0.6);
# par(mar = c(0,4,2,0))
# plot(sampleTree, main = "Sample clustering to detect outliers", sub="", xlab="", cex.lab = 1.5,
#      cex.axis = 1.5, cex.main = 2)

wh.outlier <- which(rownames(X) == "F2_221")
wh.y.NA <- which(is.na(y))
wh.rm <- c(wh.outlier, wh.y.NA)

y <- y[-wh.rm]
X <- X[-wh.rm, ]
X.norm <- scale(X)
y.norm <- scale(y)



set.seed(821)
wh.samples <- sample(1:nrow(X.norm), size = n, replace = F)
X <- X[wh.samples, ]
y <- y[wh.samples]
X.norm <- scale(X)
y.norm <- scale(y)


# cor.X <- cor(x = X.norm, use = "pairwise.complete.obs")
# cor.xy <- cor(x = X.norm, y = y.norm, use = "pairwise.complete.obs")
# #w.cor.X <- pblapply(1:ncol(cor.X), function(j) HarmonicMean(abs(cor.X[-j, j]))*abs(cor.xy[j]))
# w.cor.X <- pblapply(1:ncol(cor.X), function(j) mean(abs(cor.X[-j, j]))*abs(cor.xy[j]))
# w.cor.vals <- unlist(w.cor.X)
# var.ids <- 1:ncol(X)
# ord <- order(w.cor.vals, decreasing = T)
# wh.keep <- ord[1:p.keep]
# var.ids <- var.ids[wh.keep]
# X.out <- X.norm[, wh.keep]
# heatmap(abs(cor(X.out)))

train <- cbind.data.frame(y = y.norm, X.norm)
res <- plsr(y ~ ., ncomp = n.comp, data = train, validation = "CV")
summary(res)
plot(RMSEP(res), legendpos = "topright")
loadings <- res$loadings
loadings <- ScaleNonzeroL2Norm(loadings, center = F)

# most.varying <- order(apply(X, 2, var), decreasing = T)[1:p.vary]
# X.norm <- X.norm[, most.varying]
# X <- X[, most.varying]
# 
# PCA <- prcomp(x = X.norm)
# plot(PCA)
# n.comp <- 5
# loadings <- PCA$rotation[, 1:n.comp]
# Xty <- t(X.norm)%*%y.norm
# Xty.norm <- L2Norm(Xty)
# cos.xy <- Xty/Xty.norm
# 
# hmeans <- apply(loadings, 2, function(x) sapply(1:length(x), function(z)
#   HarmonicMean(c(abs(x[z]), abs(cos.xy[z])))))
hmeans <- abs(loadings)

total.keep <- (p.keep/n.comp)
ords <- as.data.frame(apply(hmeans, 2, function(x) order(x, decreasing = T)[1:total.keep]))
wh.keep <- unlist(ords)
#wh.keep <- order(abs(cos.xy), decreasing = T)[1:p.keep]
n.unique(wh.keep)
wh.keep <- unique(wh.keep)

X.out <- X.norm[, wh.keep]

# MakeHeatmapGG(map.data = abs(cor(X.out)), name.scale = "Abs Corr")
# 
# 
# heatmap(abs(cor(X.out)), scale = "none", Rowv = NA, Colv = NA)
# heatmap(abs(cor(X.out)), scale = "none")
pdf(paste0("../../Plots/", data.name, "/heatmap_", data.name, ".pdf"), width = 9)
MakeHeatmapGG(map.data = abs(cor(X.out)), max.min = c(0, 1), name.scale = "Abs Corr")
dev.off()
# wh.comp <- 2
# heatmap(abs(cor(X.norm[, ords[[wh.comp]]])), scale = "none")

# plot(loadings[ords[[wh.comp]], wh.comp])

set.seed(2274)
enet <- cv.glmnet(x = X.out, y = y.norm, alpha = 0.5, type.measure = "mse")
betas <- enet$glmnet.fit$beta[, which(enet$lambda == enet$lambda.min)]
heatmap(abs(cor(X.out[, which(betas != 0)])), scale = "none")
plot(enet)

#install.packages("Rtsne")
library(Rtsne)
tsne <- Rtsne(t(X.out), dims = 2, perplexity=20, verbose=TRUE, max_iter = 500)
plot(tsne$Y, t='n', main="tsne")
text(tsne$Y)


exp.data <- list(train = list(X = X.out, y = y.norm))
save(exp.data, file = paste0(data.path, "exp_data.RData"))

exp.params <- cbind(n = nrow(X.out), p = ncol(X.out))
save(exp.params, file = paste0(data.path, "exp_params.RData"))
