# main.R: this script runs all 11 models with varying amounts of within-season
#         replication to compare estimates of occupancy probability of the 
#         Ovenbird in Hubbard Brook Experimental Forest. 
# Author: Jeffrey W. Doser
# Approximate run time: 1 hour
rm(list = ls())
library(spOccupancy)
# Set seed to get the same sub-sampled data set
set.seed(100)
# Load HBEF data ----------------------------------------------------------
load('data/hbef-spOccupancy-data.rda')
# Matrix specifying how many replicate surveys at each site/year combination
rep.ind.mat <- apply(data.list$y, c(1, 2), function(a) sum(!is.na(a)))
n.models <- 11
mean.visits <- rep(NA, n.models)

# Wrapper function to write model for a given data set --------------------
run.model <- function(data.list) {
  # Pair-wise distance between all sites
  dist.hbef <- dist(data.list$coords)
  
  priors <- list(beta.normal = list(mean = 0, var = 2.72),
  	         alpha.normal = list(mean = 0, var = 2.72),
  	         sigma.sq.t.ig = c(2, 0.5),
  	         rho.unif = c(-1, 1),
  	         sigma.sq.ig = c(2, 1),
  	         phi.unif = c(3 / 7000, 3 / 300))
  
  n.batch <- 1600
  batch.length <- 25
  n.burn <- 20000
  n.thin <- 20
  n.chains <- 3
  
  out <- stPGOcc(occ.formula = ~ scale(elev) + I(scale(elev)^2) + scale(ppt) + scale(tmax),
  	         det.formula = ~ scale(day) + I(scale(day)^2) + scale(tod),
  	         data = data.list,
  	         priors = priors,
  	         cov.model = 'exponential',
  	         n.neighbors = 5,
  	         NNGP = TRUE,
  	         ar1 = TRUE,
  	         batch.length = batch.length,
  	         n.batch = n.batch,
  	         n.report = 100,
  	         n.burn = n.burn, 
  	         n.thin = n.thin,
  	         n.chains = n.chains)
  return(out)
}

# Single-visit model ------------------------------------------------------
data.single.visit <- data.list
data.single.visit$y <- data.list$y[, , 1, drop = FALSE]
data.single.visit$det.covs$day <- data.single.visit$det.covs$day[, , 1]
data.single.visit$det.covs$tod <- data.single.visit$det.covs$tod[, , 1]
mean.visits[1] <- mean(apply(data.single.visit$y, c(1, 2), function(a) sum(!is.na(a))))
out.sv <- run.model(data.single.visit)
beta.samples.sv <- out.sv$beta.samples
alpha.samples.sv <- out.sv$alpha.samples

# 20% double visit model --------------------------------------------------
data.dv.20 <- data.list
data.dv.20$y <- data.list$y[, , 1:2, drop = FALSE]
data.dv.20$det.covs$day <- data.dv.20$det.covs$day[, , 1:2]
data.dv.20$det.covs$tod <- data.dv.20$det.covs$tod[, , 1:2]
indx.dv <- which(rep.ind.mat >= 2, arr.ind = TRUE)
remove.indx.dv <- sample(1:nrow(indx.dv), round(nrow(indx.dv) * .80), replace = FALSE)
for (i in 1:length(remove.indx.dv)) {
  data.dv.20$y[indx.dv[remove.indx.dv[i], 1], indx.dv[remove.indx.dv[i], 2], sample(1:2, 1)] <- NA
}
mean.visits[2] <- mean(apply(data.dv.20$y, c(1, 2), function(a) sum(!is.na(a))))
out.dv.20 <- run.model(data.dv.20)
beta.samples.dv.20 <- out.dv.20$beta.samples
alpha.samples.dv.20 <- out.dv.20$alpha.samples

# 40% double visit model --------------------------------------------------
data.dv.40 <- data.list
data.dv.40$y <- data.list$y[, , 1:2, drop = FALSE]
data.dv.40$det.covs$day <- data.dv.40$det.covs$day[, , 1:2]
data.dv.40$det.covs$tod <- data.dv.40$det.covs$tod[, , 1:2]
indx.dv <- which(rep.ind.mat >= 2, arr.ind = TRUE)
remove.indx.dv <- sample(1:nrow(indx.dv), round(nrow(indx.dv) * .60), replace = FALSE)
for (i in 1:length(remove.indx.dv)) {
  data.dv.40$y[indx.dv[remove.indx.dv[i], 1], indx.dv[remove.indx.dv[i], 2], sample(1:2, 1)] <- NA
}
mean.visits[3] <- mean(apply(data.dv.40$y, c(1, 2), function(a) sum(!is.na(a))))
out.dv.40 <- run.model(data.dv.40)
beta.samples.dv.40 <- out.dv.40$beta.samples
alpha.samples.dv.40 <- out.dv.40$alpha.samples

# 60% double visit model --------------------------------------------------
data.dv.60 <- data.list
data.dv.60$y <- data.list$y[, , 1:2, drop = FALSE]
data.dv.60$det.covs$day <- data.dv.60$det.covs$day[, , 1:2]
data.dv.60$det.covs$tod <- data.dv.60$det.covs$tod[, , 1:2]
indx.dv <- which(rep.ind.mat >= 2, arr.ind = TRUE)
remove.indx.dv <- sample(1:nrow(indx.dv), round(nrow(indx.dv) * .40), replace = FALSE)
for (i in 1:length(remove.indx.dv)) {
  data.dv.60$y[indx.dv[remove.indx.dv[i], 1], indx.dv[remove.indx.dv[i], 2], sample(1:2, 1)] <- NA
}
mean.visits[4] <- mean(apply(data.dv.60$y, c(1, 2), function(a) sum(!is.na(a))))
out.dv.60 <- run.model(data.dv.60)
beta.samples.dv.60 <- out.dv.60$beta.samples
alpha.samples.dv.60 <- out.dv.60$alpha.samples

# 80% double visit model --------------------------------------------------
data.dv.80 <- data.list
data.dv.80$y <- data.list$y[, , 1:2, drop = FALSE]
data.dv.80$det.covs$day <- data.dv.80$det.covs$day[, , 1:2]
data.dv.80$det.covs$tod <- data.dv.80$det.covs$tod[, , 1:2]
indx.dv <- which(rep.ind.mat >= 2, arr.ind = TRUE)
remove.indx.dv <- sample(1:nrow(indx.dv), round(nrow(indx.dv) * .20), replace = FALSE)
for (i in 1:length(remove.indx.dv)) {
  data.dv.80$y[indx.dv[remove.indx.dv[i], 1], indx.dv[remove.indx.dv[i], 2], sample(1:2, 1)] <- NA
}
mean.visits[5] <- mean(apply(data.dv.80$y, c(1, 2), function(a) sum(!is.na(a))))
out.dv.80 <- run.model(data.dv.80)
beta.samples.dv.80 <- out.dv.80$beta.samples
alpha.samples.dv.80 <- out.dv.80$alpha.samples

# Complete double-visit model ---------------------------------------------
data.dv <- data.list
data.dv$y <- data.list$y[, , 1:2, drop = FALSE]
data.dv$det.covs$day <- data.dv$det.covs$day[, , 1:2]
data.dv$det.covs$tod <- data.dv$det.covs$tod[, , 1:2]
out.dv <- run.model(data.dv)
mean.visits[6] <- mean(apply(data.dv$y, c(1, 2), function(a) sum(!is.na(a))))
beta.samples.dv <- out.dv$beta.samples
alpha.samples.dv <- out.dv$alpha.samples

# 20% three visit model --------------------------------------------------
data.tv.20 <- data.list
data.tv.20$y <- data.list$y[, , 1:3, drop = FALSE]
data.tv.20$det.covs$day <- data.tv.20$det.covs$day[, , 1:3]
data.tv.20$det.covs$tod <- data.tv.20$det.covs$tod[, , 1:3]
indx.tv <- which(rep.ind.mat >= 3, arr.ind = TRUE)
remove.indx.tv <- sample(1:nrow(indx.tv), round(nrow(indx.tv) * .80), replace = FALSE)
for (i in 1:length(remove.indx.tv)) {
  data.tv.20$y[indx.tv[remove.indx.tv[i], 1], indx.tv[remove.indx.tv[i], 2], sample(1:3, 1)] <- NA
}
mean.visits[7] <- mean(apply(data.tv.20$y, c(1, 2), function(a) sum(!is.na(a))))
out.tv.20 <- run.model(data.tv.20)
beta.samples.tv.20 <- out.tv.20$beta.samples
alpha.samples.tv.20 <- out.tv.20$alpha.samples

# 40% three visit model --------------------------------------------------
data.tv.40 <- data.list
data.tv.40$y <- data.list$y[, , 1:3, drop = FALSE]
data.tv.40$det.covs$day <- data.tv.40$det.covs$day[, , 1:3]
data.tv.40$det.covs$tod <- data.tv.40$det.covs$tod[, , 1:3]
indx.tv <- which(rep.ind.mat >= 3, arr.ind = TRUE)
remove.indx.tv <- sample(1:nrow(indx.tv), round(nrow(indx.tv) * .60), replace = FALSE)
for (i in 1:length(remove.indx.tv)) {
  data.tv.40$y[indx.tv[remove.indx.tv[i], 1], indx.tv[remove.indx.tv[i], 2], sample(1:3, 1)] <- NA
}
mean.visits[8] <- mean(apply(data.tv.40$y, c(1, 2), function(a) sum(!is.na(a))))
out.tv.40 <- run.model(data.tv.40)
beta.samples.tv.40 <- out.tv.40$beta.samples
alpha.samples.tv.40 <- out.tv.40$alpha.samples

# 60% three visit model --------------------------------------------------
data.tv.60 <- data.list
data.tv.60$y <- data.list$y[, , 1:3, drop = FALSE]
data.tv.60$det.covs$day <- data.tv.60$det.covs$day[, , 1:3]
data.tv.60$det.covs$tod <- data.tv.60$det.covs$tod[, , 1:3]
indx.tv <- which(rep.ind.mat >= 3, arr.ind = TRUE)
remove.indx.tv <- sample(1:nrow(indx.tv), round(nrow(indx.tv) * .40), replace = FALSE)
for (i in 1:length(remove.indx.tv)) {
  data.tv.60$y[indx.tv[remove.indx.tv[i], 1], indx.tv[remove.indx.tv[i], 2], sample(1:3, 1)] <- NA
}
mean.visits[9] <- mean(apply(data.tv.60$y, c(1, 2), function(a) sum(!is.na(a))))
out.tv.60 <- run.model(data.tv.60)
beta.samples.tv.60 <- out.tv.60$beta.samples
alpha.samples.tv.60 <- out.tv.60$alpha.samples

# 80% three visit model --------------------------------------------------
data.tv.80 <- data.list
data.tv.80$y <- data.list$y[, , 1:3, drop = FALSE]
data.tv.80$det.covs$day <- data.tv.80$det.covs$day[, , 1:3]
data.tv.80$det.covs$tod <- data.tv.80$det.covs$tod[, , 1:3]
indx.tv <- which(rep.ind.mat >= 3, arr.ind = TRUE)
remove.indx.tv <- sample(1:nrow(indx.tv), round(nrow(indx.tv) * .20), replace = FALSE)
for (i in 1:length(remove.indx.tv)) {
  data.tv.80$y[indx.tv[remove.indx.tv[i], 1], indx.tv[remove.indx.tv[i], 2], sample(1:3, 1)] <- NA
}
mean.visits[10] <- mean(apply(data.tv.80$y, c(1, 2), function(a) sum(!is.na(a))))
out.tv.80 <- run.model(data.tv.80)
beta.samples.tv.80 <- out.tv.80$beta.samples
alpha.samples.tv.80 <- out.tv.80$alpha.samples

# Full model --------------------------------------------------------------
mean.visits[11] <- mean(apply(data.list$y, c(1, 2), function(a) sum(!is.na(a))))
out.tv <- run.model(data.list)
beta.samples.tv <- out.tv$beta.samples
alpha.samples.tv <- out.tv$alpha.samples

# Save coefficients and occupancy means to hard drive ---------------------
psi.means <- c(mean(out.sv$psi.samples), mean(out.dv.20$psi.samples),
	       mean(out.dv.40$psi.samples), mean(out.dv.60$psi.samples), 
	       mean(out.dv.80$psi.samples), mean(out.dv$psi.samples), 
	       mean(out.tv.20$psi.samples), mean(out.tv.40$psi.samples), 
               mean(out.tv.60$psi.samples), mean(out.tv.80$psi.samples),
	       mean(out.tv$psi.samples))
names(psi.means) <- c('SV', 'DV.20', 'DV.40', 'DV.60', 'DV.80', 
		      'DV', 'TV.20', 'TV.40', 'TV.60', 'TV.80', 'TV')
save(psi.means, beta.samples.sv, beta.samples.dv.20, 
     beta.samples.dv.40, beta.samples.dv.60, 
     beta.samples.dv.80, beta.samples.dv, beta.samples.tv.20, 
     beta.samples.tv.40, beta.samples.tv.60, beta.samples.tv.80, 
     beta.samples.tv, alpha.samples.sv, alpha.samples.dv.20, 
     alpha.samples.dv.40, alpha.samples.dv.60, 
     alpha.samples.dv.80, alpha.samples.dv, alpha.samples.tv.20, 
     alpha.samples.tv.40, alpha.samples.tv.60, alpha.samples.tv.80, 
     alpha.samples.tv, mean.visits, file = 'results/hbef-analysis-results.rda')
