library(spOccupancy)
library(coda)
library(stars)
library(ggplot2)
set.seed(102)

data(hbef2015)

sp.names <- dimnames(hbef2015$y)[[1]]
ovenHBEF <- hbef2015
ovenHBEF$y <- ovenHBEF$y[sp.names == "OVEN", , ]
table(ovenHBEF$y)

oven.occ.formula <- ~ scale(Elevation) + I(scale(Elevation)^2)
oven.det.formula <- ~ scale(day) + scale(tod) + I(scale(day)^2)

oven.inits <- list(alpha = c(0, 0, 0, 0), 
                   beta = c(0, 0, 0), 
                   z = apply(ovenHBEF$y, 1, max, na.rm = TRUE))
# Format with abbreviated specification of inits for alpha and beta.
oven.inits <- list(alpha = 0, 
                   beta = 0, 
                   z = apply(ovenHBEF$y, 1, max, na.rm = TRUE))

oven.priors <- list(alpha.normal = list(mean = 0, var = 2.72), 
                    beta.normal = list(mean = 0, var = 2.72))

n.samples <- 5000
n.burn <- 3000
n.thin <- 2
n.chains <- 3

out <- PGOcc(occ.formula = oven.occ.formula, 
             det.formula = oven.det.formula, 
             data = ovenHBEF, 
             inits = oven.inits, 
             n.samples = n.samples, 
             priors = oven.priors, 
             n.omp.threads = 1, 
             verbose = TRUE, 
             n.report = 1000, 
             n.burn = n.burn, 
             n.thin = n.thin, 
             n.chains = n.chains)

str(ovenHBEF)
