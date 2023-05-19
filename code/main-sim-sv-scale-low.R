# main-sim-sv-scale-low.R: script to run a set of simulations to assess identifiability
#                          of a single-visit multi-season occupancy model under varying
#                          levels of spatial and temporal autocorrelation. This script
#                          simulates data that are "misspecified" using a scale parameter
#                          of 0.5. 
# Authors: Jeffrey W. Doser and Sara Stoudt

rm(list = ls())
library(spOccupancy)

# Parameters for simulation -----------------------------------------------
# Number of data sets for each scenario
n.sims <- 100
set.seed(10110)
my.seeds <- sample(1:100000, n.sims, replace = FALSE)
# Spatial locations (400 total)
J.x <- 30
J.y <- 40
J <- J.x * J.y
# Number of years (keep constant for now, but will likely want to explore 
#                  this as well). 
n.time <- 10
# Only one replicate (aka single visit)
n.rep <- matrix(1, J, n.time)
# Occurrence coefficient --------------
# Generate a single covariate
beta <- c(0, 0.5)
p.occ <- length(beta)
# Detection coefficient ---------------
# A single covariate on detection
alpha <- c(0, -0.5)
# Spatial parameters ------------------
sp <- TRUE
# Assume an exponential correlation model for now. 
cov.model <- 'exponential'
# Spatial variances
sigma.sq.vals <- c(0.3, 1.5)
# Spatial decay
# NOTE: simTOcc generates data across a unit square. When using an exponential 
#       correlation function, the effective spatial range (distance at which 
#       correlation between sites drops to 0.05) is 3 / phi. Thus, the following
#       values correspond to effective spatial ranges of 20% and 80% of the 
#       study region. 
phi.vals <- c(3 / .2, 3 / .8)
# Temporal parameters -----------------
rho.vals <- c(0.5, 0.9)
sigma.sq.t.vals <- c(0.3, 1.5)
# Total number of simulation scenarios
n.scenarios <- length(sigma.sq.vals) * length(phi.vals) * 
               length(rho.vals) * length(sigma.sq.t.vals)
# Different combinations of all the four parameters that vary
scenario.vals <- expand.grid(sigma.sq = sigma.sq.vals, phi = phi.vals, 
			     rho = rho.vals, sigma.sq.t = sigma.sq.t.vals)
psi.true <- array(NA, dim = c(J, n.time, n.sims, n.scenarios))
psi.mean.samples <- array(NA, dim = c(J, n.time, n.sims, n.scenarios))
psi.low.samples <- array(NA, dim = c(J, n.time, n.sims, n.scenarios))
psi.high.samples <- array(NA, dim = c(J, n.time, n.sims, n.scenarios))
beta.mean.samples <- array(NA, dim = c(p.occ, n.sims, n.scenarios))
beta.low.samples <- array(NA, dim = c(p.occ, n.sims, n.scenarios))
beta.high.samples <- array(NA, dim = c(p.occ, n.sims, n.scenarios))

n.samples <- 25000
batch.length <- 25
n.batch <- n.samples / batch.length
n.burn <- 15000
n.thin <- 10
n.chains <- 1
accept.rate <- 0.43

# Run simulations ---------------------------------------------------------
for (j in 1:n.sims) {
  print(paste("Currently on simulation set ", j, " out of ", n.sims, sep = ''))
  set.seed(my.seeds[j])
  for (i in 1:n.scenarios) {
    print(paste("Currently on scenario ", i, " out of ", n.scenarios, sep = ''))
    sigma.sq.curr <- scenario.vals$sigma.sq[i]
    phi.curr <- scenario.vals$phi[i]
    rho.curr <- scenario.vals$rho[i]
    sigma.sq.t.curr <- scenario.vals$sigma.sq.t[i]
    phi.tune <- 0.75
    trend <- FALSE
    ar1 <- TRUE
    dat <- simTOcc(J.x = J.x, J.y = J.y, n.time = rep(n.time, J), 
		   beta = beta, alpha = alpha,
		   n.rep = n.rep, sp = sp, cov.model = cov.model,
		   trend = trend, sigma.sq = sigma.sq.curr, phi = phi.curr,
		   ar1 = ar1, rho = rho.curr, sigma.sq.t = sigma.sq.t.curr, 
                   scale.param = 0.5, mis.spec.type = 'scale')
    psi.true[, , j, i] <- dat$psi
    # Prep the data for spOccupancy -------------------------------------------
    # Site x Replicate
    y <- dat$y
    # Occurrence Covariates
    X <- dat$X
    # Detection Covariates
    X.p <- dat$X.p
    # Coordinates
    coords <- dat$coords
    # Package all data into a list
    occ.covs <- list(int = X[, , 1],
                     occ.cov.1 = X[, , 2])
    det.covs <- list(det.cov.1 = X.p[, , , 2])
    data.list <- list(y = y, occ.covs = occ.covs, det.covs = det.covs, coords = coords)
    # Priors
    # May want to explore sensitivity of results to these a bit more.  
    prior.list <- list(beta.normal = list(mean = 0, var = 2.72),
    		       alpha.normal = list(mean = 0, var = 2.72),
    		       sigma.sq.ig = c(a = 2, b = 1.5),
		       sigma.sq.t.ig = c(2, 1),
                       phi.unif = c(a = 3 / 1, b = 3 / 0.05))
    # Starting values
    z.init <- apply(y, c(1, 2), function(a) as.numeric(sum(a, na.rm = TRUE) > 0))
    inits.list <- list(beta = 0, alpha = 0, sigma.sq.t = 0.5, phi = 3 / .5, 
		       sigma.sq = 1, rho = 0, z = z.init)
    # Tuning
    tuning.list <- list(phi = phi.tune, rho = 0.5)
    # Fit the model with stPGOcc
    out <- stPGOcc(occ.formula = ~ occ.cov.1,
		     det.formula = ~ det.cov.1,
		     data = data.list,
		     n.batch = n.batch,
		     batch.length = batch.length,
		     inits = inits.list,
		     priors = prior.list,
		     accept.rate = 0.43,
		     cov.model = cov.model,
		     tuning = tuning.list,
		     n.omp.threads = 1, # TODO: change as necessary. 
		     verbose = TRUE,
		     ar1 = TRUE,
		     NNGP = TRUE,
		     n.neighbors = 5,
		     n.report = 25,
		     n.burn = n.burn,
		     n.thin = n.thin,
		     n.chains = 1)
    psi.mean.samples[, , j, i] <- apply(out$psi.samples, c(2, 3), mean)
    psi.low.samples[, , j, i] <- apply(out$psi.samples, c(2, 3), quantile, 0.025)
    psi.high.samples[, , j, i] <- apply(out$psi.samples, c(2, 3), quantile, 0.975)
    beta.mean.samples[, j, i] <- apply(out$beta.samples, 2, mean)
    beta.low.samples[, j, i] <- apply(out$beta.samples, 2, quantile, 0.025)
    beta.high.samples[, j, i] <- apply(out$beta.samples, 2, quantile, 0.975)
  } # i (n.scenarios)
} # j (n.sims)

save(psi.mean.samples, psi.low.samples, psi.high.samples,
     beta.mean.samples, beta.low.samples, beta.high.samples,
     psi.true, beta, scenario.vals, file = 'results/sim-sv-scale-low-results.rda')

