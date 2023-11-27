# get-sim-data-line.R: script to generate the simulated data sets with 
#                      linear misspecification for Simulation Study 2.
# Author: Jeffrey W. Doser and Sara Stoudt
# Approximate run time: <5 min
library(spOccupancy)
i
# Generate the data -------------------------------------------------------
# Number of simulations for each scenario
n.sims <- 100
set.seed(10110)
my.seeds <- sample(1:100000, n.sims, replace = FALSE)
# Spatial locations (1200 total)
J.x <- 30
J.y <- 40
J <- J.x * J.y
# Number of years (keep constant for now, but will likely want to explore
#                  this as well).
n.time <- 10
# Five total replicates
n.rep <- matrix(5, J, n.time)
# Occurrence coefficient --------------
# Generate a single covariate
beta <- c(0.5, 0.1)
p.occ <- length(beta)
# Detection coefficient ---------------
# A single covariate on detection
alpha <- c(0.5, 0.1)
# Spatial parameters ------------------
sp <- TRUE
# Assume an exponential correlation model for now.
cov.model <- 'exponential'
# Spatial variances
sigma.sq.vals <- c(0.005, 0.02)
# Spatial decay
# NOTE: simTOcc generates data across a unit square. When using an exponential
#       correlation function, the effective spatial range (distance at which
#       correlation between sites drops to 0.05) is 3 / phi. Thus, the following
#       values correspond to effective spatial ranges of 20% and 80% of the
#       study region.
phi.vals <- c(3 / .2, 3 / .8)
# Temporal parameters -----------------
rho.vals <- c(0.5, 0.9)
sigma.sq.t.vals <- c(0.005, 0.02)
# Total number of simulation scenarios
n.scenarios <- length(sigma.sq.vals) * length(phi.vals) *
               length(rho.vals) * length(sigma.sq.t.vals)
# Different combinations of all the four parameters that vary
scenario.vals <- expand.grid(sigma.sq = sigma.sq.vals, phi = phi.vals,
			     rho = rho.vals, sigma.sq.t = sigma.sq.t.vals)
dat <- list()
for (j in 1:n.sims) {
  print(paste("Currently on simulation set ", j, " out of ", n.sims, sep = ''))
  set.seed(my.seeds[j])
  for (i in 1:n.scenarios) {
    print(paste("Currently on scenario ", i, " out of ", n.scenarios, sep = ''))
    sigma.sq.curr <- scenario.vals$sigma.sq[i]
    phi.curr <- scenario.vals$phi[i]
    rho.curr <- scenario.vals$rho[i]
    sigma.sq.t.curr <- scenario.vals$sigma.sq.t[i]
    phi.tune <- 0.5
    trend <- FALSE
    ar1 <- TRUE
    dat[[(j - 1) * n.scenarios + i]] <- simTOcc(J.x = J.x, J.y = J.y, n.time = rep(n.time, J), 
		                                beta = beta, alpha = alpha,
		                                n.rep = n.rep, sp = sp, cov.model = cov.model,
		                                trend = trend, sigma.sq = sigma.sq.curr, phi = phi.curr,
		                                ar1 = ar1, rho = rho.curr, sigma.sq.t = sigma.sq.t.curr,
		                                mis.spec.type = 'line')
  }
}
save(dat, file = 'data/sim-data-line.rda')

