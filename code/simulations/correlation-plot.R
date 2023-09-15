# correlation-plot.R: this script generates figures that display how 
#                     the spatial autocorrelation and temporal autocorrelation
#                     parameter values used in the simulation study 
#                     effect the underlying random effects.
# Authors: Jeffrey W. Doser
rm(list = ls())
library(tidyverse)
library(spOccupancy)
library(ggpubr)

# Parameters for simulation -----------------------------------------------
# Number of data sets for each scenario
n.sims <- 100
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
plot.w.data <- list()
eta.vals <- matrix(NA, n.time, n.scenarios)
for (i in 1:n.scenarios) {
  set.seed(1340)
  print(paste("Currently on scenario ", i, " out of ", n.scenarios, sep = ''))
  sigma.sq.curr <- scenario.vals$sigma.sq[i]
  phi.curr <- scenario.vals$phi[i]
  rho.curr <- scenario.vals$rho[i]
  sigma.sq.t.curr <- scenario.vals$sigma.sq.t[i]
  phi.tune <- 0.5
  trend <- FALSE
  ar1 <- TRUE
  dat <- simTOcc(J.x = J.x, J.y = J.y, n.time = rep(n.time, J), 
      	   beta = beta, alpha = alpha,
      	   n.rep = n.rep, sp = sp, cov.model = cov.model,
      	   trend = trend, sigma.sq = sigma.sq.curr, phi = phi.curr,
      	   ar1 = ar1, rho = rho.curr, sigma.sq.t = sigma.sq.t.curr)
  plot.w.data[[i]] <- data.frame(x = dat$coords[, 1], 
  		               y = dat$coords[, 2], 
  		               w = dat$w)
  eta.vals[, i] <- dat$eta
}

# Plot of spatial autocorrelation -----------------------------------------
plot.data.df <- do.call(rbind, plot.w.data)
plot.data.df <- plot.data.df %>%
  mutate(phi = factor(rep(scenario.vals$phi, each = J), 
		      levels = c(15, 3.75), ordered = TRUE, 
		      labels = c(expression(paste(phi, " = 3 / 0.2")), 
				 expression(paste(phi, " = 3 / 0.8")))), 
	 sigma.sq = factor(rep(scenario.vals$sigma.sq, each = J), 
			   labels = c(expression(paste(sigma, " "^2, " = 0.3")), 
				      expression(paste(sigma, " "^2, " = 1.5")))))
lower.val <- min(sapply(plot.w.data, function(a) min(a$w)))
upper.val <- max(sapply(plot.w.data, function(a) max(a$w)))


space.plot <- ggplot(data = plot.data.df, aes(x = x, y = y, fill = w)) + 
    scale_fill_gradient2(midpoint = 0, low = '#B2182B', mid = 'white', high = '#2166AC', 
      	               na.value = NA, limits = c(lower.val, upper.val)) + 
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0)) + 
    geom_raster() + 
    facet_grid(phi ~ sigma.sq, 
	       labeller = label_parsed) + 
    theme_light(base_size = 22) + 
    labs(x = 'Easting', y = 'Northing', fill = 'w') +
    theme(text = element_text(family="LM Roman 10"),
	  axis.text.x = element_text(angle = 45, hjust = 1),
          strip.text.x = element_text(color = 'black'), 
          strip.text.y = element_text(color = 'black')) 
space.plot
ggsave(device = 'png', file = 'figures/Figure-1B.png', height = 6, width = 8, 
       units = 'in')

# Plot of temporal autocorrelation ----------------------------------------
plot.time.df <- data.frame(time = factor(1:n.time), 
			   eta = c(eta.vals), 
			   rho = factor(rep(scenario.vals$rho, each = n.time), 
					levels = c(0.5, 0.9), ordered = TRUE, 
					labels = c(expression(paste(rho, " = 0.5")), 
						   expression(paste(rho, " = 0.9")))), 
			   sigma.sq.t = factor(rep(scenario.vals$sigma.sq.t, each = n.time), 
					       levels = c(0.3, 1.5), ordered = TRUE, 
					       labels = c(expression(paste(sigma, " "[T]^2, " = 0.3")),
							  expression(paste(sigma, " "[T]^2, " = 1.5")))))

time.plot <- ggplot(data = plot.time.df, aes(x = time, y = eta)) + 
    geom_hline(yintercept = 0) +
    geom_point(pch = 21, col = 'black', size = 3, fill = 'black') + 
    facet_grid(rho ~ sigma.sq.t, 
	       labeller = label_parsed) + 
    theme_light(base_size = 22) + 
    labs(x = 'Time', y = expression(eta), fill = 'w') +
    theme(text = element_text(family="LM Roman 10"), 
          strip.text.x = element_text(color = 'black'), 
          strip.text.y = element_text(color = 'black'))
time.plot
ggsave(device = 'png', file = 'figures/Figure-1A.png', height = 6, width = 8, 
       units = 'in')

