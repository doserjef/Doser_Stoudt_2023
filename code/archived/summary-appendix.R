# summary.R: this script summarizes results from the different simulation
#            scenarios.
rm(list = ls())
library(spOccupancy)
library(tidyverse)

# Single-visit 
# Load in results --------------------------------------------------------
load("results/sim-sv-stPGOcc-results.rda")
# Simulation scenarios ----------------------------------------------------
sigma.sq.vals <- c(0.3, 1.5)
phi.vals <- c(3 / .2, 3 / .8)
rho.vals <- c(0.2, 0.8)
sigma.sq.t.vals <- c(0.3, 1.5)
n.scenarios <- length(sigma.sq.vals) * length(phi.vals) *
               length(rho.vals) * length(sigma.sq.t.vals)
scenario.vals <- expand.grid(sigma.sq = sigma.sq.vals, phi = phi.vals,
			     rho = rho.vals, sigma.sq.t = sigma.sq.t.vals)

plot(c(psi.true[, , 1, 1]), c(psi.mean.samples[, , 1, 1]), pch = 19)
abline(0, 1)

plot.df <- as.data.frame.table(psi.true)
colnames(plot.df) <- c('site', 'year', 'sim', 'scenario', 'val')
psi.mean.df <- as.data.frame.table(psi.mean.samples)
colnames(psi.mean.df) <- c('site', 'year', 'sim', 'scenario', 'val')
plot.df$est <- psi.mean.df$val
ggplot(data = plot.df, aes(x = val, y = est, col = sim)) + 
  geom_abline(slope = 1, intercept = 0) + 
  theme_bw(base_size = 18) + 
  facet_wrap(vars(scenario)) + 
  geom_smooth(aes(x = val, y = est)) + 
  guides(col = 'none')
ggsave(file = 'figures/sim-sv.png', width = 14, height = 9, units = 'in')

avg.df <- plot.df %>%
  group_by(scenario, sim) %>%
  arrange(val, .by_group = TRUE) %>%
  mutate(fake.id = 1:n()) %>%
  ungroup() %>%
  group_by(scenario, fake.id) %>%
  summarize(val.avg = mean(val),
	    est.avg = mean(est)) %>%
  ungroup()

ggplot(data = avg.df, aes(x = val.avg, y = est.avg)) +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw(base_size = 18) +
  facet_wrap(vars(scenario)) +
  geom_smooth(aes(x = val.avg, y = est.avg))
ggsave(file = 'figures/sim-sv-avg.png', width = 14, height = 9, units = 'in')

avg.df %>%
  group_by(scenario) %>%
  summarize(avg.bias = mean(abs(val.avg - est.avg))) %>%
  mutate(x = rep(1:4, time = 4), 
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = avg.bias)) + 
  geom_raster() + 
  geom_text(aes(label = scenario)) + 
  scale_fill_viridis_c() +
  theme_bw(base_size = 18)
ggsave(file = 'figures/sim-sv-heat-map-bias.png', width = 6, height = 6, units = 'in')

# Calculate coverage ------------------------------------------------------
J <- nrow(psi.high.samples)
n.time <- ncol(psi.high.samples)
n.sims <- dim(psi.high.samples)[3]
# msPGOcc
psi.covered.sv <- array(NA, dim = dim(psi.low.samples))
for (i in 1:n.sims) { # simulations
  print(i)
  for (k in 1:n.time) { # time
    for (j in 1:n.scenarios) { # scenarios
      psi.covered.sv[, k, i, j] <- ifelse((psi.true[, k, i, j] >
          				   psi.low.samples[, k, i, j]) &
          			          (psi.true[, k, i, j] <
          			           psi.high.samples[, k, i, j]),
          		                   1, 0)
    } # j
  } # k
} # i
psi.coverage.sv <- apply(psi.covered.sv, c(3, 4), mean)

plot.df.sv <- data.frame(coverage = c(psi.coverage.sv), 
			 sim.num = rep(1:n.sims, times = n.scenarios),
			 scenario = rep(LETTERS[1:n.scenarios], each = n.sims))
hist.coverage.sv <- ggplot(data = plot.df.sv, aes(x = coverage)) + 
  geom_histogram() + 
  geom_vline(xintercept = 0.95) + 
  facet_wrap(vars(scenario)) + 
  theme_bw(base_size = 18)
hist.coverage.sv
ggsave(file = 'figures/sim-sv-hist-cov.png', width = 14, height = 9, units = 'in')

heat.map.coverage.sv <- plot.df.sv %>%
  group_by(scenario) %>%
  summarize(coverage = mean(coverage)) %>%
  mutate(x = rep(1:4, time = 4), 
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = coverage)) + 
  geom_raster() + 
  geom_text(aes(label = scenario)) + 
  scale_fill_gradient2(midpoint = 0.95, low = '#B2182B', mid = 'white', high = '#2166AC', 
  	               na.value = NA) + 
  # geom_vline(xintercept = 0.95) + 
  theme_bw(base_size = 18)
heat.map.coverage.sv
ggsave(file = 'figures/sim-sv-heat-map-cov.png', width = 6, height = 6, units = 'in')

# Credible width ----------------------------------------------------------
psi.ci.width.sv.full <- array(NA, dim = dim(psi.low.samples))
for (i in 1:n.sims) { # simulations
  for (k in 1:n.time) { # time
    for (j in 1:n.scenarios) { # scenarios
      psi.ci.width.sv.full[, k, i, j] <- psi.high.samples[, k, i, j] - psi.low.samples[, k, i, j]
    } # j
  } # k
} # i

psi.ci.width.sv <- apply(psi.ci.width.sv.full, c(3, 4), mean)

plot.df.sv <- data.frame(ci.width = c(psi.ci.width.sv), 
			 sim.num = rep(1:n.sims, times = n.scenarios),
			 scenario = rep(LETTERS[1:n.scenarios], each = n.sims))
hist.ci.width.sv <- ggplot(data = plot.df.sv, aes(x = ci.width)) + 
  geom_histogram() + 
  facet_wrap(vars(scenario)) + 
  theme_bw(base_size = 18)
hist.ci.width.sv
ggsave(file = 'figures/sim-sv-hist-ci-width.png', width = 14, height = 9, units = 'in')

heat.map.ci.width.sv <- plot.df.sv %>%
  group_by(scenario) %>%
  summarize(ci.width = mean(ci.width)) %>%
  mutate(x = rep(1:4, time = 4), 
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = ci.width)) + 
  geom_raster() + 
  scale_fill_viridis_c() +
  theme_bw(base_size = 18)
heat.map.ci.width.sv
ggsave(file = 'figures/sim-sv-heat-map-ci-width.png', width = 6, height = 6, units = 'in')

# Single-visit, scaled high -----------------------------------------------
# Load in results --------------------------------------------------------
load("results/sim-sv-scale-high-results.rda")
# Simulation scenarios ----------------------------------------------------
sigma.sq.vals <- c(0.3, 1.5)
phi.vals <- c(3 / .2, 3 / .8)
rho.vals <- c(0.2, 0.8)
sigma.sq.t.vals <- c(0.3, 1.5)
n.scenarios <- length(sigma.sq.vals) * length(phi.vals) *
               length(rho.vals) * length(sigma.sq.t.vals)
scenario.vals <- expand.grid(sigma.sq = sigma.sq.vals, phi = phi.vals,
			     rho = rho.vals, sigma.sq.t = sigma.sq.t.vals)

plot(c(psi.true[, , 1, 1]), c(psi.mean.samples[, , 1, 1]), pch = 19)
abline(0, 1)

plot.df <- as.data.frame.table(psi.true)
colnames(plot.df) <- c('site', 'year', 'sim', 'scenario', 'val')
psi.mean.df <- as.data.frame.table(psi.mean.samples)
colnames(psi.mean.df) <- c('site', 'year', 'sim', 'scenario', 'val')
plot.df$est <- psi.mean.df$val
ggplot(data = plot.df, aes(x = val, y = est, col = sim)) + 
  geom_abline(slope = 1, intercept = 0) + 
  theme_bw(base_size = 18) + 
  facet_wrap(vars(scenario)) + 
  geom_smooth(aes(x = val, y = est)) + 
  guides(col = 'none')
ggsave(file = 'figures/sim-sv-scale-high.png', width = 14, height = 9, units = 'in')

avg.df <- plot.df %>%
  group_by(scenario, sim) %>%
  arrange(val, .by_group = TRUE) %>%
  mutate(fake.id = 1:n()) %>%
  ungroup() %>%
  group_by(scenario, fake.id) %>%
  summarize(val.avg = mean(val),
	    est.avg = mean(est)) %>%
  ungroup()

ggplot(data = avg.df, aes(x = val.avg, y = est.avg)) +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw(base_size = 18) +
  facet_wrap(vars(scenario)) +
  geom_smooth(aes(x = val.avg, y = est.avg))
ggsave(file = 'figures/sim-sv-scale-high-avg.png', width = 14, height = 9, units = 'in')

avg.df %>%
  group_by(scenario) %>%
  summarize(avg.bias = mean(abs(val.avg - est.avg))) %>%
  mutate(x = rep(1:4, time = 4), 
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = avg.bias)) + 
  geom_raster() + 
  geom_text(aes(label = scenario)) + 
  scale_fill_viridis_c() +
  theme_bw(base_size = 18)
ggsave(file = 'figures/sim-sv-scale-high-heat-map-bias.png', width = 6, height = 6, units = 'in')
# Calculate coverage ------------------------------------------------------
J <- nrow(psi.high.samples)
n.time <- ncol(psi.high.samples)
n.sims <- dim(psi.high.samples)[3]
# msPGOcc
psi.covered.sv.scale.high <- array(NA, dim = dim(psi.low.samples))
for (i in 1:n.sims) { # simulations
  print(i)
  for (k in 1:n.time) { # time
    for (j in 1:n.scenarios) { # scenarios
      psi.covered.sv.scale.high[, k, i, j] <- ifelse((psi.true[, k, i, j] >
          				   psi.low.samples[, k, i, j]) &
          			          (psi.true[, k, i, j] <
          			           psi.high.samples[, k, i, j]),
          		                   1, 0)
    } # j
  } # k
} # i
psi.coverage.sv.scale.high <- apply(psi.covered.sv.scale.high, c(3, 4), mean)

plot.df.sv.scale.high <- data.frame(coverage = c(psi.coverage.sv.scale.high), 
			 sim.num = rep(1:n.sims, times = n.scenarios),
			 scenario = rep(LETTERS[1:n.scenarios], each = n.sims))
hist.coverage.sv.scale.high <- ggplot(data = plot.df.sv.scale.high, aes(x = coverage)) + 
  geom_histogram() + 
  geom_vline(xintercept = 0.95) + 
  facet_wrap(vars(scenario)) + 
  theme_bw(base_size = 18)
hist.coverage.sv.scale.high
ggsave(file = 'figures/sim-sv-scale-high-hist-cov.png', width = 14, height = 9, units = 'in')

heat.map.coverage.sv.scale.high <- plot.df.sv.scale.high %>%
  group_by(scenario) %>%
  summarize(coverage = mean(coverage)) %>%
  mutate(x = rep(1:4, time = 4), 
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = coverage)) + 
  geom_raster() + 
  geom_text(aes(label = scenario)) + 
  scale_fill_gradient2(midpoint = 0.95, low = '#B2182B', mid = 'white', high = '#2166AC', 
  	               na.value = NA) + 
  # geom_vline(xintercept = 0.95) + 
  theme_bw(base_size = 18)
heat.map.coverage.sv.scale.high
ggsave(file = 'figures/sim-sv-scale-high-heat-map-cov.png', width = 6, height = 6, units = 'in')

# Credible width ----------------------------------------------------------
psi.ci.width.sv.scale.high.full <- array(NA, dim = dim(psi.low.samples))
for (i in 1:n.sims) { # simulations
  for (k in 1:n.time) { # time
    for (j in 1:n.scenarios) { # scenarios
      psi.ci.width.sv.scale.high.full[, k, i, j] <- psi.high.samples[, k, i, j] - psi.low.samples[, k, i, j]
    } # j
  } # k
} # i

psi.ci.width.sv.scale.high <- apply(psi.ci.width.sv.scale.high.full, c(3, 4), mean)

plot.df.sv.scale.high <- data.frame(ci.width = c(psi.ci.width.sv.scale.high), 
			 sim.num = rep(1:n.sims, times = n.scenarios),
			 scenario = rep(LETTERS[1:n.scenarios], each = n.sims))
hist.ci.width.sv.scale.high <- ggplot(data = plot.df.sv.scale.high, aes(x = ci.width)) + 
  geom_histogram() + 
  facet_wrap(vars(scenario)) + 
  theme_bw(base_size = 18)
hist.ci.width.sv.scale.high
ggsave(file = 'figures/sim-sv-scale-high-hist-ci-width.png', width = 14, height = 9, units = 'in')

heat.map.ci.width.sv.scale.high <- plot.df.sv.scale.high %>%
  group_by(scenario) %>%
  summarize(ci.width = mean(ci.width)) %>%
  mutate(x = rep(1:4, time = 4), 
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = ci.width)) + 
  geom_raster() + 
  scale_fill_viridis_c() +
  theme_bw(base_size = 18)
heat.map.ci.width.sv.scale.high
ggsave(file = 'figures/sim-sv-scale-high-heat-map-ci-width.png', width = 6, height = 6, units = 'in')

# SV scale low ------------------------------------------------------------
# Load in results --------------------------------------------------------
load("results/sim-sv-scale-low-results.rda")
# Simulation scenarios ----------------------------------------------------
sigma.sq.vals <- c(0.3, 1.5)
phi.vals <- c(3 / .2, 3 / .8)
rho.vals <- c(0.2, 0.8)
sigma.sq.t.vals <- c(0.3, 1.5)
n.scenarios <- length(sigma.sq.vals) * length(phi.vals) *
               length(rho.vals) * length(sigma.sq.t.vals)
scenario.vals <- expand.grid(sigma.sq = sigma.sq.vals, phi = phi.vals,
			     rho = rho.vals, sigma.sq.t = sigma.sq.t.vals)

plot(c(psi.true[, , 1, 1]), c(psi.mean.samples[, , 1, 1]), pch = 19)
abline(0, 1)

plot.df <- as.data.frame.table(psi.true)
colnames(plot.df) <- c('site', 'year', 'sim', 'scenario', 'val')
psi.mean.df <- as.data.frame.table(psi.mean.samples)
colnames(psi.mean.df) <- c('site', 'year', 'sim', 'scenario', 'val')
plot.df$est <- psi.mean.df$val
ggplot(data = plot.df, aes(x = val, y = est, col = sim)) + 
  geom_abline(slope = 1, intercept = 0) + 
  theme_bw(base_size = 18) + 
  facet_wrap(vars(scenario)) + 
  geom_smooth(aes(x = val, y = est)) + 
  guides(col = 'none')
ggsave(file = 'figures/sim-sv-scale-low.png', width = 14, height = 9, units = 'in')

avg.df <- plot.df %>%
  group_by(scenario, sim) %>%
  arrange(val, .by_group = TRUE) %>%
  mutate(fake.id = 1:n()) %>%
  ungroup() %>%
  group_by(scenario, fake.id) %>%
  summarize(val.avg = mean(val),
	    est.avg = mean(est)) %>%
  ungroup()

ggplot(data = avg.df, aes(x = val.avg, y = est.avg)) +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw(base_size = 18) +
  facet_wrap(vars(scenario)) +
  geom_smooth(aes(x = val.avg, y = est.avg))
ggsave(file = 'figures/sim-sv-scale-low-avg.png', width = 14, height = 9, units = 'in')

avg.df %>%
  group_by(scenario) %>%
  summarize(avg.bias = mean(abs(val.avg - est.avg))) %>%
  mutate(x = rep(1:4, time = 4), 
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = avg.bias)) + 
  geom_raster() + 
  geom_text(aes(label = scenario)) + 
  scale_fill_viridis_c() +
  theme_bw(base_size = 18)
ggsave(file = 'figures/sim-sv-scale-low-heat-map-bias.png', width = 6, height = 6, units = 'in')

# Calculate coverage ------------------------------------------------------
J <- nrow(psi.high.samples)
n.time <- ncol(psi.high.samples)
n.sims <- dim(psi.high.samples)[3]
# msPGOcc
psi.covered.sv.scale.low <- array(NA, dim = dim(psi.low.samples))
for (i in 1:n.sims) { # simulations
  print(i)
  for (k in 1:n.time) { # time
    for (j in 1:n.scenarios) { # scenarios
      psi.covered.sv.scale.low[, k, i, j] <- ifelse((psi.true[, k, i, j] >
          				   psi.low.samples[, k, i, j]) &
          			          (psi.true[, k, i, j] <
          			           psi.high.samples[, k, i, j]),
          		                   1, 0)
    } # j
  } # k
} # i
psi.coverage.sv.scale.low <- apply(psi.covered.sv.scale.low, c(3, 4), mean)

plot.df.sv.scale.low <- data.frame(coverage = c(psi.coverage.sv.scale.low), 
			 sim.num = rep(1:n.sims, times = n.scenarios),
			 scenario = rep(LETTERS[1:n.scenarios], each = n.sims))
hist.coverage.sv.scale.low <- ggplot(data = plot.df.sv.scale.low, aes(x = coverage)) + 
  geom_histogram() + 
  geom_vline(xintercept = 0.95) + 
  facet_wrap(vars(scenario)) + 
  theme_bw(base_size = 18)
hist.coverage.sv.scale.low
ggsave(file = 'figures/sim-sv-scale-low-hist-cov.png', width = 14, height = 9, units = 'in')

heat.map.coverage.sv.scale.low <- plot.df.sv.scale.low %>%
  group_by(scenario) %>%
  summarize(coverage = mean(coverage)) %>%
  mutate(x = rep(1:4, time = 4), 
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = coverage)) + 
  geom_raster() + 
  geom_text(aes(label = scenario)) + 
  scale_fill_gradient2(midpoint = 0.95, low = '#B2182B', mid = 'white', high = '#2166AC', 
  	               na.value = NA) + 
  # geom_vline(xintercept = 0.95) + 
  theme_bw(base_size = 18)
heat.map.coverage.sv.scale.low
ggsave(file = 'figures/sim-sv-scale-low-heat-map-cov.png', width = 6, height = 6, units = 'in')

# Credible width ----------------------------------------------------------
psi.ci.width.sv.scale.low.full <- array(NA, dim = dim(psi.low.samples))
for (i in 1:n.sims) { # simulations
  for (k in 1:n.time) { # time
    for (j in 1:n.scenarios) { # scenarios
      psi.ci.width.sv.scale.low.full[, k, i, j] <- psi.high.samples[, k, i, j] - psi.low.samples[, k, i, j]
    } # j
  } # k
} # i

psi.ci.width.sv.scale.low <- apply(psi.ci.width.sv.scale.low.full, c(3, 4), mean)

plot.df.sv.scale.low <- data.frame(ci.width = c(psi.ci.width.sv.scale.low), 
			 sim.num = rep(1:n.sims, times = n.scenarios),
			 scenario = rep(LETTERS[1:n.scenarios], each = n.sims))
hist.ci.width.sv.scale.low <- ggplot(data = plot.df.sv.scale.low, aes(x = ci.width)) + 
  geom_histogram() + 
  facet_wrap(vars(scenario)) + 
  theme_bw(base_size = 18)
hist.ci.width.sv.scale.low
ggsave(file = 'figures/sim-sv-scale-low-hist-ci-width.png', width = 14, height = 9, units = 'in')

heat.map.ci.width.sv.scale.low <- plot.df.sv.scale.low %>%
  group_by(scenario) %>%
  summarize(ci.width = mean(ci.width)) %>%
  mutate(x = rep(1:4, time = 4), 
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = ci.width)) + 
  geom_raster() + 
  scale_fill_viridis_c() +
  theme_bw(base_size = 18)
heat.map.ci.width.sv.scale.low
ggsave(file = 'figures/sim-sv-scale-low-heat-map-ci-width.png', width = 6, height = 6, units = 'in')

# Single-visit, line ------------------------------------------------------
# Load in results --------------------------------------------------------
load("results/sim-sv-line-results.rda")
# Simulation scenarios ----------------------------------------------------
sigma.sq.vals <- c(0.3, 1.5)
phi.vals <- c(3 / .2, 3 / .8)
rho.vals <- c(0.2, 0.8)
sigma.sq.t.vals <- c(0.3, 1.5)
n.scenarios <- length(sigma.sq.vals) * length(phi.vals) *
               length(rho.vals) * length(sigma.sq.t.vals)
scenario.vals <- expand.grid(sigma.sq = sigma.sq.vals, phi = phi.vals,
			     rho = rho.vals, sigma.sq.t = sigma.sq.t.vals)

plot(c(psi.true[, , 1, 1]), c(psi.mean.samples[, , 1, 1]), pch = 19)
abline(0, 1)

plot.df <- as.data.frame.table(psi.true)
colnames(plot.df) <- c('site', 'year', 'sim', 'scenario', 'val')
psi.mean.df <- as.data.frame.table(psi.mean.samples)
colnames(psi.mean.df) <- c('site', 'year', 'sim', 'scenario', 'val')
plot.df$est <- psi.mean.df$val
ggplot(data = plot.df, aes(x = val, y = est, col = sim)) + 
  geom_abline(slope = 1, intercept = 0) + 
  theme_bw(base_size = 18) + 
  facet_wrap(vars(scenario)) + 
  geom_smooth(aes(x = val, y = est)) + 
  guides(col = 'none')
ggsave(file = 'figures/sim-sv-line.png', width = 14, height = 9, units = 'in')

avg.df <- plot.df %>%
  group_by(scenario, sim) %>%
  arrange(val, .by_group = TRUE) %>%
  mutate(fake.id = 1:n()) %>%
  ungroup() %>%
  group_by(scenario, fake.id) %>%
  summarize(val.avg = mean(val),
	    est.avg = mean(est)) %>%
  ungroup()

ggplot(data = avg.df, aes(x = val.avg, y = est.avg)) +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw(base_size = 18) +
  facet_wrap(vars(scenario)) +
  geom_smooth(aes(x = val.avg, y = est.avg))
ggsave(file = 'figures/sim-sv-line-avg.png', width = 14, height = 9, units = 'in')

avg.df %>%
  group_by(scenario) %>%
  summarize(avg.bias = mean(abs(val.avg - est.avg))) %>%
  mutate(x = rep(1:4, time = 4),
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = avg.bias)) +
  geom_raster() +
  geom_text(aes(label = scenario)) +
  scale_fill_viridis_c() +
  theme_bw(base_size = 18)
ggsave(file = 'figures/sim-sv-line-heat-map-bias.png', width = 6, height = 6, units = 'in')

# Calculate coverage ------------------------------------------------------
J <- nrow(psi.high.samples)
n.time <- ncol(psi.high.samples)
n.sims <- dim(psi.high.samples)[3]
# msPGOcc
psi.covered.sv.line <- array(NA, dim = dim(psi.low.samples))
for (i in 1:n.sims) { # simulations
  print(i)
  for (k in 1:n.time) { # time
    for (j in 1:n.scenarios) { # scenarios
      psi.covered.sv.line[, k, i, j] <- ifelse((psi.true[, k, i, j] >
          				   psi.low.samples[, k, i, j]) &
          			          (psi.true[, k, i, j] <
          			           psi.high.samples[, k, i, j]),
          		                   1, 0)
    } # j
  } # k
} # i
psi.coverage.sv.line <- apply(psi.covered.sv.line, c(3, 4), mean)

plot.df.sv.line <- data.frame(coverage = c(psi.coverage.sv.line), 
			 sim.num = rep(1:n.sims, times = n.scenarios),
			 scenario = rep(LETTERS[1:n.scenarios], each = n.sims))
hist.coverage.sv.line <- ggplot(data = plot.df.sv.line, aes(x = coverage)) + 
  geom_histogram() + 
  geom_vline(xintercept = 0.95) + 
  facet_wrap(vars(scenario)) + 
  theme_bw(base_size = 18)
hist.coverage.sv.line
ggsave(file = 'figures/sim-sv-line-hist-cov.png', width = 14, height = 9, units = 'in')

heat.map.coverage.sv.line <- plot.df.sv.line %>%
  group_by(scenario) %>%
  summarize(coverage = mean(coverage)) %>%
  mutate(x = rep(1:4, time = 4), 
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = coverage)) + 
  geom_raster() + 
  geom_text(aes(label = scenario)) + 
  scale_fill_gradient2(midpoint = 0.95, low = '#B2182B', mid = 'white', high = '#2166AC', 
  	               na.value = NA) + 
  # geom_vline(xintercept = 0.95) + 
  theme_bw(base_size = 18)
heat.map.coverage.sv.line
ggsave(file = 'figures/sim-sv-line-heat-map-cov.png', width = 6, height = 6, units = 'in')

# Credible width ----------------------------------------------------------
psi.ci.width.sv.line.full <- array(NA, dim = dim(psi.low.samples))
for (i in 1:n.sims) { # simulations
  for (k in 1:n.time) { # time
    for (j in 1:n.scenarios) { # scenarios
      psi.ci.width.sv.line.full[, k, i, j] <- psi.high.samples[, k, i, j] - psi.low.samples[, k, i, j]
    } # j
  } # k
} # i

psi.ci.width.sv.line <- apply(psi.ci.width.sv.line.full, c(3, 4), mean)

plot.df.sv.line <- data.frame(ci.width = c(psi.ci.width.sv.line), 
			 sim.num = rep(1:n.sims, times = n.scenarios),
			 scenario = rep(LETTERS[1:n.scenarios], each = n.sims))
hist.ci.width.sv.line <- ggplot(data = plot.df.sv.line, aes(x = ci.width)) + 
  geom_histogram() + 
  facet_wrap(vars(scenario)) + 
  theme_bw(base_size = 18)
hist.ci.width.sv.line
ggsave(file = 'figures/sim-sv-line-hist-ci-width.png', width = 14, height = 9, units = 'in')

heat.map.ci.width.sv.line <- plot.df.sv.line %>%
  group_by(scenario) %>%
  summarize(ci.width = mean(ci.width)) %>%
  mutate(x = rep(1:4, time = 4), 
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = ci.width)) + 
  geom_raster() + 
  scale_fill_viridis_c() +
  theme_bw(base_size = 18)
heat.map.ci.width.sv.line
ggsave(file = 'figures/sim-sv-line-heat-map-ci-width.png', width = 6, height = 6, units = 'in')

# Single-visit, probit ------------------------------------------------------
# Load in results --------------------------------------------------------
load("results/sim-sv-probit-results.rda")
# Simulation scenarios ----------------------------------------------------
sigma.sq.vals <- c(0.3, 1.5)
phi.vals <- c(3 / .2, 3 / .8)
rho.vals <- c(0.2, 0.8)
sigma.sq.t.vals <- c(0.3, 1.5)
n.scenarios <- length(sigma.sq.vals) * length(phi.vals) *
               length(rho.vals) * length(sigma.sq.t.vals)
scenario.vals <- expand.grid(sigma.sq = sigma.sq.vals, phi = phi.vals,
			     rho = rho.vals, sigma.sq.t = sigma.sq.t.vals)

plot(c(psi.true[, , 1, 1]), c(psi.mean.samples[, , 1, 1]), pch = 19)
abline(0, 1)

plot.df <- as.data.frame.table(psi.true)
colnames(plot.df) <- c('site', 'year', 'sim', 'scenario', 'val')
psi.mean.df <- as.data.frame.table(psi.mean.samples)
colnames(psi.mean.df) <- c('site', 'year', 'sim', 'scenario', 'val')
plot.df$est <- psi.mean.df$val
ggplot(data = plot.df, aes(x = val, y = est, col = sim)) + 
  geom_abline(slope = 1, intercept = 0) + 
  theme_bw(base_size = 18) + 
  facet_wrap(vars(scenario)) + 
  geom_smooth(aes(x = val, y = est)) + 
  guides(col = 'none')
ggsave(file = 'figures/sim-sv-probit.png', width = 14, height = 9, units = 'in')

avg.df <- plot.df %>%
  group_by(scenario, sim) %>%
  arrange(val, .by_group = TRUE) %>%
  mutate(fake.id = 1:n()) %>%
  ungroup() %>%
  group_by(scenario, fake.id) %>%
  summarize(val.avg = mean(val),
	    est.avg = mean(est)) %>%
  ungroup()

ggplot(data = avg.df, aes(x = val.avg, y = est.avg)) +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw(base_size = 18) +
  facet_wrap(vars(scenario)) +
  geom_smooth(aes(x = val.avg, y = est.avg))
ggsave(file = 'figures/sim-sv-probit-avg.png', width = 14, height = 9, units = 'in')

avg.df %>%
  group_by(scenario) %>%
  summarize(avg.bias = mean(abs(val.avg - est.avg))) %>%
  mutate(x = rep(1:4, time = 4),
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = avg.bias)) +
  geom_raster() +
  geom_text(aes(label = scenario)) +
  scale_fill_viridis_c() +
  theme_bw(base_size = 18)
ggsave(file = 'figures/sim-sv-probit-heat-map-bias.png', width = 6, height = 6, units = 'in')

# Calculate coverage ------------------------------------------------------
J <- nrow(psi.high.samples)
n.time <- ncol(psi.high.samples)
n.sims <- dim(psi.high.samples)[3]
# msPGOcc
psi.covered.sv.probit <- array(NA, dim = dim(psi.low.samples))
for (i in 1:n.sims) { # simulations
  print(i)
  for (k in 1:n.time) { # time
    for (j in 1:n.scenarios) { # scenarios
      psi.covered.sv.probit[, k, i, j] <- ifelse((psi.true[, k, i, j] >
          				   psi.low.samples[, k, i, j]) &
          			          (psi.true[, k, i, j] <
          			           psi.high.samples[, k, i, j]),
          		                   1, 0)
    } # j
  } # k
} # i
psi.coverage.sv.probit <- apply(psi.covered.sv.probit, c(3, 4), mean)

plot.df.sv.probit <- data.frame(coverage = c(psi.coverage.sv.probit), 
			 sim.num = rep(1:n.sims, times = n.scenarios),
			 scenario = rep(LETTERS[1:n.scenarios], each = n.sims))
hist.coverage.sv.probit <- ggplot(data = plot.df.sv.probit, aes(x = coverage)) + 
  geom_histogram() + 
  geom_vline(xintercept = 0.95) + 
  facet_wrap(vars(scenario)) + 
  theme_bw(base_size = 18)
hist.coverage.sv.probit
ggsave(file = 'figures/sim-sv-probit-hist-cov.png', width = 14, height = 9, units = 'in')

heat.map.coverage.sv.probit <- plot.df.sv.probit %>%
  group_by(scenario) %>%
  summarize(coverage = mean(coverage)) %>%
  mutate(x = rep(1:4, time = 4), 
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = coverage)) + 
  geom_raster() + 
  geom_text(aes(label = scenario)) + 
  scale_fill_gradient2(midpoint = 0.95, low = '#B2182B', mid = 'white', high = '#2166AC', 
  	               na.value = NA) + 
  theme_bw(base_size = 18)
heat.map.coverage.sv.probit
ggsave(file = 'figures/sim-sv-probit-heat-map-cov.png', width = 6, height = 6, units = 'in')

# Credible width ----------------------------------------------------------
psi.ci.width.sv.probit.full <- array(NA, dim = dim(psi.low.samples))
for (i in 1:n.sims) { # simulations
  for (k in 1:n.time) { # time
    for (j in 1:n.scenarios) { # scenarios
      psi.ci.width.sv.probit.full[, k, i, j] <- psi.high.samples[, k, i, j] - psi.low.samples[, k, i, j]
    } # j
  } # k
} # i

psi.ci.width.sv.probit <- apply(psi.ci.width.sv.probit.full, c(3, 4), mean)

plot.df.sv.probit <- data.frame(ci.width = c(psi.ci.width.sv.probit), 
			 sim.num = rep(1:n.sims, times = n.scenarios),
			 scenario = rep(LETTERS[1:n.scenarios], each = n.sims))
hist.ci.width.sv.probit <- ggplot(data = plot.df.sv.probit, aes(x = ci.width)) + 
  geom_histogram() + 
  facet_wrap(vars(scenario)) + 
  theme_bw(base_size = 18)
hist.ci.width.sv.probit
ggsave(file = 'figures/sim-sv-probit-hist-ci-width.png', width = 14, height = 9, units = 'in')

heat.map.ci.width.sv.probit <- plot.df.sv.probit %>%
  group_by(scenario) %>%
  summarize(ci.width = mean(ci.width)) %>%
  mutate(x = rep(1:4, time = 4), 
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = ci.width)) + 
  geom_raster() + 
  scale_fill_viridis_c() +
  theme_bw(base_size = 18)
heat.map.ci.width.sv.probit
ggsave(file = 'figures/sim-sv-probit-heat-map-ci-width.png', width = 6, height = 6, units = 'in')

# Double-visit ------------------------------------------------------------
# Load in results --------------------------------------------------------
load("results/sim-dv-stPGOcc-results.rda")
# Simulation scenarios ----------------------------------------------------
sigma.sq.vals <- c(0.3, 1.5)
phi.vals <- c(3 / .2, 3 / .8)
rho.vals <- c(0.2, 0.8)
sigma.sq.t.vals <- c(0.3, 1.5)
n.scenarios <- length(sigma.sq.vals) * length(phi.vals) *
               length(rho.vals) * length(sigma.sq.t.vals)
scenario.vals <- expand.grid(sigma.sq = sigma.sq.vals, phi = phi.vals,
			     rho = rho.vals, sigma.sq.t = sigma.sq.t.vals)

plot(c(psi.true[, , 1, 1]), c(psi.mean.samples[, , 1, 1]), pch = 19)
abline(0, 1)

plot.df <- as.data.frame.table(psi.true)
colnames(plot.df) <- c('site', 'year', 'sim', 'scenario', 'val')
psi.mean.df <- as.data.frame.table(psi.mean.samples)
colnames(psi.mean.df) <- c('site', 'year', 'sim', 'scenario', 'val')
plot.df$est <- psi.mean.df$val
ggplot(data = plot.df, aes(x = val, y = est, col = sim)) + 
  geom_abline(slope = 1, intercept = 0) + 
  theme_bw(base_size = 18) + 
  facet_wrap(vars(scenario)) + 
  geom_smooth(aes(x = val, y = est)) + 
  guides(col = 'none')
ggsave(file = 'figures/sim-dv-stPGOcc.png', width = 14, height = 9, units = 'in')

avg.df <- plot.df %>%
  group_by(scenario, sim) %>%
  arrange(val, .by_group = TRUE) %>%
  mutate(fake.id = 1:n()) %>%
  ungroup() %>%
  group_by(scenario, fake.id) %>%
  summarize(val.avg = mean(val),
	    est.avg = mean(est)) %>%
  ungroup()

ggplot(data = avg.df, aes(x = val.avg, y = est.avg)) +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw(base_size = 18) +
  facet_wrap(vars(scenario)) +
  geom_smooth(aes(x = val.avg, y = est.avg))
ggsave(file = 'figures/sim-dv-avg.png', width = 14, height = 9, units = 'in')

avg.df %>%
  group_by(scenario) %>%
  summarize(avg.bias = mean(abs(val.avg - est.avg))) %>%
  mutate(x = rep(1:4, time = 4),
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = avg.bias)) +
  geom_raster() +
  geom_text(aes(label = scenario)) +
  scale_fill_viridis_c() +
  theme_bw(base_size = 18)
ggsave(file = 'figures/sim-dv-heat-map-bias.png', width = 6, height = 6, units = 'in')

# Calculate coverage ------------------------------------------------------
J <- nrow(psi.high.samples)
n.time <- ncol(psi.high.samples)
n.sims <- dim(psi.high.samples)[3]
# msPGOcc
psi.covered.dv <- array(NA, dim = dim(psi.low.samples))
for (i in 1:n.sims) { # simulations
  print(i)
  for (k in 1:n.time) { # time
    for (j in 1:n.scenarios) { # scenarios
      psi.covered.dv[, k, i, j] <- ifelse((psi.true[, k, i, j] >
          				   psi.low.samples[, k, i, j]) &
          			          (psi.true[, k, i, j] <
          			           psi.high.samples[, k, i, j]),
          		                   1, 0)
    } # j
  } # k
} # i
psi.coverage.dv <- apply(psi.covered.dv, c(3, 4), mean)

plot.df.dv <- data.frame(coverage = c(psi.coverage.dv), 
			 sim.num = rep(1:n.sims, times = n.scenarios),
			 scenario = rep(LETTERS[1:n.scenarios], each = n.sims))
hist.coverage.dv <- ggplot(data = plot.df.dv, aes(x = coverage)) + 
  geom_histogram() + 
  geom_vline(xintercept = 0.95) + 
  facet_wrap(vars(scenario)) + 
  theme_bw(base_size = 18)
hist.coverage.dv
ggsave(file = 'figures/sim-dv-hist-cov.png', width = 14, height = 9, units = 'in')

heat.map.coverage.dv <- plot.df.dv %>%
  group_by(scenario) %>%
  summarize(coverage = mean(coverage)) %>%
  mutate(x = rep(1:4, time = 4), 
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = coverage)) + 
  geom_raster() + 
  geom_text(aes(label = scenario)) + 
  scale_fill_gradient2(midpoint = 0.95, low = '#B2182B', mid = 'white', high = '#2166AC', 
  	               na.value = NA) + 
  # geom_vline(xintercept = 0.95) + 
  theme_bw(base_size = 18)
heat.map.coverage.dv
ggsave(file = 'figures/sim-dv-heat-map-cov.png', width = 6, height = 6, units = 'in')

# Credible width ----------------------------------------------------------
psi.ci.width.dv.full <- array(NA, dim = dim(psi.low.samples))
for (i in 1:n.sims) { # simulations
  for (k in 1:n.time) { # time
    for (j in 1:n.scenarios) { # scenarios
      psi.ci.width.dv.full[, k, i, j] <- psi.high.samples[, k, i, j] - psi.low.samples[, k, i, j]
    } # j
  } # k
} # i

psi.ci.width.dv <- apply(psi.ci.width.dv.full, c(3, 4), mean)

plot.df.dv <- data.frame(ci.width = c(psi.ci.width.dv), 
			 sim.num = rep(1:n.sims, times = n.scenarios),
			 scenario = rep(LETTERS[1:n.scenarios], each = n.sims))
hist.ci.width.dv <- ggplot(data = plot.df.dv, aes(x = ci.width)) + 
  geom_histogram() + 
  facet_wrap(vars(scenario)) + 
  theme_bw(base_size = 18)
hist.ci.width.dv
ggsave(file = 'figures/sim-dv-hist-ci-width.png', width = 14, height = 9, units = 'in')

heat.map.ci.width.dv <- plot.df.dv %>%
  group_by(scenario) %>%
  summarize(ci.width = mean(ci.width)) %>%
  mutate(x = rep(1:4, time = 4), 
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = ci.width)) + 
  geom_raster() + 
  scale_fill_viridis_c() +
  theme_bw(base_size = 18)
heat.map.ci.width.dv
ggsave(file = 'figures/sim-dv-heat-map-ci-width.png', width = 6, height = 6, units = 'in')

# Double visit, scale high ------------------------------------------------
# Load in results --------------------------------------------------------
load("results/sim-dv-scale-high-results.rda")
# Simulation scenarios ----------------------------------------------------
sigma.sq.vals <- c(0.3, 1.5)
phi.vals <- c(3 / .2, 3 / .8)
rho.vals <- c(0.2, 0.8)
sigma.sq.t.vals <- c(0.3, 1.5)
n.scenarios <- length(sigma.sq.vals) * length(phi.vals) *
               length(rho.vals) * length(sigma.sq.t.vals)
scenario.vals <- expand.grid(sigma.sq = sigma.sq.vals, phi = phi.vals,
			     rho = rho.vals, sigma.sq.t = sigma.sq.t.vals)

plot(c(psi.true[, , 1, 1]), c(psi.mean.samples[, , 1, 1]), pch = 19)
abline(0, 1)

plot.df <- as.data.frame.table(psi.true)
colnames(plot.df) <- c('site', 'year', 'sim', 'scenario', 'val')
psi.mean.df <- as.data.frame.table(psi.mean.samples)
colnames(psi.mean.df) <- c('site', 'year', 'sim', 'scenario', 'val')
plot.df$est <- psi.mean.df$val
ggplot(data = plot.df, aes(x = val, y = est, col = sim)) + 
  geom_abline(slope = 1, intercept = 0) + 
  theme_bw(base_size = 18) + 
  facet_wrap(vars(scenario)) + 
  geom_smooth(aes(x = val, y = est)) + 
  guides(col = 'none')
ggsave(file = 'figures/sim-dv-scale-high.png', width = 14, height = 9, units = 'in')

avg.df <- plot.df %>%
  group_by(scenario, sim) %>%
  arrange(val, .by_group = TRUE) %>%
  mutate(fake.id = 1:n()) %>%
  ungroup() %>%
  group_by(scenario, fake.id) %>%
  summarize(val.avg = mean(val),
	    est.avg = mean(est)) %>%
  ungroup()

ggplot(data = avg.df, aes(x = val.avg, y = est.avg)) +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw(base_size = 18) +
  facet_wrap(vars(scenario)) +
  geom_smooth(aes(x = val.avg, y = est.avg))
ggsave(file = 'figures/sim-dv-scale-high-avg.png', width = 14, height = 9, units = 'in')

avg.df %>%
  group_by(scenario) %>%
  summarize(avg.bias = mean(abs(val.avg - est.avg))) %>%
  mutate(x = rep(1:4, time = 4),
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = avg.bias)) +
  geom_raster() +
  geom_text(aes(label = scenario)) +
  scale_fill_viridis_c() +
  theme_bw(base_size = 18)
ggsave(file = 'figures/sim-dv-scale-high-heat-map-bias.png', width = 6, height = 6, units = 'in')

# Calculate coverage ------------------------------------------------------
J <- nrow(psi.high.samples)
n.time <- ncol(psi.high.samples)
n.sims <- dim(psi.high.samples)[3]
# msPGOcc
psi.covered.dv.scale.high <- array(NA, dim = dim(psi.low.samples))
for (i in 1:n.sims) { # simulations
  print(i)
  for (k in 1:n.time) { # time
    for (j in 1:n.scenarios) { # scenarios
      psi.covered.dv.scale.high[, k, i, j] <- ifelse((psi.true[, k, i, j] >
          				   psi.low.samples[, k, i, j]) &
          			          (psi.true[, k, i, j] <
          			           psi.high.samples[, k, i, j]),
          		                   1, 0)
    } # j
  } # k
} # i
psi.coverage.dv.scale.high <- apply(psi.covered.dv.scale.high, c(3, 4), mean)

plot.df.dv.scale.high <- data.frame(coverage = c(psi.coverage.dv.scale.high), 
			 sim.num = rep(1:n.sims, times = n.scenarios),
			 scenario = rep(LETTERS[1:n.scenarios], each = n.sims))
hist.coverage.dv.scale.high <- ggplot(data = plot.df.dv.scale.high, aes(x = coverage)) + 
  geom_histogram() + 
  geom_vline(xintercept = 0.95) + 
  facet_wrap(vars(scenario)) + 
  theme_bw(base_size = 18)
hist.coverage.dv.scale.high
ggsave(file = 'figures/sim-dv-scale-high-hist-cov.png', width = 14, height = 9, units = 'in')

heat.map.coverage.dv.scale.high <- plot.df.dv.scale.high %>%
  group_by(scenario) %>%
  summarize(coverage = mean(coverage)) %>%
  mutate(x = rep(1:4, time = 4), 
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = coverage)) + 
  geom_raster() + 
  geom_text(aes(label = scenario)) + 
  scale_fill_gradient2(midpoint = 0.95, low = '#B2182B', mid = 'white', high = '#2166AC', 
  	               na.value = NA) + 
  # geom_vline(xintercept = 0.95) + 
  theme_bw(base_size = 18)
heat.map.coverage.dv.scale.high
ggsave(file = 'figures/sim-dv-scale-high-heat-map-cov.png', width = 6, height = 6, units = 'in')

# Credible width ----------------------------------------------------------
psi.ci.width.dv.scale.high.full <- array(NA, dim = dim(psi.low.samples))
for (i in 1:n.sims) { # simulations
  for (k in 1:n.time) { # time
    for (j in 1:n.scenarios) { # scenarios
      psi.ci.width.dv.scale.high.full[, k, i, j] <- psi.high.samples[, k, i, j] - psi.low.samples[, k, i, j]
    } # j
  } # k
} # i

psi.ci.width.dv.scale.high <- apply(psi.ci.width.dv.scale.high.full, c(3, 4), mean)

plot.df.dv.scale.high <- data.frame(ci.width = c(psi.ci.width.dv.scale.high), 
			 sim.num = rep(1:n.sims, times = n.scenarios),
			 scenario = rep(LETTERS[1:n.scenarios], each = n.sims))
hist.ci.width.dv.scale.high <- ggplot(data = plot.df.dv.scale.high, aes(x = ci.width)) + 
  geom_histogram() + 
  facet_wrap(vars(scenario)) + 
  theme_bw(base_size = 18)
hist.ci.width.dv.scale.high
ggsave(file = 'figures/sim-dv-scale-high-hist-ci-width.png', width = 14, height = 9, units = 'in')

heat.map.ci.width.dv.scale.high <- plot.df.dv.scale.high %>%
  group_by(scenario) %>%
  summarize(ci.width = mean(ci.width)) %>%
  mutate(x = rep(1:4, time = 4), 
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = ci.width)) + 
  geom_raster() + 
  scale_fill_viridis_c() +
  theme_bw(base_size = 18)
heat.map.ci.width.dv.scale.high
ggsave(file = 'figures/sim-dv-scale-high-heat-map-ci-width.png', width = 6, height = 6, units = 'in')

# Double visit, scale low -------------------------------------------------
# Load in results --------------------------------------------------------
load("results/sim-dv-scale-low-results.rda")
# Simulation scenarios ----------------------------------------------------
sigma.sq.vals <- c(0.3, 1.5)
phi.vals <- c(3 / .2, 3 / .8)
rho.vals <- c(0.2, 0.8)
sigma.sq.t.vals <- c(0.3, 1.5)
n.scenarios <- length(sigma.sq.vals) * length(phi.vals) *
               length(rho.vals) * length(sigma.sq.t.vals)
scenario.vals <- expand.grid(sigma.sq = sigma.sq.vals, phi = phi.vals,
			     rho = rho.vals, sigma.sq.t = sigma.sq.t.vals)

plot(c(psi.true[, , 1, 14]), c(psi.mean.samples[, , 1, 14]), pch = 19)
abline(0, 1)

plot.df <- as.data.frame.table(psi.true)
colnames(plot.df) <- c('site', 'year', 'sim', 'scenario', 'val')
psi.mean.df <- as.data.frame.table(psi.mean.samples)
colnames(psi.mean.df) <- c('site', 'year', 'sim', 'scenario', 'val')
plot.df$est <- psi.mean.df$val
ggplot(data = plot.df, aes(x = val, y = est, col = sim)) + 
  geom_abline(slope = 1, intercept = 0) + 
  theme_bw(base_size = 18) + 
  facet_wrap(vars(scenario)) + 
  geom_smooth(aes(x = val, y = est)) + 
  guides(col = 'none')
ggsave(file = 'figures/sim-dv-scale-low.png', width = 14, height = 9, units = 'in')

avg.df <- plot.df %>%
  group_by(scenario, sim) %>%
  arrange(val, .by_group = TRUE) %>%
  mutate(fake.id = 1:n()) %>%
  ungroup() %>%
  group_by(scenario, fake.id) %>%
  summarize(val.avg = mean(val),
	    est.avg = mean(est)) %>%
  ungroup()

ggplot(data = avg.df, aes(x = val.avg, y = est.avg)) +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw(base_size = 18) +
  facet_wrap(vars(scenario)) +
  geom_smooth(aes(x = val.avg, y = est.avg))
ggsave(file = 'figures/sim-dv-scale-low-avg.png', width = 14, height = 9, units = 'in')

avg.df %>%
  group_by(scenario) %>%
  summarize(avg.bias = mean(abs(val.avg - est.avg))) %>%
  mutate(x = rep(1:4, time = 4),
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = avg.bias)) +
  geom_raster() +
  geom_text(aes(label = scenario)) +
  scale_fill_viridis_c() +
  theme_bw(base_size = 18)
ggsave(file = 'figures/sim-dv-scale-low-heat-map-bias.png', width = 6, height = 6, units = 'in')

# Calculate coverage ------------------------------------------------------
J <- nrow(psi.high.samples)
n.time <- ncol(psi.high.samples)
n.sims <- dim(psi.high.samples)[3]
# msPGOcc
psi.covered.dv.scale.low <- array(NA, dim = dim(psi.low.samples))
for (i in 1:n.sims) { # simulations
  print(i)
  for (k in 1:n.time) { # time
    for (j in 1:n.scenarios) { # scenarios
      psi.covered.dv.scale.low[, k, i, j] <- ifelse((psi.true[, k, i, j] >
          				   psi.low.samples[, k, i, j]) &
          			          (psi.true[, k, i, j] <
          			           psi.high.samples[, k, i, j]),
          		                   1, 0)
    } # j
  } # k
} # i
psi.coverage.dv.scale.low <- apply(psi.covered.dv.scale.low, c(3, 4), mean)

plot.df.dv.scale.low <- data.frame(coverage = c(psi.coverage.dv.scale.low), 
			 sim.num = rep(1:n.sims, times = n.scenarios),
			 scenario = rep(LETTERS[1:n.scenarios], each = n.sims))
hist.coverage.dv.scale.low <- ggplot(data = plot.df.dv.scale.low, aes(x = coverage)) + 
  geom_histogram() + 
  geom_vline(xintercept = 0.95) + 
  facet_wrap(vars(scenario)) + 
  theme_bw(base_size = 18)
hist.coverage.dv.scale.low
ggsave(file = 'figures/sim-dv-scale-low-hist-cov.png', width = 14, height = 9, units = 'in')

heat.map.coverage.dv.scale.low <- plot.df.dv.scale.low %>%
  group_by(scenario) %>%
  summarize(coverage = mean(coverage)) %>%
  mutate(x = rep(1:4, time = 4), 
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = coverage)) + 
  geom_raster() + 
  geom_text(aes(label = scenario)) + 
  scale_fill_gradient2(midpoint = 0.95, low = '#B2182B', mid = 'white', high = '#2166AC', 
  	               na.value = NA) + 
  # geom_vline(xintercept = 0.95) + 
  theme_bw(base_size = 18)
heat.map.coverage.dv.scale.low
ggsave(file = 'figures/sim-dv-scale-low-heat-map-cov.png', width = 6, height = 6, units = 'in')

# Credible width ----------------------------------------------------------
psi.ci.width.dv.scale.low.full <- array(NA, dim = dim(psi.low.samples))
for (i in 1:n.sims) { # simulations
  for (k in 1:n.time) { # time
    for (j in 1:n.scenarios) { # scenarios
      psi.ci.width.dv.scale.low.full[, k, i, j] <- psi.high.samples[, k, i, j] - psi.low.samples[, k, i, j]
    } # j
  } # k
} # i

psi.ci.width.dv.scale.low <- apply(psi.ci.width.dv.scale.low.full, c(3, 4), mean)

plot.df.dv.scale.low <- data.frame(ci.width = c(psi.ci.width.dv.scale.low), 
			 sim.num = rep(1:n.sims, times = n.scenarios),
			 scenario = rep(LETTERS[1:n.scenarios], each = n.sims))
hist.ci.width.dv.scale.low <- ggplot(data = plot.df.dv.scale.low, aes(x = ci.width)) + 
  geom_histogram() + 
  facet_wrap(vars(scenario)) + 
  theme_bw(base_size = 18)
hist.ci.width.dv.scale.low
ggsave(file = 'figures/sim-dv-scale-low-hist-ci-width.png', width = 14, height = 9, units = 'in')

heat.map.ci.width.dv.scale.low <- plot.df.dv.scale.low %>%
  group_by(scenario) %>%
  summarize(ci.width = mean(ci.width)) %>%
  mutate(x = rep(1:4, time = 4), 
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = ci.width)) + 
  geom_raster() + 
  scale_fill_viridis_c() +
  theme_bw(base_size = 18)
heat.map.ci.width.dv.scale.low
ggsave(file = 'figures/sim-dv-scale-low-heat-map-ci-width.png', width = 6, height = 6, units = 'in')

# Double visit, line ------------------------------------------------------
# Load in results --------------------------------------------------------
load("results/sim-dv-line-results.rda")
# Simulation scenarios ----------------------------------------------------
sigma.sq.vals <- c(0.3, 1.5)
phi.vals <- c(3 / .2, 3 / .8)
rho.vals <- c(0.2, 0.8)
sigma.sq.t.vals <- c(0.3, 1.5)
n.scenarios <- length(sigma.sq.vals) * length(phi.vals) *
               length(rho.vals) * length(sigma.sq.t.vals)
scenario.vals <- expand.grid(sigma.sq = sigma.sq.vals, phi = phi.vals,
			     rho = rho.vals, sigma.sq.t = sigma.sq.t.vals)

plot(c(psi.true[, , 1, 1]), c(psi.mean.samples[, , 1, 1]), pch = 19)
abline(0, 1)

plot.df <- as.data.frame.table(psi.true)
colnames(plot.df) <- c('site', 'year', 'sim', 'scenario', 'val')
psi.mean.df <- as.data.frame.table(psi.mean.samples)
colnames(psi.mean.df) <- c('site', 'year', 'sim', 'scenario', 'val')
plot.df$est <- psi.mean.df$val
ggplot(data = plot.df, aes(x = val, y = est, col = sim)) + 
  geom_abline(slope = 1, intercept = 0) + 
  theme_bw(base_size = 18) + 
  facet_wrap(vars(scenario)) + 
  geom_smooth(aes(x = val, y = est)) + 
  guides(col = 'none')
ggsave(file = 'figures/sim-dv-line.png', width = 14, height = 9, units = 'in')

avg.df <- plot.df %>%
  group_by(scenario, sim) %>%
  arrange(val, .by_group = TRUE) %>%
  mutate(fake.id = 1:n()) %>%
  ungroup() %>%
  group_by(scenario, fake.id) %>%
  summarize(val.avg = mean(val),
	    est.avg = mean(est)) %>%
  ungroup()

ggplot(data = avg.df, aes(x = val.avg, y = est.avg)) +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw(base_size = 18) +
  facet_wrap(vars(scenario)) +
  geom_smooth(aes(x = val.avg, y = est.avg))
ggsave(file = 'figures/sim-dv-line-avg.png', width = 14, height = 9, units = 'in')

avg.df %>%
  group_by(scenario) %>%
  summarize(avg.bias = mean(abs(val.avg - est.avg))) %>%
  mutate(x = rep(1:4, time = 4),
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = avg.bias)) +
  geom_raster() +
  geom_text(aes(label = scenario)) +
  scale_fill_viridis_c() +
  theme_bw(base_size = 18)
ggsave(file = 'figures/sim-dv-line-heat-map-bias.png', width = 6, height = 6, units = 'in')

# Calculate coverage ------------------------------------------------------
J <- nrow(psi.high.samples)
n.time <- ncol(psi.high.samples)
n.sims <- dim(psi.high.samples)[3]
# msPGOcc
psi.covered.dv.line <- array(NA, dim = dim(psi.low.samples))
for (i in 1:n.sims) { # simulations
  print(i)
  for (k in 1:n.time) { # time
    for (j in 1:n.scenarios) { # scenarios
      psi.covered.dv.line[, k, i, j] <- ifelse((psi.true[, k, i, j] >
          				   psi.low.samples[, k, i, j]) &
          			          (psi.true[, k, i, j] <
          			           psi.high.samples[, k, i, j]),
          		                   1, 0)
    } # j
  } # k
} # i
psi.coverage.dv.line <- apply(psi.covered.dv.line, c(3, 4), mean)

plot.df.dv.line <- data.frame(coverage = c(psi.coverage.dv.line),
			 sim.num = rep(1:n.sims, times = n.scenarios),
			 scenario = rep(LETTERS[1:n.scenarios], each = n.sims))
hist.coverage.dv.line <- ggplot(data = plot.df.dv.line, aes(x = coverage)) +
  geom_histogram() +
  geom_vline(xintercept = 0.95) +
  facet_wrap(vars(scenario)) +
  theme_bw(base_size = 18)
hist.coverage.dv.line
ggsave(file = 'figures/sim-dv-line-hist-cov.png', width = 14, height = 9, units = 'in')

heat.map.coverage.dv.line <- plot.df.dv.line %>%
  group_by(scenario) %>%
  summarize(coverage = mean(coverage)) %>%
  mutate(x = rep(1:4, time = 4), 
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = coverage)) +
  geom_raster() +
  geom_text(aes(label = scenario)) + 
  scale_fill_gradient2(midpoint = 0.95, low = '#B2182B', mid = 'white', high = '#2166AC',
  	               na.value = NA) +
  # geom_vline(xintercept = 0.95) +
  theme_bw(base_size = 18)
heat.map.coverage.dv.line
ggsave(file = 'figures/sim-dv-line-heat-map-cov.png', width = 6, height = 6, units = 'in')

# Credible width ----------------------------------------------------------
psi.ci.width.dv.line.full <- array(NA, dim = dim(psi.low.samples))
for (i in 1:n.sims) { # simulations
  for (k in 1:n.time) { # time
    for (j in 1:n.scenarios) { # scenarios
      psi.ci.width.dv.line.full[, k, i, j] <- psi.high.samples[, k, i, j] - psi.low.samples[, k, i, j]
    } # j
  } # k
} # i

psi.ci.width.dv.line <- apply(psi.ci.width.dv.line.full, c(3, 4), mean)

plot.df.dv.line <- data.frame(ci.width = c(psi.ci.width.dv.line),
			 sim.num = rep(1:n.sims, times = n.scenarios),
			 scenario = rep(LETTERS[1:n.scenarios], each = n.sims))
hist.ci.width.dv.line <- ggplot(data = plot.df.dv.line, aes(x = ci.width)) +
  geom_histogram() +
  facet_wrap(vars(scenario)) +
  theme_bw(base_size = 18)
hist.ci.width.dv.line
ggsave(file = 'figures/sim-dv-line-hist-ci-width.png', width = 14, height = 9, units = 'in')

heat.map.ci.width.dv.line <- plot.df.dv.line %>%
  group_by(scenario) %>%
  summarize(ci.width = mean(ci.width)) %>%
  mutate(x = rep(1:4, time = 4), 
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = ci.width)) +
  geom_raster() +
  scale_fill_viridis_c() +
  theme_bw(base_size = 18)
heat.map.ci.width.dv.line
ggsave(file = 'figures/sim-dv-line-heat-map-ci-width.png', width = 6, height = 6, units = 'in')

# Double visit, probit ------------------------------------------------------
# Load in results --------------------------------------------------------
load("results/sim-dv-probit-results.rda")
# Simulation scenarios ----------------------------------------------------
sigma.sq.vals <- c(0.3, 1.5)
phi.vals <- c(3 / .2, 3 / .8)
rho.vals <- c(0.2, 0.8)
sigma.sq.t.vals <- c(0.3, 1.5)
n.scenarios <- length(sigma.sq.vals) * length(phi.vals) *
               length(rho.vals) * length(sigma.sq.t.vals)
scenario.vals <- expand.grid(sigma.sq = sigma.sq.vals, phi = phi.vals,
			     rho = rho.vals, sigma.sq.t = sigma.sq.t.vals)

plot(c(psi.true[, , 1, 1]), c(psi.mean.samples[, , 1, 1]), pch = 19)
abline(0, 1)

plot.df <- as.data.frame.table(psi.true)
colnames(plot.df) <- c('site', 'year', 'sim', 'scenario', 'val')
psi.mean.df <- as.data.frame.table(psi.mean.samples)
colnames(psi.mean.df) <- c('site', 'year', 'sim', 'scenario', 'val')
plot.df$est <- psi.mean.df$val
ggplot(data = plot.df, aes(x = val, y = est, col = sim)) + 
  geom_abline(slope = 1, intercept = 0) + 
  theme_bw(base_size = 18) + 
  facet_wrap(vars(scenario)) + 
  geom_smooth(aes(x = val, y = est)) + 
  guides(col = 'none')
ggsave(file = 'figures/sim-dv-probit.png', width = 14, height = 9, units = 'in')

avg.df <- plot.df %>%
  group_by(scenario, sim) %>%
  arrange(val, .by_group = TRUE) %>%
  mutate(fake.id = 1:n()) %>%
  ungroup() %>%
  group_by(scenario, fake.id) %>%
  summarize(val.avg = mean(val),
	    est.avg = mean(est)) %>%
  ungroup()

ggplot(data = avg.df, aes(x = val.avg, y = est.avg)) +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw(base_size = 18) +
  facet_wrap(vars(scenario)) +
  geom_smooth(aes(x = val.avg, y = est.avg))
ggsave(file = 'figures/sim-dv-probit-avg.png', width = 14, height = 9, units = 'in')

avg.df %>%
  group_by(scenario) %>%
  summarize(avg.bias = mean(abs(val.avg - est.avg))) %>%
  mutate(x = rep(1:4, time = 4),
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = avg.bias)) +
  geom_raster() +
  geom_text(aes(label = scenario)) +
  scale_fill_viridis_c() +
  theme_bw(base_size = 18)
ggsave(file = 'figures/sim-dv-probit-heat-map-bias.png', width = 6, height = 6, units = 'in')

# Calculate coverage ------------------------------------------------------
J <- nrow(psi.high.samples)
n.time <- ncol(psi.high.samples)
n.sims <- dim(psi.high.samples)[3]
# msPGOcc
psi.covered.dv.probit <- array(NA, dim = dim(psi.low.samples))
for (i in 1:n.sims) { # simulations
  print(i)
  for (k in 1:n.time) { # time
    for (j in 1:n.scenarios) { # scenarios
      psi.covered.dv.probit[, k, i, j] <- ifelse((psi.true[, k, i, j] >
          				   psi.low.samples[, k, i, j]) &
          			          (psi.true[, k, i, j] <
          			           psi.high.samples[, k, i, j]),
          		                   1, 0)
    } # j
  } # k
} # i
psi.coverage.dv.probit <- apply(psi.covered.dv.probit, c(3, 4), mean)

plot.df.dv.probit <- data.frame(coverage = c(psi.coverage.dv.probit),
			 sim.num = rep(1:n.sims, times = n.scenarios),
			 scenario = rep(LETTERS[1:n.scenarios], each = n.sims))
hist.coverage.dv.probit <- ggplot(data = plot.df.dv.probit, aes(x = coverage)) +
  geom_histogram() +
  geom_vline(xintercept = 0.95) +
  facet_wrap(vars(scenario)) +
  theme_bw(base_size = 18)
hist.coverage.dv.probit
ggsave(file = 'figures/sim-dv-probit-hist-cov.png', width = 14, height = 9, units = 'in')

heat.map.coverage.dv.probit <- plot.df.dv.probit %>%
  group_by(scenario) %>%
  summarize(coverage = mean(coverage)) %>%
  mutate(x = rep(1:4, time = 4), 
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = coverage)) +
  geom_raster() +
  geom_text(aes(label = scenario)) + 
  scale_fill_gradient2(midpoint = 0.95, low = '#B2182B', mid = 'white', high = '#2166AC',
  	               na.value = NA) +
  # geom_vprobit(xintercept = 0.95) +
  theme_bw(base_size = 18)
heat.map.coverage.dv.probit
ggsave(file = 'figures/sim-dv-probit-heat-map-cov.png', width = 6, height = 6, units = 'in')

# Credible width ----------------------------------------------------------
psi.ci.width.dv.probit.full <- array(NA, dim = dim(psi.low.samples))
for (i in 1:n.sims) { # simulations
  for (k in 1:n.time) { # time
    for (j in 1:n.scenarios) { # scenarios
      psi.ci.width.dv.probit.full[, k, i, j] <- psi.high.samples[, k, i, j] - psi.low.samples[, k, i, j]
    } # j
  } # k
} # i

psi.ci.width.dv.probit <- apply(psi.ci.width.dv.probit.full, c(3, 4), mean)

plot.df.dv.probit <- data.frame(ci.width = c(psi.ci.width.dv.probit),
			 sim.num = rep(1:n.sims, times = n.scenarios),
			 scenario = rep(LETTERS[1:n.scenarios], each = n.sims))
hist.ci.width.dv.probit <- ggplot(data = plot.df.dv.probit, aes(x = ci.width)) +
  geom_histogram() +
  facet_wrap(vars(scenario)) +
  theme_bw(base_size = 18)
hist.ci.width.dv.probit
ggsave(file = 'figures/sim-dv-probit-hist-ci-width.png', width = 14, height = 9, units = 'in')

heat.map.ci.width.dv.probit <- plot.df.dv.probit %>%
  group_by(scenario) %>%
  summarize(ci.width = mean(ci.width)) %>%
  mutate(x = rep(1:4, time = 4), 
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = ci.width)) +
  geom_raster() +
  scale_fill_viridis_c() +
  theme_bw(base_size = 18)
heat.map.ci.width.dv.probit
ggsave(file = 'figures/sim-dv-probit-heat-map-ci-width.png', width = 6, height = 6, units = 'in')


# Five-visit ------------------------------------------------------------
# Load in results --------------------------------------------------------
load("results/sim-five-visits-stPGOcc-results.rda")
# Simulation scenarios ----------------------------------------------------
sigma.sq.vals <- c(0.3, 1.5)
phi.vals <- c(3 / .2, 3 / .8)
rho.vals <- c(0.2, 0.8)
sigma.sq.t.vals <- c(0.3, 1.5)
n.scenarios <- length(sigma.sq.vals) * length(phi.vals) *
               length(rho.vals) * length(sigma.sq.t.vals)
scenario.vals <- expand.grid(sigma.sq = sigma.sq.vals, phi = phi.vals,
			     rho = rho.vals, sigma.sq.t = sigma.sq.t.vals)

plot(c(psi.true[, , 1, 1]), c(psi.mean.samples[, , 1, 1]), pch = 19)
abline(0, 1)

plot.df <- as.data.frame.table(psi.true)
colnames(plot.df) <- c('site', 'year', 'sim', 'scenario', 'val')
psi.mean.df <- as.data.frame.table(psi.mean.samples)
colnames(psi.mean.df) <- c('site', 'year', 'sim', 'scenario', 'val')
plot.df$est <- psi.mean.df$val
ggplot(data = plot.df, aes(x = val, y = est, col = sim)) + 
  geom_abline(slope = 1, intercept = 0) + 
  theme_bw(base_size = 18) + 
  facet_wrap(vars(scenario)) + 
  geom_smooth(aes(x = val, y = est)) + 
  guides(col = 'none')
ggsave(file = 'figures/sim-five-visits-stPGOcc.png', width = 14, height = 9, units = 'in')

avg.df <- plot.df %>%
  group_by(scenario, sim) %>%
  arrange(val, .by_group = TRUE) %>%
  mutate(fake.id = 1:n()) %>%
  ungroup() %>%
  group_by(scenario, fake.id) %>%
  summarize(val.avg = mean(val),
	    est.avg = mean(est)) %>%
  ungroup()

ggplot(data = avg.df, aes(x = val.avg, y = est.avg)) +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw(base_size = 18) +
  facet_wrap(vars(scenario)) +
  geom_smooth(aes(x = val.avg, y = est.avg))
ggsave(file = 'figures/sim-five-visits-avg.png', width = 14, height = 9, units = 'in')

avg.df %>%
  group_by(scenario) %>%
  summarize(avg.bias = mean(abs(val.avg - est.avg))) %>%
  mutate(x = rep(1:4, time = 4),
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = avg.bias)) +
  geom_raster() +
  geom_text(aes(label = scenario)) +
  scale_fill_viridis_c() +
  theme_bw(base_size = 18)
ggsave(file = 'figures/sim-five-visits-heat-map-bias.png', width = 6, height = 6, units = 'in')

# Calculate coverage ------------------------------------------------------
J <- nrow(psi.high.samples)
n.time <- ncol(psi.high.samples)
n.sims <- dim(psi.high.samples)[3]
# msPGOcc
psi.covered.five.visits <- array(NA, dim = dim(psi.low.samples))
for (i in 1:n.sims) { # simulations
  print(i)
  for (k in 1:n.time) { # time
    for (j in 1:n.scenarios) { # scenarios
      psi.covered.five.visits[, k, i, j] <- ifelse((psi.true[, k, i, j] >
          				   psi.low.samples[, k, i, j]) &
          			          (psi.true[, k, i, j] <
          			           psi.high.samples[, k, i, j]),
          		                   1, 0)
    } # j
  } # k
} # i
psi.coverage.five.visits <- apply(psi.covered.five.visits, c(3, 4), mean)

plot.df.five.visits <- data.frame(coverage = c(psi.coverage.five.visits), 
			 sim.num = rep(1:n.sims, times = n.scenarios),
			 scenario = rep(LETTERS[1:n.scenarios], each = n.sims))
hist.coverage.five.visits <- ggplot(data = plot.df.five.visits, aes(x = coverage)) + 
  geom_histogram() + 
  geom_vline(xintercept = 0.95) + 
  facet_wrap(vars(scenario)) + 
  theme_bw(base_size = 18)
hist.coverage.five.visits
ggsave(file = 'figures/sim-five-visits-hist-cov.png', width = 14, height = 9, units = 'in')

heat.map.coverage.five.visits <- plot.df.five.visits %>%
  group_by(scenario) %>%
  summarize(coverage = mean(coverage)) %>%
  mutate(x = rep(1:4, time = 4), 
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = coverage)) + 
  geom_raster() + 
  geom_text(aes(label = scenario)) + 
  scale_fill_gradient2(midpoint = 0.95, low = '#B2182B', mid = 'white', high = '#2166AC', 
  	               na.value = NA) + 
  # geom_vline(xintercept = 0.95) + 
  theme_bw(base_size = 18)
heat.map.coverage.five.visits
ggsave(file = 'figures/sim-five-visits-heat-map-cov.png', width = 6, height = 6, units = 'in')

# Credible width ----------------------------------------------------------
psi.ci.width.five.visits.full <- array(NA, dim = dim(psi.low.samples))
for (i in 1:n.sims) { # simulations
  for (k in 1:n.time) { # time
    for (j in 1:n.scenarios) { # scenarios
      psi.ci.width.five.visits.full[, k, i, j] <- psi.high.samples[, k, i, j] - psi.low.samples[, k, i, j]
    } # j
  } # k
} # i

psi.ci.width.five.visits <- apply(psi.ci.width.five.visits.full, c(3, 4), mean)

plot.df.five.visits <- data.frame(ci.width = c(psi.ci.width.five.visits), 
			 sim.num = rep(1:n.sims, times = n.scenarios),
			 scenario = rep(LETTERS[1:n.scenarios], each = n.sims))
hist.ci.width.five.visits <- ggplot(data = plot.df.five.visits, aes(x = ci.width)) + 
  geom_histogram() + 
  facet_wrap(vars(scenario)) + 
  theme_bw(base_size = 18)
hist.ci.width.five.visits
ggsave(file = 'figures/sim-five-visits-hist-ci-width.png', width = 14, height = 9, units = 'in')

heat.map.ci.width.five.visits <- plot.df.five.visits %>%
  group_by(scenario) %>%
  summarize(ci.width = mean(ci.width)) %>%
  mutate(x = rep(1:4, time = 4), 
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = ci.width)) + 
  geom_raster() + 
  scale_fill_viridis_c() +
  theme_bw(base_size = 18)
heat.map.ci.width.five.visits
ggsave(file = 'figures/sim-five-visits-heat-map-ci-width.png', width = 6, height = 6, units = 'in')

# Five visit, scale high ------------------------------------------------
# Load in results --------------------------------------------------------
load("results/sim-five.visits-scale-high-results.rda")
# Simulation scenarios ----------------------------------------------------
sigma.sq.vals <- c(0.3, 1.5)
phi.vals <- c(3 / .2, 3 / .8)
rho.vals <- c(0.2, 0.8)
sigma.sq.t.vals <- c(0.3, 1.5)
n.scenarios <- length(sigma.sq.vals) * length(phi.vals) *
               length(rho.vals) * length(sigma.sq.t.vals)
scenario.vals <- expand.grid(sigma.sq = sigma.sq.vals, phi = phi.vals,
			     rho = rho.vals, sigma.sq.t = sigma.sq.t.vals)

plot(c(psi.true[, , 1, 1]), c(psi.mean.samples[, , 1, 1]), pch = 19)
abline(0, 1)

plot.df <- as.data.frame.table(psi.true)
colnames(plot.df) <- c('site', 'year', 'sim', 'scenario', 'val')
psi.mean.df <- as.data.frame.table(psi.mean.samples)
colnames(psi.mean.df) <- c('site', 'year', 'sim', 'scenario', 'val')
plot.df$est <- psi.mean.df$val
ggplot(data = plot.df, aes(x = val, y = est, col = sim)) + 
  geom_abline(slope = 1, intercept = 0) + 
  theme_bw(base_size = 18) + 
  facet_wrap(vars(scenario)) + 
  geom_smooth(aes(x = val, y = est)) + 
  guides(col = 'none')
ggsave(file = 'figures/sim-five-visits-scale-high.png', width = 14, height = 9, units = 'in')

avg.df <- plot.df %>%
  group_by(scenario, sim) %>%
  arrange(val, .by_group = TRUE) %>%
  mutate(fake.id = 1:n()) %>%
  ungroup() %>%
  group_by(scenario, fake.id) %>%
  summarize(val.avg = mean(val),
	    est.avg = mean(est)) %>%
  ungroup()

ggplot(data = avg.df, aes(x = val.avg, y = est.avg)) +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw(base_size = 18) +
  facet_wrap(vars(scenario)) +
  geom_smooth(aes(x = val.avg, y = est.avg))
ggsave(file = 'figures/sim-five-visits-scale-high-avg.png', width = 14, height = 9, units = 'in')

avg.df %>%
  group_by(scenario) %>%
  summarize(avg.bias = mean(abs(val.avg - est.avg))) %>%
  mutate(x = rep(1:4, time = 4),
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = avg.bias)) +
  geom_raster() +
  geom_text(aes(label = scenario)) +
  scale_fill_viridis_c() +
  theme_bw(base_size = 18)
ggsave(file = 'figures/sim-five-visits-scale-high-heat-map-bias.png', width = 6, height = 6, units = 'in')

# Calculate coverage ------------------------------------------------------
J <- nrow(psi.high.samples)
n.time <- ncol(psi.high.samples)
n.sims <- dim(psi.high.samples)[3]
# msPGOcc
psi.covered.five.visits.scale.high <- array(NA, dim = dim(psi.low.samples))
for (i in 1:n.sims) { # simulations
  print(i)
  for (k in 1:n.time) { # time
    for (j in 1:n.scenarios) { # scenarios
      psi.covered.five.visits.scale.high[, k, i, j] <- ifelse((psi.true[, k, i, j] >
          				   psi.low.samples[, k, i, j]) &
          			          (psi.true[, k, i, j] <
          			           psi.high.samples[, k, i, j]),
          		                   1, 0)
    } # j
  } # k
} # i
psi.coverage.five.visits.scale.high <- apply(psi.covered.five.visits.scale.high, c(3, 4), mean)

plot.df.five.visits.scale.high <- data.frame(coverage = c(psi.coverage.five.visits.scale.high), 
			 sim.num = rep(1:n.sims, times = n.scenarios),
			 scenario = rep(LETTERS[1:n.scenarios], each = n.sims))
hist.coverage.five.visits.scale.high <- ggplot(data = plot.df.five.visits.scale.high, aes(x = coverage)) + 
  geom_histogram() + 
  geom_vline(xintercept = 0.95) + 
  facet_wrap(vars(scenario)) + 
  theme_bw(base_size = 18)
hist.coverage.five.visits.scale.high
ggsave(file = 'figures/sim-five-visits-scale-high-hist-cov.png', width = 14, height = 9, units = 'in')

heat.map.coverage.five.visits.scale.high <- plot.df.five.visits.scale.high %>%
  group_by(scenario) %>%
  summarize(coverage = mean(coverage)) %>%
  mutate(x = rep(1:4, time = 4), 
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = coverage)) + 
  geom_raster() + 
  geom_text(aes(label = scenario)) + 
  scale_fill_gradient2(midpoint = 0.95, low = '#B2182B', mid = 'white', high = '#2166AC', 
  	               na.value = NA) + 
  # geom_vline(xintercept = 0.95) + 
  theme_bw(base_size = 18)
heat.map.coverage.five.visits.scale.high
ggsave(file = 'figures/sim-five-visits-scale-high-heat-map-cov.png', width = 6, height = 6, units = 'in')

# Credible width ----------------------------------------------------------
psi.ci.width.five.visits.scale.high.full <- array(NA, dim = dim(psi.low.samples))
for (i in 1:n.sims) { # simulations
  for (k in 1:n.time) { # time
    for (j in 1:n.scenarios) { # scenarios
      psi.ci.width.five.visits.scale.high.full[, k, i, j] <- psi.high.samples[, k, i, j] - psi.low.samples[, k, i, j]
    } # j
  } # k
} # i

psi.ci.width.five.visits.scale.high <- apply(psi.ci.width.five.visits.scale.high.full, c(3, 4), mean)

plot.df.five.visits.scale.high <- data.frame(ci.width = c(psi.ci.width.five.visits.scale.high), 
			 sim.num = rep(1:n.sims, times = n.scenarios),
			 scenario = rep(LETTERS[1:n.scenarios], each = n.sims))
hist.ci.width.five.visits.scale.high <- ggplot(data = plot.df.five.visits.scale.high, aes(x = ci.width)) + 
  geom_histogram() + 
  facet_wrap(vars(scenario)) + 
  theme_bw(base_size = 18)
hist.ci.width.five.visits.scale.high
ggsave(file = 'figures/sim-five-visits-scale-high-hist-ci-width.png', width = 14, height = 9, units = 'in')

heat.map.ci.width.five.visits.scale.high <- plot.df.five.visits.scale.high %>%
  group_by(scenario) %>%
  summarize(ci.width = mean(ci.width)) %>%
  mutate(x = rep(1:4, time = 4), 
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = ci.width)) + 
  geom_raster() + 
  scale_fill_viridis_c() +
  theme_bw(base_size = 18)
heat.map.ci.width.five.visits.scale.high
ggsave(file = 'figures/sim-five-visits-scale-high-heat-map-ci-width.png', width = 6, height = 6, units = 'in')

# Five visit, scale low -------------------------------------------------
# Load in results --------------------------------------------------------
load("results/sim-five-visits-scale-low-results.rda")
# Simulation scenarios ----------------------------------------------------
sigma.sq.vals <- c(0.3, 1.5)
phi.vals <- c(3 / .2, 3 / .8)
rho.vals <- c(0.2, 0.8)
sigma.sq.t.vals <- c(0.3, 1.5)
n.scenarios <- length(sigma.sq.vals) * length(phi.vals) *
               length(rho.vals) * length(sigma.sq.t.vals)
scenario.vals <- expand.grid(sigma.sq = sigma.sq.vals, phi = phi.vals,
			     rho = rho.vals, sigma.sq.t = sigma.sq.t.vals)

plot(c(psi.true[, , 1, 16]), c(psi.mean.samples[, , 1, 16]), pch = 19)
abline(0, 1)

plot.df <- as.data.frame.table(psi.true)
colnames(plot.df) <- c('site', 'year', 'sim', 'scenario', 'val')
psi.mean.df <- as.data.frame.table(psi.mean.samples)
colnames(psi.mean.df) <- c('site', 'year', 'sim', 'scenario', 'val')
plot.df$est <- psi.mean.df$val
ggplot(data = plot.df, aes(x = val, y = est, col = sim)) + 
  geom_abline(slope = 1, intercept = 0) + 
  theme_bw(base_size = 18) + 
  facet_wrap(vars(scenario)) + 
  geom_smooth(aes(x = val, y = est)) + 
  guides(col = 'none')
ggsave(file = 'figures/sim-five-visits-scale-low.png', width = 14, height = 9, units = 'in')

avg.df <- plot.df %>%
  group_by(scenario, sim) %>%
  arrange(val, .by_group = TRUE) %>%
  mutate(fake.id = 1:n()) %>%
  ungroup() %>%
  group_by(scenario, fake.id) %>%
  summarize(val.avg = mean(val),
	    est.avg = mean(est)) %>%
  ungroup()

ggplot(data = avg.df, aes(x = val.avg, y = est.avg)) +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw(base_size = 18) +
  facet_wrap(vars(scenario)) +
  geom_smooth(aes(x = val.avg, y = est.avg))
ggsave(file = 'figures/sim-five-visits-scale-low-avg.png', width = 14, height = 9, units = 'in')

avg.df %>%
  group_by(scenario) %>%
  summarize(avg.bias = mean(abs(val.avg - est.avg))) %>%
  mutate(x = rep(1:4, time = 4),
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = avg.bias)) +
  geom_raster() +
  geom_text(aes(label = scenario)) +
  scale_fill_viridis_c() +
  theme_bw(base_size = 18)
ggsave(file = 'figures/sim-five-visits-scale-low-heat-map-bias.png', width = 6, height = 6, units = 'in')

# Calculate coverage ------------------------------------------------------
J <- nrow(psi.high.samples)
n.time <- ncol(psi.high.samples)
n.sims <- dim(psi.high.samples)[3]
# msPGOcc
psi.covered.five.visits.scale.low <- array(NA, dim = dim(psi.low.samples))
for (i in 1:n.sims) { # simulations
  print(i)
  for (k in 1:n.time) { # time
    for (j in 1:n.scenarios) { # scenarios
      psi.covered.five.visits.scale.low[, k, i, j] <- ifelse((psi.true[, k, i, j] >
          				   psi.low.samples[, k, i, j]) &
          			          (psi.true[, k, i, j] <
          			           psi.high.samples[, k, i, j]),
          		                   1, 0)
    } # j
  } # k
} # i
psi.coverage.five.visits.scale.low <- apply(psi.covered.five.visits.scale.low, c(3, 4), mean)

plot.df.five.visits.scale.low <- data.frame(coverage = c(psi.coverage.five.visits.scale.low), 
			 sim.num = rep(1:n.sims, times = n.scenarios),
			 scenario = rep(LETTERS[1:n.scenarios], each = n.sims))
hist.coverage.five.visits.scale.low <- ggplot(data = plot.df.five.visits.scale.low, aes(x = coverage)) + 
  geom_histogram() + 
  geom_vline(xintercept = 0.95) + 
  facet_wrap(vars(scenario)) + 
  theme_bw(base_size = 18)
hist.coverage.five.visits.scale.low
ggsave(file = 'figures/sim-five-visits-scale-low-hist-cov.png', width = 14, height = 9, units = 'in')

heat.map.coverage.five.visits.scale.low <- plot.df.five.visits.scale.low %>%
  group_by(scenario) %>%
  summarize(coverage = mean(coverage)) %>%
  mutate(x = rep(1:4, time = 4), 
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = coverage)) + 
  geom_raster() + 
  geom_text(aes(label = scenario)) + 
  scale_fill_gradient2(midpoint = 0.95, low = '#B2182B', mid = 'white', high = '#2166AC', 
  	               na.value = NA) + 
  # geom_vline(xintercept = 0.95) + 
  theme_bw(base_size = 18)
heat.map.coverage.five.visits.scale.low
ggsave(file = 'figures/sim-five-visits-scale-low-heat-map-cov.png', width = 6, height = 6, units = 'in')

# Credible width ----------------------------------------------------------
psi.ci.width.five.visits.scale.low.full <- array(NA, dim = dim(psi.low.samples))
for (i in 1:n.sims) { # simulations
  for (k in 1:n.time) { # time
    for (j in 1:n.scenarios) { # scenarios
      psi.ci.width.five.visits.scale.low.full[, k, i, j] <- psi.high.samples[, k, i, j] - psi.low.samples[, k, i, j]
    } # j
  } # k
} # i

psi.ci.width.five.visits.scale.low <- apply(psi.ci.width.five.visits.scale.low.full, c(3, 4), mean)

plot.df.five.visits.scale.low <- data.frame(ci.width = c(psi.ci.width.five.visits.scale.low), 
			 sim.num = rep(1:n.sims, times = n.scenarios),
			 scenario = rep(LETTERS[1:n.scenarios], each = n.sims))
hist.ci.width.five.visits.scale.low <- ggplot(data = plot.df.five.visits.scale.low, aes(x = ci.width)) + 
  geom_histogram() + 
  facet_wrap(vars(scenario)) + 
  theme_bw(base_size = 18)
hist.ci.width.five.visits.scale.low
ggsave(file = 'figures/sim-five-visits-scale-low-hist-ci-width.png', width = 14, height = 9, units = 'in')

heat.map.ci.width.five.visits.scale.low <- plot.df.five.visits.scale.low %>%
  group_by(scenario) %>%
  summarize(ci.width = mean(ci.width)) %>%
  mutate(x = rep(1:4, time = 4), 
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = ci.width)) + 
  geom_raster() + 
  scale_fill_viridis_c() +
  theme_bw(base_size = 18)
heat.map.ci.width.five.visits.scale.low
ggsave(file = 'figures/sim-five-visits-scale-low-heat-map-ci-width.png', width = 6, height = 6, units = 'in')

# Five visit, line ------------------------------------------------------
# Load in results --------------------------------------------------------
load("results/sim-five-visits-line-results.rda")
# Simulation scenarios ----------------------------------------------------
sigma.sq.vals <- c(0.3, 1.5)
phi.vals <- c(3 / .2, 3 / .8)
rho.vals <- c(0.2, 0.8)
sigma.sq.t.vals <- c(0.3, 1.5)
n.scenarios <- length(sigma.sq.vals) * length(phi.vals) *
               length(rho.vals) * length(sigma.sq.t.vals)
scenario.vals <- expand.grid(sigma.sq = sigma.sq.vals, phi = phi.vals,
			     rho = rho.vals, sigma.sq.t = sigma.sq.t.vals)

plot(c(psi.true[, , 1, 1]), c(psi.mean.samples[, , 1, 1]), pch = 19)
abline(0, 1)

plot.df <- as.data.frame.table(psi.true)
colnames(plot.df) <- c('site', 'year', 'sim', 'scenario', 'val')
psi.mean.df <- as.data.frame.table(psi.mean.samples)
colnames(psi.mean.df) <- c('site', 'year', 'sim', 'scenario', 'val')
plot.df$est <- psi.mean.df$val
ggplot(data = plot.df, aes(x = val, y = est, col = sim)) + 
  geom_abline(slope = 1, intercept = 0) + 
  theme_bw(base_size = 18) + 
  facet_wrap(vars(scenario)) + 
  geom_smooth(aes(x = val, y = est)) + 
  guides(col = 'none')
ggsave(file = 'figures/sim-five-visits-line.png', width = 14, height = 9, units = 'in')

avg.df <- plot.df %>%
  group_by(scenario, sim) %>%
  arrange(val, .by_group = TRUE) %>%
  mutate(fake.id = 1:n()) %>%
  ungroup() %>%
  group_by(scenario, fake.id) %>%
  summarize(val.avg = mean(val),
	    est.avg = mean(est)) %>%
  ungroup()

ggplot(data = avg.df, aes(x = val.avg, y = est.avg)) +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw(base_size = 18) +
  facet_wrap(vars(scenario)) +
  geom_smooth(aes(x = val.avg, y = est.avg))
ggsave(file = 'figures/sim-five-visits-line-avg.png', width = 14, height = 9, units = 'in')

avg.df %>%
  group_by(scenario) %>%
  summarize(avg.bias = mean(abs(val.avg - est.avg))) %>%
  mutate(x = rep(1:4, time = 4),
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = avg.bias)) +
  geom_raster() +
  geom_text(aes(label = scenario)) +
  scale_fill_viridis_c() +
  theme_bw(base_size = 18)
ggsave(file = 'figures/sim-five-visits-line-heat-map-bias.png', width = 6, height = 6, units = 'in')

# Calculate coverage ------------------------------------------------------
J <- nrow(psi.high.samples)
n.time <- ncol(psi.high.samples)
n.sims <- dim(psi.high.samples)[3]
# msPGOcc
psi.covered.five.visits.line <- array(NA, dim = dim(psi.low.samples))
for (i in 1:n.sims) { # simulations
  print(i)
  for (k in 1:n.time) { # time
    for (j in 1:n.scenarios) { # scenarios
      psi.covered.five.visits.line[, k, i, j] <- ifelse((psi.true[, k, i, j] >
          				   psi.low.samples[, k, i, j]) &
          			          (psi.true[, k, i, j] <
          			           psi.high.samples[, k, i, j]),
          		                   1, 0)
    } # j
  } # k
} # i
psi.coverage.five.visits.line <- apply(psi.covered.five.visits.line, c(3, 4), mean)

plot.df.five.visits.line <- data.frame(coverage = c(psi.coverage.five.visits.line),
			 sim.num = rep(1:n.sims, times = n.scenarios),
			 scenario = rep(LETTERS[1:n.scenarios], each = n.sims))
hist.coverage.five.visits.line <- ggplot(data = plot.df.five.visits.line, aes(x = coverage)) +
  geom_histogram() +
  geom_vline(xintercept = 0.95) +
  facet_wrap(vars(scenario)) +
  theme_bw(base_size = 18)
hist.coverage.five.visits.line
ggsave(file = 'figures/sim-five-visits-line-hist-cov.png', width = 14, height = 9, units = 'in')

heat.map.coverage.five.visits.line <- plot.df.five.visits.line %>%
  group_by(scenario) %>%
  summarize(coverage = mean(coverage)) %>%
  mutate(x = rep(1:4, time = 4), 
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = coverage)) +
  geom_raster() +
  geom_text(aes(label = scenario)) + 
  scale_fill_gradient2(midpoint = 0.95, low = '#B2182B', mid = 'white', high = '#2166AC',
  	               na.value = NA) +
  # geom_vline(xintercept = 0.95) +
  theme_bw(base_size = 18)
heat.map.coverage.five.visits.line
ggsave(file = 'figures/sim-five-visits-line-heat-map-cov.png', width = 6, height = 6, units = 'in')

# Credible width ----------------------------------------------------------
psi.ci.width.five.visits.line.full <- array(NA, dim = dim(psi.low.samples))
for (i in 1:n.sims) { # simulations
  for (k in 1:n.time) { # time
    for (j in 1:n.scenarios) { # scenarios
      psi.ci.width.five.visits.line.full[, k, i, j] <- psi.high.samples[, k, i, j] - psi.low.samples[, k, i, j]
    } # j
  } # k
} # i

psi.ci.width.five.visits.line <- apply(psi.ci.width.five.visits.line.full, c(3, 4), mean)

plot.df.five.visits.line <- data.frame(ci.width = c(psi.ci.width.five.visits.line),
			 sim.num = rep(1:n.sims, times = n.scenarios),
			 scenario = rep(LETTERS[1:n.scenarios], each = n.sims))
hist.ci.width.five.visits.line <- ggplot(data = plot.df.five.visits.line, aes(x = ci.width)) +
  geom_histogram() +
  facet_wrap(vars(scenario)) +
  theme_bw(base_size = 18)
hist.ci.width.five.visits.line
ggsave(file = 'figures/sim-five-visits-line-hist-ci-width.png', width = 14, height = 9, units = 'in')

heat.map.ci.width.five.visits.line <- plot.df.five.visits.line %>%
  group_by(scenario) %>%
  summarize(ci.width = mean(ci.width)) %>%
  mutate(x = rep(1:4, time = 4), 
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = ci.width)) +
  geom_raster() +
  scale_fill_viridis_c() +
  theme_bw(base_size = 18)
heat.map.ci.width.five.visits.line
ggsave(file = 'figures/sim-five-visits-line-heat-map-ci-width.png', width = 6, height = 6, units = 'in')

# Five visit, probit ------------------------------------------------------
# Load in results --------------------------------------------------------
load("results/sim-five-visits-probit-results.rda")
# Simulation scenarios ----------------------------------------------------
sigma.sq.vals <- c(0.3, 1.5)
phi.vals <- c(3 / .2, 3 / .8)
rho.vals <- c(0.2, 0.8)
sigma.sq.t.vals <- c(0.3, 1.5)
n.scenarios <- length(sigma.sq.vals) * length(phi.vals) *
               length(rho.vals) * length(sigma.sq.t.vals)
scenario.vals <- expand.grid(sigma.sq = sigma.sq.vals, phi = phi.vals,
			     rho = rho.vals, sigma.sq.t = sigma.sq.t.vals)

plot(c(psi.true[, , 1, 1]), c(psi.mean.samples[, , 1, 1]), pch = 19)
abline(0, 1)

plot.df <- as.data.frame.table(psi.true)
colnames(plot.df) <- c('site', 'year', 'sim', 'scenario', 'val')
psi.mean.df <- as.data.frame.table(psi.mean.samples)
colnames(psi.mean.df) <- c('site', 'year', 'sim', 'scenario', 'val')
plot.df$est <- psi.mean.df$val
ggplot(data = plot.df, aes(x = val, y = est, col = sim)) + 
  geom_abline(slope = 1, intercept = 0) + 
  theme_bw(base_size = 18) + 
  facet_wrap(vars(scenario)) + 
  geom_smooth(aes(x = val, y = est)) + 
  guides(col = 'none')
ggsave(file = 'figures/sim-five-visits-probit.png', width = 14, height = 9, units = 'in')

avg.df <- plot.df %>%
  group_by(scenario, sim) %>%
  arrange(val, .by_group = TRUE) %>%
  mutate(fake.id = 1:n()) %>%
  ungroup() %>%
  group_by(scenario, fake.id) %>%
  summarize(val.avg = mean(val),
	    est.avg = mean(est)) %>%
  ungroup()

ggplot(data = avg.df, aes(x = val.avg, y = est.avg)) +
  geom_abline(slope = 1, intercept = 0) +
  theme_bw(base_size = 18) +
  facet_wrap(vars(scenario)) +
  geom_smooth(aes(x = val.avg, y = est.avg))
ggsave(file = 'figures/sim-five-visits-probit-avg.png', width = 14, height = 9, units = 'in')

avg.df %>%
  group_by(scenario) %>%
  summarize(avg.bias = mean(abs(val.avg - est.avg))) %>%
  mutate(x = rep(1:4, time = 4),
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = avg.bias)) +
  geom_raster() +
  geom_text(aes(label = scenario)) +
  scale_fill_viridis_c() +
  theme_bw(base_size = 18)
ggsave(file = 'figures/sim-five-visits-probit-heat-map-bias.png', width = 6, height = 6, units = 'in')

# Calculate coverage ------------------------------------------------------
J <- nrow(psi.high.samples)
n.time <- ncol(psi.high.samples)
n.sims <- dim(psi.high.samples)[3]
# msPGOcc
psi.covered.five.visits.probit <- array(NA, dim = dim(psi.low.samples))
for (i in 1:n.sims) { # simulations
  print(i)
  for (k in 1:n.time) { # time
    for (j in 1:n.scenarios) { # scenarios
      psi.covered.five.visits.probit[, k, i, j] <- ifelse((psi.true[, k, i, j] >
          				   psi.low.samples[, k, i, j]) &
          			          (psi.true[, k, i, j] <
          			           psi.high.samples[, k, i, j]),
          		                   1, 0)
    } # j
  } # k
} # i
psi.coverage.five.visits.probit <- apply(psi.covered.five.visits.probit, c(3, 4), mean)

plot.df.five.visits.probit <- data.frame(coverage = c(psi.coverage.five.visits.probit),
			 sim.num = rep(1:n.sims, times = n.scenarios),
			 scenario = rep(LETTERS[1:n.scenarios], each = n.sims))
hist.coverage.five.visits.probit <- ggplot(data = plot.df.five.visits.probit, aes(x = coverage)) +
  geom_histogram() +
  geom_vline(xintercept = 0.95) +
  facet_wrap(vars(scenario)) +
  theme_bw(base_size = 18)
hist.coverage.five.visits.probit
ggsave(file = 'figures/sim-five-visits-probit-hist-cov.png', width = 14, height = 9, units = 'in')

heat.map.coverage.five.visits.probit <- plot.df.five.visits.probit %>%
  group_by(scenario) %>%
  summarize(coverage = mean(coverage)) %>%
  mutate(x = rep(1:4, time = 4), 
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = coverage)) +
  geom_raster() +
  geom_text(aes(label = scenario)) + 
  scale_fill_gradient2(midpoint = 0.95, low = '#B2182B', mid = 'white', high = '#2166AC',
  	               na.value = NA) +
  # geom_vprobit(xintercept = 0.95) +
  theme_bw(base_size = 18)
heat.map.coverage.five.visits.probit
ggsave(file = 'figures/sim-five-visits-probit-heat-map-cov.png', width = 6, height = 6, units = 'in')

# Credible width ----------------------------------------------------------
psi.ci.width.five.visits.probit.full <- array(NA, dim = dim(psi.low.samples))
for (i in 1:n.sims) { # simulations
  for (k in 1:n.time) { # time
    for (j in 1:n.scenarios) { # scenarios
      psi.ci.width.five.visits.probit.full[, k, i, j] <- psi.high.samples[, k, i, j] - psi.low.samples[, k, i, j]
    } # j
  } # k
} # i

psi.ci.width.five.visits.probit <- apply(psi.ci.width.five.visits.probit.full, c(3, 4), mean)

plot.df.five.visits.probit <- data.frame(ci.width = c(psi.ci.width.five.visits.probit),
			 sim.num = rep(1:n.sims, times = n.scenarios),
			 scenario = rep(LETTERS[1:n.scenarios], each = n.sims))
hist.ci.width.five.visits.probit <- ggplot(data = plot.df.five.visits.probit, aes(x = ci.width)) +
  geom_histogram() +
  facet_wrap(vars(scenario)) +
  theme_bw(base_size = 18)
hist.ci.width.five.visits.probit
ggsave(file = 'figures/sim-five-visits-probit-hist-ci-width.png', width = 14, height = 9, units = 'in')

heat.map.ci.width.five.visits.probit <- plot.df.five.visits.probit %>%
  group_by(scenario) %>%
  summarize(ci.width = mean(ci.width)) %>%
  mutate(x = rep(1:4, time = 4), 
	 y = rep(4:1, each = 4)) %>%
ggplot(aes(x = x, y = y, fill = ci.width)) +
  geom_raster() +
  scale_fill_viridis_c() +
  theme_bw(base_size = 18)
heat.map.ci.width.five.visits.probit
ggsave(file = 'figures/sim-five-visits-probit-heat-map-ci-width.png', width = 6, height = 6, units = 'in')
