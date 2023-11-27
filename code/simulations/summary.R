# summary.R: this script summarizes results from the different simulation
#            scenarios for both the main text and supplemental information.
# Authors: Jeffrey W. Doser
# Approximate run time: < 10 min
rm(list = ls())
library(spOccupancy)
library(tidyverse)
# Some color palettes
library(viridis)
library(ggthemes)

# NOTE: the full results files that contain the full posterior distributions
#       for all simulations are too large to put on GitHub, so we have 
#       instead put summary files that can still be used to generate the figures. 
#       Code to generate Table 1 in the manuscript is included but cannot be 
#       run as the files containing the 95% credible intervals for the occupancy
#       probability values for each simulation are too large. Please contact
#       Jeff Doser (doserjef@msu.edu) if you want these files.

# Simulation scenarios ----------------------------------------------------
# Note the values are actually not all the same across the different 
# scenarios because of how the covariate sizes influence what's going on 
# under the different link functions, but that doesn't matter for the 
# purposes here.
sigma.sq.vals <- c(0.3, 1.5)
phi.vals <- c(3 / .2, 3 / .8)
rho.vals <- c(0.2, 0.8)
sigma.sq.t.vals <- c(0.3, 1.5)
n.scenarios <- length(sigma.sq.vals) * length(phi.vals) *
               length(rho.vals) * length(sigma.sq.t.vals)
scenario.vals <- expand.grid(sigma.sq = sigma.sq.vals, phi = phi.vals,
			     rho = rho.vals, sigma.sq.t = sigma.sq.t.vals)
# For storing average coverage values
coverage.df <- data.frame(logit = rep(NA, 5),
			  probit = rep(NA, 5),
			  scale.high = rep(NA, 5),
			  scale.low = rep(NA, 5),
			  line = rep(NA, 5))

# Logistic regression results ---------------------------------------------
# Code to manipulate full model results 
# load("results/sim-sv-no-det-results.rda")
# 
# plot.df <- as.data.frame.table(psi.true)
# colnames(plot.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# psi.mean.df <- as.data.frame.table(psi.mean.samples)
# colnames(psi.mean.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# plot.df$est <- psi.mean.df$val
# plot.df$diff <- plot.df$val - plot.df$est
# 
# avg.df.sv.nd <- plot.df %>%
#   group_by(scenario) %>%
#   summarize(diff.avg = mean(abs(diff)), 
# 	    diff.quant.low = quantile(abs(diff), 0.025),
# 	    diff.quant.high = quantile(abs(diff), 0.975))
# avg.df.sv.nd$type <- 'Logit'
# avg.df <- plot.df %>%
#   group_by(scenario, sim) %>%
#   arrange(val, .by_group = TRUE) %>%
#   mutate(fake.id = 1:n()) %>%
#   ungroup() %>%
#   group_by(scenario, fake.id) %>%
#   summarize(val.avg = mean(val),
# 	    est.avg = mean(est)) %>%
#   ungroup()
# avg.df.2.sv.nd <- avg.df
# avg.df.2.sv.nd$type <- 'Logit'
# save(avg.df.sv.nd, avg.df.2.sv.nd, file = 'results/sv-nd-bias-summary.rda')

# Load in summary of full model results
load("results/sv-nd-bias-summary.rda")

# Calculate coverage ------------------
# Files not available on Github
# J <- nrow(psi.high.samples)
# n.time <- ncol(psi.high.samples)
# n.sims <- dim(psi.high.samples)[3]
# psi.covered.sv.nd <- array(NA, dim = dim(psi.low.samples))
# for (i in 1:n.sims) { # simulations
#   print(i)
#   for (k in 1:n.time) { # time
#     for (j in 1:n.scenarios) { # scenarios
#       psi.covered.sv.nd[, k, i, j] <- ifelse((psi.true[, k, i, j] >
#           				   psi.low.samples[, k, i, j]) &
#           			          (psi.true[, k, i, j] <
#           			           psi.high.samples[, k, i, j]),
#           		                   1, 0)
#     } # j
#   } # k
# } # i
# psi.coverage.sv.nd <- apply(psi.covered.sv.nd, c(3, 4), mean)
# coverage.df[5, 1] <- mean(psi.coverage.sv.nd)
# rm(psi.high.samples, psi.true, psi.mean.samples, psi.low.samples)
# gc()

# Logistic regression probit results --------------------------------------
# Code to manipulate full model results
# load("results/sim-sv-no-det-probit-results.rda")
# 
# plot.df <- as.data.frame.table(psi.true)
# colnames(plot.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# psi.mean.df <- as.data.frame.table(psi.mean.samples)
# colnames(psi.mean.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# plot.df$est <- psi.mean.df$val
# plot.df$diff <- plot.df$val - plot.df$est
# avg.df.probit <- plot.df %>%
#   group_by(scenario) %>%
#   summarize(diff.avg = mean(abs(diff)), 
# 	    diff.quant.low = quantile(abs(diff), 0.025),
# 	    diff.quant.high = quantile(abs(diff), 0.975))
# avg.df.probit$type <- 'Probit'
# avg.2.probit.df <- plot.df %>%
#   group_by(scenario, sim) %>%
#   arrange(val, .by_group = TRUE) %>%
#   mutate(fake.id = 1:n()) %>%
#   ungroup() %>%
#   group_by(scenario, fake.id) %>%
#   summarize(val.avg = mean(val),
# 	    est.avg = mean(est)) %>%
#   ungroup()
# avg.2.probit.df$type <- 'Probit'
# save(avg.df.probit, avg.2.probit.df, file = 'results/sv-nd-probit-bias-summary.rda')

load("results/sv-nd-probit-bias-summary.rda")

# Calculate coverage ------------------
# J <- nrow(psi.high.samples)
# n.time <- ncol(psi.high.samples)
# n.sims <- dim(psi.high.samples)[3]
# psi.covered.sv.nd <- array(NA, dim = dim(psi.low.samples))
# for (i in 1:n.sims) { # simulations
#   print(i)
#   for (k in 1:n.time) { # time
#     for (j in 1:n.scenarios) { # scenarios
#       psi.covered.sv.nd[, k, i, j] <- ifelse((psi.true[, k, i, j] >
#           				   psi.low.samples[, k, i, j]) &
#           			          (psi.true[, k, i, j] <
#           			           psi.high.samples[, k, i, j]),
#           		                   1, 0)
#     } # j
#   } # k
# } # i
# psi.coverage.sv.nd <- apply(psi.covered.sv.nd, c(3, 4), mean)
# coverage.df[5, 2] <- mean(psi.coverage.sv.nd)
# rm(psi.high.samples, psi.true, psi.mean.samples, psi.low.samples)
# gc()

# Logistic regression scale low results ------------------------------------
# load("results/sim-sv-no-det-scale-low-results.rda")
# 
# plot.df <- as.data.frame.table(psi.true)
# colnames(plot.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# psi.mean.df <- as.data.frame.table(psi.mean.samples)
# colnames(psi.mean.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# plot.df$est <- psi.mean.df$val
# plot.df$diff <- plot.df$val - plot.df$est
# avg.df.scale.low <- plot.df %>%
#   group_by(scenario) %>%
#   summarize(diff.avg = mean(abs(diff)), 
# 	    diff.quant.low = quantile(abs(diff), 0.025),
# 	    diff.quant.high = quantile(abs(diff), 0.975))
# avg.df.scale.low$type <- 'Scale Low'
# avg.2.scale.low.df <- plot.df %>%
#   group_by(scenario, sim) %>%
#   arrange(val, .by_group = TRUE) %>%
#   mutate(fake.id = 1:n()) %>%
#   ungroup() %>%
#   group_by(scenario, fake.id) %>%
#   summarize(val.avg = mean(val),
# 	    est.avg = mean(est)) %>%
#   ungroup()
# avg.2.scale.low.df$type <- 'Scale Low'
# save(avg.df.scale.low, avg.2.scale.low.df, file = 'results/sv-nd-scale-low-bias-summary.rda')

load("results/sv-nd-scale-low-bias-summary.rda")

# Calculate coverage ------------------
# J <- nrow(psi.high.samples)
# n.time <- ncol(psi.high.samples)
# n.sims <- dim(psi.high.samples)[3]
# psi.covered.sv.nd <- array(NA, dim = dim(psi.low.samples))
# for (i in 1:n.sims) { # simulations
#   print(i)
#   for (k in 1:n.time) { # time
#     for (j in 1:n.scenarios) { # scenarios
#       psi.covered.sv.nd[, k, i, j] <- ifelse((psi.true[, k, i, j] >
#           				   psi.low.samples[, k, i, j]) &
#           			          (psi.true[, k, i, j] <
#           			           psi.high.samples[, k, i, j]),
#           		                   1, 0)
#     } # j
#   } # k
# } # i
# psi.coverage.sv.nd <- apply(psi.covered.sv.nd, c(3, 4), mean)
# coverage.df[5, 4] <- mean(psi.coverage.sv.nd)
# rm(psi.high.samples, psi.true, psi.mean.samples, psi.low.samples)
# gc()

# Logistic regression scale-high results ----------------------------------
# load("results/sim-sv-no-det-scale-high-results.rda")
# 
# plot.df <- as.data.frame.table(psi.true)
# colnames(plot.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# psi.mean.df <- as.data.frame.table(psi.mean.samples)
# colnames(psi.mean.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# plot.df$est <- psi.mean.df$val
# plot.df$diff <- plot.df$val - plot.df$est
# 
# avg.df.scale.high <- plot.df %>%
#   group_by(scenario) %>%
#   summarize(diff.avg = mean(abs(diff)), 
# 	    diff.quant.low = quantile(abs(diff), 0.025),
# 	    diff.quant.high = quantile(abs(diff), 0.975))
# avg.df.scale.high$type <- 'Scale High'
# avg.2.scale.high.df <- plot.df %>%
#   group_by(scenario, sim) %>%
#   arrange(val, .by_group = TRUE) %>%
#   mutate(fake.id = 1:n()) %>%
#   ungroup() %>%
#   group_by(scenario, fake.id) %>%
#   summarize(val.avg = mean(val),
# 	    est.avg = mean(est)) %>%
#   ungroup()
# avg.2.scale.high.df$type <- 'Scale High'
# save(avg.df.scale.high, avg.2.scale.high.df, file = 'results/sv-nd-scale-high-bias-summary.rda')

load("results/sv-nd-scale-high-bias-summary.rda")

# Calculate coverage ------------------
# J <- nrow(psi.high.samples)
# n.time <- ncol(psi.high.samples)
# n.sims <- dim(psi.high.samples)[3]
# psi.covered.sv <- array(NA, dim = dim(psi.low.samples))
# for (i in 1:n.sims) { # simulations
#   print(i)
#   for (k in 1:n.time) { # time
#     for (j in 1:n.scenarios) { # scenarios
#       psi.covered.sv[, k, i, j] <- ifelse((psi.true[, k, i, j] >
#           				   psi.low.samples[, k, i, j]) &
#           			          (psi.true[, k, i, j] <
#           			           psi.high.samples[, k, i, j]),
#           		                   1, 0)
#     } # j
#   } # k
# } # i
# psi.coverage.sv <- apply(psi.covered.sv, c(3, 4), mean)
# coverage.df[5, 3] <- mean(psi.coverage.sv)
# rm(psi.high.samples, psi.true, psi.mean.samples, psi.low.samples)
# gc()

# Logistic regression line results ----------------------------------------
# load("results/sim-sv-no-det-line-results.rda")
# 
# plot.df <- as.data.frame.table(psi.true)
# colnames(plot.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# psi.mean.df <- as.data.frame.table(psi.mean.samples)
# colnames(psi.mean.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# plot.df$est <- psi.mean.df$val
# plot.df$diff <- plot.df$val - plot.df$est
# avg.df.line <- plot.df %>%
#   group_by(scenario) %>%
#   summarize(diff.avg = mean(abs(diff)), 
# 	    diff.quant.low = quantile(abs(diff), 0.025),
# 	    diff.quant.high = quantile(abs(diff), 0.975))
# avg.df.line$type <- 'Linear'
# avg.2.line.df <- plot.df %>%
#   group_by(scenario, sim) %>%
#   arrange(val, .by_group = TRUE) %>%
#   mutate(fake.id = 1:n()) %>%
#   ungroup() %>%
#   group_by(scenario, fake.id) %>%
#   summarize(val.avg = mean(val),
# 	    est.avg = mean(est)) %>%
#   ungroup()
# avg.2.line.df$type <- 'Linear'
# save(avg.df.line, avg.2.line.df, file = 'results/sv-nd-line-bias-summary.rda')

load("results/sv-nd-line-bias-summary.rda")

# Calculate coverage ------------------
# J <- nrow(psi.high.samples)
# n.time <- ncol(psi.high.samples)
# n.sims <- dim(psi.high.samples)[3]
# psi.covered.sv <- array(NA, dim = dim(psi.low.samples))
# for (i in 1:n.sims) { # simulations
#   print(i)
#   for (k in 1:n.time) { # time
#     for (j in 1:n.scenarios) { # scenarios
#       psi.covered.sv[, k, i, j] <- ifelse((psi.true[, k, i, j] >
#           				   psi.low.samples[, k, i, j]) &
#           			          (psi.true[, k, i, j] <
#           			           psi.high.samples[, k, i, j]),
#           		                   1, 0)
#     } # j
#   } # k
# } # i
# psi.coverage.sv <- apply(psi.covered.sv, c(3, 4), mean)
# coverage.df[5, 5] <- mean(psi.coverage.sv)
# rm(psi.high.samples, psi.true, psi.mean.samples, psi.low.samples)
# gc()

# Generate plot -----------------------------------------------------------
avg.df.full <- rbind(avg.df.sv.nd, avg.df.probit, avg.df.scale.low, 
		     avg.df.scale.high, avg.df.line)
avg.df.full$type <- ifelse(avg.df.full$type == 'Logit', 'Logit (correct model)', avg.df.full$type)
avg.df.full$type <- factor(avg.df.full$type, levels = c('Logit (correct model)', 'Probit', 
							'Scale High', 'Scale Low', 
							'Linear'), 
			   ordered = TRUE)
avg.df.full$scenario <- as.numeric(avg.df.full$scenario)
avg.df.full$spatial <- ifelse(avg.df.full$scenario %in% c(1, 5, 9, 13), 'A', 
			      ifelse(avg.df.full$scenario %in% c(2, 6, 10, 14), 'B', 
				     ifelse(avg.df.full$scenario %in% c(3, 7, 11, 15), 'C', 'D')))
avg.df.full$time <- ifelse(avg.df.full$scenario %in% c(1, 2, 3, 4), 'A', 
			      ifelse(avg.df.full$scenario %in% c(5, 6, 7, 8), 'B', 
				     ifelse(avg.df.full$scenario %in% c(9, 10, 11, 12), 'C', 'D')))
spatial.labs <- c(expression(paste(sigma, " "^2, " Low, ", phi, "  High")), 
                  expression(paste(sigma, " "^2, " High, ", phi, "  High")),
                  expression(paste(sigma, " "^2, " Low, ", phi, "  Low")),
                  expression(paste(sigma, " "^2, " High, ", phi, "  Low")))
avg.df.full$spatial <- factor(avg.df.full$spatial, levels = c('A', 'B', 'C', 'D'), 
			 labels = spatial.labs)
time.labs <- c(expression(paste(sigma, " "[T]^2, " Low, ", rho, "  Low")), 
               expression(paste(sigma, " "[T]^2, " Low, ", rho, "  High")), 
               expression(paste(sigma, " "[T]^2, " High, ", rho, "  Low")), 
               expression(paste(sigma, " "[T]^2, " High, ", rho, "  High"))) 
avg.df.full$time <- factor(avg.df.full$time, levels = c('A', 'B', 'C', 'D'), 
		      labels = time.labs)
# Figure S4 
bias.plot <- ggplot(data = avg.df.full, aes(x = type, y = diff.avg, col = type)) +
  scale_color_colorblind() +
  theme_light(base_size = 12) +
  facet_grid(time ~ spatial, 
	     labeller = label_parsed) +
  geom_segment(aes(x = type, y = diff.quant.low, xend = type, yend = diff.quant.high), 
	       lineend = 'butt', linewidth = 0.65) +
  geom_point(size = 3) +
  scale_y_continuous(limits = c(0, 0.66)) +
  labs(x = 'Link Function', y = 'Absolute Bias', 
       col = '') + 
  theme(legend.position = 'bottom', 
        strip.text.y = element_text(color = 'black'),
        strip.text.x = element_text(color = 'black'), 
        text = element_text(family="LM Roman 10"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1, size = 10), 
        axis.text.y = element_text(size = 10))
bias.plot
ggsave(file = 'figures/Figure-S4.png', device = 'png', width = 8.5, height = 7)
# Generate second plot ----------------------------------------------------
avg.df.2.full <- rbind(avg.df.2.sv.nd, avg.2.probit.df, avg.2.scale.low.df, 
		     avg.2.scale.high.df, avg.2.line.df)
avg.df.2.full$type <- ifelse(avg.df.2.full$type == 'Logit', 'Logit (correct model)', 
			     avg.df.2.full$type)
avg.df.2.full$type <- factor(avg.df.2.full$type, levels = c('Logit (correct model)', 'Probit', 
							'Scale High', 'Scale Low', 
							'Linear'), 
			   ordered = TRUE)
avg.df.2.full$scenario <- as.numeric(avg.df.2.full$scenario)
avg.df.2.full$spatial <- ifelse(avg.df.2.full$scenario %in% c(1, 5, 9, 13), 'A', 
			      ifelse(avg.df.2.full$scenario %in% c(2, 6, 10, 14), 'B', 
				     ifelse(avg.df.2.full$scenario %in% c(3, 7, 11, 15), 'C', 'D')))
avg.df.2.full$time <- ifelse(avg.df.2.full$scenario %in% c(1, 2, 3, 4), 'A', 
			      ifelse(avg.df.2.full$scenario %in% c(5, 6, 7, 8), 'B', 
				     ifelse(avg.df.2.full$scenario %in% c(9, 10, 11, 12), 'C', 'D')))
avg.df.2.full$spatial <- factor(avg.df.2.full$spatial, levels = c('A', 'B', 'C', 'D'), 
			 labels = spatial.labs)
avg.df.2.full$time <- factor(avg.df.2.full$time, levels = c('A', 'B', 'C', 'D'), 
		      labels = time.labs)
# Generate Figure S3
bias.plot.2 <- ggplot(data = avg.df.2.full) +
  theme_light(base_size = 14) +
  facet_grid(time ~ spatial, 
	     labeller = label_parsed) +
  geom_abline(slope = 1, intercept = 0, col = 'grey', lty = 2) +
  geom_smooth(aes(x = val.avg, y = est.avg, col = type, group = type), 
	      se = FALSE, lineend = 'round', lwd = 0.5) +
  scale_color_colorblind() +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = 'True Occupancy Probability', y = 'Estimated Occupancy Probability', col = '') +
  theme(legend.position = 'bottom',
        strip.text.y = element_text(color = 'black'),
        strip.text.x = element_text(color = 'black'),
        text = element_text(family="LM Roman 10"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10))
bias.plot.2
ggsave(file = 'figures/Figure-S3.png', device = 'png', width = 8.5, height = 7, 
       units = 'in')

# Single-visit results ----------------------------------------------------
# Code to manipulate full model results 
# load("results/sim-sv-stPGOcc-results.rda")
# 
# plot.df <- as.data.frame.table(psi.true)
# colnames(plot.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# psi.mean.df <- as.data.frame.table(psi.mean.samples)
# colnames(psi.mean.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# plot.df$est <- psi.mean.df$val
# plot.df$diff <- plot.df$val - plot.df$est
# 
# avg.df.sv <- plot.df %>%
#   group_by(scenario) %>%
#   summarize(diff.avg = mean(abs(diff)), 
# 	    diff.quant.low = quantile(abs(diff), 0.025),
# 	    diff.quant.high = quantile(abs(diff), 0.975))
# avg.df.sv$type <- 'Logit'
# avg.df <- plot.df %>%
#   group_by(scenario, sim) %>%
#   arrange(val, .by_group = TRUE) %>%
#   mutate(fake.id = 1:n()) %>%
#   ungroup() %>%
#   group_by(scenario, fake.id) %>%
#   summarize(val.avg = mean(val),
# 	    est.avg = mean(est)) %>%
#   ungroup()
# avg.df.2.sv <- avg.df
# avg.df.2.sv$type <- 'Logit'
# save(avg.df, avg.df.sv, avg.df.2.sv, file = 'results/sv-bias-summary.rda')

# Load in summary of full model results
load("results/sv-bias-summary.rda")

# Summary plot for single-visit logit
avg.df$scenario <- as.numeric(avg.df$scenario)
avg.df$spatial <- ifelse(avg.df$scenario %in% c(1, 5, 9, 13), 'A', 
			      ifelse(avg.df$scenario %in% c(2, 6, 10, 14), 'B', 
				     ifelse(avg.df$scenario %in% c(3, 7, 11, 15), 'C', 'D')))
avg.df$time <- ifelse(avg.df$scenario %in% c(1, 2, 3, 4), 'A', 
			      ifelse(avg.df$scenario %in% c(5, 6, 7, 8), 'B', 
				     ifelse(avg.df$scenario %in% c(9, 10, 11, 12), 'C', 'D')))
spatial.labs <- c(expression(paste(sigma, " "^2, " Low, ", phi, "  High")), 
                  expression(paste(sigma, " "^2, " High, ", phi, "  High")),
                  expression(paste(sigma, " "^2, " Low, ", phi, "  Low")),
                  expression(paste(sigma, " "^2, " High, ", phi, "  Low")))
avg.df$spatial <- factor(avg.df$spatial, levels = c('A', 'B', 'C', 'D'), 
			 labels = spatial.labs)
time.labs <- c(expression(paste(sigma, " "[T]^2, " Low, ", rho, "  Low")), 
               expression(paste(sigma, " "[T]^2, " Low, ", rho, "  High")), 
               expression(paste(sigma, " "[T]^2, " High, ", rho, "  Low")), 
               expression(paste(sigma, " "[T]^2, " High, ", rho, "  High"))) 
avg.df$time <- factor(avg.df$time, levels = c('A', 'B', 'C', 'D'), 
		      labels = time.labs)
plot.df$scenario <- as.numeric(plot.df$scenario)
plot.df$spatial <- ifelse(plot.df$scenario %in% c(1, 5, 9, 13), 'A', 
			      ifelse(plot.df$scenario %in% c(2, 6, 10, 14), 'B', 
				     ifelse(plot.df$scenario %in% c(3, 7, 11, 15), 'C', 'D')))
plot.df$time <- ifelse(plot.df$scenario %in% c(1, 2, 3, 4), 'A', 
			      ifelse(plot.df$scenario %in% c(5, 6, 7, 8), 'B', 
				     ifelse(plot.df$scenario %in% c(9, 10, 11, 12), 'C', 'D')))
plot.df$spatial <- factor(plot.df$spatial, levels = c('A', 'B', 'C', 'D'), 
			 labels = spatial.labs)
plot.df$time <- factor(plot.df$time, levels = c('A', 'B', 'C', 'D'), 
		      labels = time.labs)

# Figure 3 requires a large object (>400MB) to create, which cannot be stored on 
# Github. 
# Figure 3 of associated manuscript. 
# fig.3.plot <- plot.df %>%
#   ggplot() +
#     theme_light(base_size = 16) +
#     facet_grid(time ~ spatial, 
#   	     labeller = label_parsed) +
#     geom_smooth(aes(x = val, y = est, group = sim), col = 'grey', alpha = 0.1, se = FALSE, 
#   	      lineend = 'round', lwd = 0.25) +
#     geom_abline(slope = 1, intercept = 0, col = 'black', lty = 2) +
#     geom_smooth(data = avg.df, aes(x = val.avg, y = est.avg), se = FALSE, col = 'black', lineend = 'round', 
#   	      lwd = 0.5) + 
#     labs(x = 'True Occupancy Probability', y = 'Estimated Occupancy Probability') + 
#     theme(legend.position = 'bottom', 
#           strip.text.y = element_text(color = 'black'),
#           strip.text.x = element_text(color = 'black'), 
#           text = element_text(family="LM Roman 10"),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           axis.text.x = element_text(angle = 45, hjust = 1, size = 10), 
#           axis.text.y = element_text(size = 10))
# ggsave(plot = fig.3.plot, file = 'figures/Figure-3.png', device = 'png', width = 8.5, height = 7, 
#        units = 'in')

# Calculate coverage ------------------
# Files not available on Github
# J <- nrow(psi.high.samples)
# n.time <- ncol(psi.high.samples)
# n.sims <- dim(psi.high.samples)[3]
# psi.covered.sv <- array(NA, dim = dim(psi.low.samples))
# for (i in 1:n.sims) { # simulations
#   print(i)
#   for (k in 1:n.time) { # time
#     for (j in 1:n.scenarios) { # scenarios
#       psi.covered.sv[, k, i, j] <- ifelse((psi.true[, k, i, j] >
#           				   psi.low.samples[, k, i, j]) &
#           			          (psi.true[, k, i, j] <
#           			           psi.high.samples[, k, i, j]),
#           		                   1, 0)
#     } # j
#   } # k
# } # i
# psi.coverage.sv <- apply(psi.covered.sv, c(3, 4), mean)
# coverage.df[1, 1] <- mean(psi.coverage.sv)
# rm(psi.high.samples, psi.true, psi.mean.samples, psi.low.samples)
# gc()

# Single-visit probit results ----------------------------------------------
# Code to manipulate full model results
# load("results/sim-sv-probit-results.rda")
# 
# plot.df <- as.data.frame.table(psi.true)
# colnames(plot.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# psi.mean.df <- as.data.frame.table(psi.mean.samples)
# colnames(psi.mean.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# plot.df$est <- psi.mean.df$val
# plot.df$diff <- plot.df$val - plot.df$est
# avg.df.probit <- plot.df %>%
#   group_by(scenario) %>%
#   summarize(diff.avg = mean(abs(diff)), 
# 	    diff.quant.low = quantile(abs(diff), 0.025),
# 	    diff.quant.high = quantile(abs(diff), 0.975))
# avg.df.probit$type <- 'Probit'
# avg.2.probit.df <- plot.df %>%
#   group_by(scenario, sim) %>%
#   arrange(val, .by_group = TRUE) %>%
#   mutate(fake.id = 1:n()) %>%
#   ungroup() %>%
#   group_by(scenario, fake.id) %>%
#   summarize(val.avg = mean(val),
# 	    est.avg = mean(est)) %>%
#   ungroup()
# avg.2.probit.df$type <- 'Probit'
# save(avg.df.probit, avg.2.probit.df, file = 'results/sv-probit-bias-summary.rda')

load("results/sv-probit-bias-summary.rda")

# Calculate coverage ------------------
# J <- nrow(psi.high.samples)
# n.time <- ncol(psi.high.samples)
# n.sims <- dim(psi.high.samples)[3]
# psi.covered.sv <- array(NA, dim = dim(psi.low.samples))
# for (i in 1:n.sims) { # simulations
#   print(i)
#   for (k in 1:n.time) { # time
#     for (j in 1:n.scenarios) { # scenarios
#       psi.covered.sv[, k, i, j] <- ifelse((psi.true[, k, i, j] >
#           				   psi.low.samples[, k, i, j]) &
#           			          (psi.true[, k, i, j] <
#           			           psi.high.samples[, k, i, j]),
#           		                   1, 0)
#     } # j
#   } # k
# } # i
# psi.coverage.sv <- apply(psi.covered.sv, c(3, 4), mean)
# coverage.df[1, 2] <- mean(psi.coverage.sv)
# rm(psi.high.samples, psi.true, psi.mean.samples, psi.low.samples)
# gc()

# Single-visit scale-low results ----------------------------------------------
# load("results/sim-sv-scale-low-results.rda")
# 
# plot.df <- as.data.frame.table(psi.true)
# colnames(plot.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# psi.mean.df <- as.data.frame.table(psi.mean.samples)
# colnames(psi.mean.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# plot.df$est <- psi.mean.df$val
# plot.df$diff <- plot.df$val - plot.df$est
# avg.df.scale.low <- plot.df %>%
#   group_by(scenario) %>%
#   summarize(diff.avg = mean(abs(diff)), 
# 	    diff.quant.low = quantile(abs(diff), 0.025),
# 	    diff.quant.high = quantile(abs(diff), 0.975))
# avg.df.scale.low$type <- 'Scale Low'
# avg.2.scale.low.df <- plot.df %>%
#   group_by(scenario, sim) %>%
#   arrange(val, .by_group = TRUE) %>%
#   mutate(fake.id = 1:n()) %>%
#   ungroup() %>%
#   group_by(scenario, fake.id) %>%
#   summarize(val.avg = mean(val),
# 	    est.avg = mean(est)) %>%
#   ungroup()
# avg.2.scale.low.df$type <- 'Scale Low'
# save(avg.df.scale.low, avg.2.scale.low.df, file = 'results/sv-scale-low-bias-summary.rda')

load("results/sv-scale-low-bias-summary.rda")

# Calculate coverage ------------------
# J <- nrow(psi.high.samples)
# n.time <- ncol(psi.high.samples)
# n.sims <- dim(psi.high.samples)[3]
# psi.covered.sv <- array(NA, dim = dim(psi.low.samples))
# for (i in 1:n.sims) { # simulations
#   print(i)
#   for (k in 1:n.time) { # time
#     for (j in 1:n.scenarios) { # scenarios
#       psi.covered.sv[, k, i, j] <- ifelse((psi.true[, k, i, j] >
#           				   psi.low.samples[, k, i, j]) &
#           			          (psi.true[, k, i, j] <
#           			           psi.high.samples[, k, i, j]),
#           		                   1, 0)
#     } # j
#   } # k
# } # i
# psi.coverage.sv <- apply(psi.covered.sv, c(3, 4), mean)
# coverage.df[1, 4] <- mean(psi.coverage.sv)
# rm(psi.high.samples, psi.true, psi.mean.samples, psi.low.samples)
# gc()

# Single-visit scale-high results ----------------------------------------------
# load("results/sim-sv-scale-high-results.rda")
# 
# plot.df <- as.data.frame.table(psi.true)
# colnames(plot.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# psi.mean.df <- as.data.frame.table(psi.mean.samples)
# colnames(psi.mean.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# plot.df$est <- psi.mean.df$val
# plot.df$diff <- plot.df$val - plot.df$est
# 
# avg.df.scale.high <- plot.df %>%
#   group_by(scenario) %>%
#   summarize(diff.avg = mean(abs(diff)), 
# 	    diff.quant.low = quantile(abs(diff), 0.025),
# 	    diff.quant.high = quantile(abs(diff), 0.975))
# avg.df.scale.high$type <- 'Scale High'
# avg.2.scale.high.df <- plot.df %>%
#   group_by(scenario, sim) %>%
#   arrange(val, .by_group = TRUE) %>%
#   mutate(fake.id = 1:n()) %>%
#   ungroup() %>%
#   group_by(scenario, fake.id) %>%
#   summarize(val.avg = mean(val),
# 	    est.avg = mean(est)) %>%
#   ungroup()
# avg.2.scale.high.df$type <- 'Scale High'
# save(avg.df.scale.high, avg.2.scale.high.df, file = 'results/sv-scale-high-bias-summary.rda')

load("results/sv-scale-high-bias-summary.rda")

# Calculate coverage ------------------
# J <- nrow(psi.high.samples)
# n.time <- ncol(psi.high.samples)
# n.sims <- dim(psi.high.samples)[3]
# psi.covered.sv <- array(NA, dim = dim(psi.low.samples))
# for (i in 1:n.sims) { # simulations
#   print(i)
#   for (k in 1:n.time) { # time
#     for (j in 1:n.scenarios) { # scenarios
#       psi.covered.sv[, k, i, j] <- ifelse((psi.true[, k, i, j] >
#           				   psi.low.samples[, k, i, j]) &
#           			          (psi.true[, k, i, j] <
#           			           psi.high.samples[, k, i, j]),
#           		                   1, 0)
#     } # j
#   } # k
# } # i
# psi.coverage.sv <- apply(psi.covered.sv, c(3, 4), mean)
# coverage.df[1, 3] <- mean(psi.coverage.sv)
# rm(psi.high.samples, psi.true, psi.mean.samples, psi.low.samples)
# gc()

# Single-visit line results ----------------------------------------------
# load("results/sim-sv-line-results.rda")
# 
# plot.df <- as.data.frame.table(psi.true)
# colnames(plot.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# psi.mean.df <- as.data.frame.table(psi.mean.samples)
# colnames(psi.mean.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# plot.df$est <- psi.mean.df$val
# plot.df$diff <- plot.df$val - plot.df$est
# avg.df.line <- plot.df %>%
#   group_by(scenario) %>%
#   summarize(diff.avg = mean(abs(diff)), 
# 	    diff.quant.low = quantile(abs(diff), 0.025),
# 	    diff.quant.high = quantile(abs(diff), 0.975))
# avg.df.line$type <- 'Linear'
# avg.2.line.df <- plot.df %>%
#   group_by(scenario, sim) %>%
#   arrange(val, .by_group = TRUE) %>%
#   mutate(fake.id = 1:n()) %>%
#   ungroup() %>%
#   group_by(scenario, fake.id) %>%
#   summarize(val.avg = mean(val),
# 	    est.avg = mean(est)) %>%
#   ungroup()
# avg.2.line.df$type <- 'Linear'
# save(avg.df.line, avg.2.line.df, file = 'results/sv-line-bias-summary.rda')

load("results/sv-line-bias-summary.rda")

# Calculate coverage ------------------
# J <- nrow(psi.high.samples)
# n.time <- ncol(psi.high.samples)
# n.sims <- dim(psi.high.samples)[3]
# psi.covered.sv <- array(NA, dim = dim(psi.low.samples))
# for (i in 1:n.sims) { # simulations
#   print(i)
#   for (k in 1:n.time) { # time
#     for (j in 1:n.scenarios) { # scenarios
#       psi.covered.sv[, k, i, j] <- ifelse((psi.true[, k, i, j] >
#           				   psi.low.samples[, k, i, j]) &
#           			          (psi.true[, k, i, j] <
#           			           psi.high.samples[, k, i, j]),
#           		                   1, 0)
#     } # j
#   } # k
# } # i
# psi.coverage.sv <- apply(psi.covered.sv, c(3, 4), mean)
# coverage.df[1, 5] <- mean(psi.coverage.sv)
# rm(psi.high.samples, psi.true, psi.mean.samples, psi.low.samples)
# gc()

# Generate plot -----------------------------------------------------------
avg.df.full <- rbind(avg.df.sv, avg.df.probit, avg.df.scale.low, 
		     avg.df.scale.high, avg.df.line)
avg.df.full$type <- ifelse(avg.df.full$type == 'Logit', 'Logit (correct model)', avg.df.full$type)
avg.df.full$type <- factor(avg.df.full$type, levels = c('Logit (correct model)', 'Probit', 
							'Scale High', 'Scale Low', 
							'Linear'), 
			   ordered = TRUE)
avg.df.full$scenario <- as.numeric(avg.df.full$scenario)
avg.df.full$spatial <- ifelse(avg.df.full$scenario %in% c(1, 5, 9, 13), 'A', 
			      ifelse(avg.df.full$scenario %in% c(2, 6, 10, 14), 'B', 
				     ifelse(avg.df.full$scenario %in% c(3, 7, 11, 15), 'C', 'D')))
avg.df.full$time <- ifelse(avg.df.full$scenario %in% c(1, 2, 3, 4), 'A', 
			      ifelse(avg.df.full$scenario %in% c(5, 6, 7, 8), 'B', 
				     ifelse(avg.df.full$scenario %in% c(9, 10, 11, 12), 'C', 'D')))
spatial.labs <- c(expression(paste(sigma, " "^2, " Low, ", phi, "  High")), 
                  expression(paste(sigma, " "^2, " High, ", phi, "  High")),
                  expression(paste(sigma, " "^2, " Low, ", phi, "  Low")),
                  expression(paste(sigma, " "^2, " High, ", phi, "  Low")))
avg.df.full$spatial <- factor(avg.df.full$spatial, levels = c('A', 'B', 'C', 'D'), 
			 labels = spatial.labs)
time.labs <- c(expression(paste(sigma, " "[T]^2, " Low, ", rho, "  Low")), 
               expression(paste(sigma, " "[T]^2, " Low, ", rho, "  High")), 
               expression(paste(sigma, " "[T]^2, " High, ", rho, "  Low")), 
               expression(paste(sigma, " "[T]^2, " High, ", rho, "  High"))) 
avg.df.full$time <- factor(avg.df.full$time, levels = c('A', 'B', 'C', 'D'), 
		      labels = time.labs)
# Figure S5 
bias.plot <- ggplot(data = avg.df.full, aes(x = type, y = diff.avg, col = type)) +
  scale_color_colorblind() +
  theme_light(base_size = 12) +
  facet_grid(time ~ spatial, 
	     labeller = label_parsed) +
  geom_segment(aes(x = type, y = diff.quant.low, xend = type, yend = diff.quant.high), 
	       lineend = 'butt', linewidth = 0.7) +
  geom_point(size = 3) +
  scale_y_continuous(limits = c(0, 0.66)) +
  labs(x = 'Link Function', y = 'Absolute Bias', 
       col = '') + 
  theme(legend.position = 'bottom', 
        strip.text.y = element_text(color = 'black'),
        strip.text.x = element_text(color = 'black'), 
        text = element_text(family="LM Roman 10"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1, size = 10), 
        axis.text.y = element_text(size = 10))
bias.plot
ggsave(file = 'figures/Figure-S5.png', device = 'png', width = 8.5, height = 7)
# Generate second plot ----------------------------------------------------
avg.df.2.full <- rbind(avg.df.2.sv, avg.2.probit.df, avg.2.scale.low.df, 
		     avg.2.scale.high.df, avg.2.line.df)
avg.df.2.full$type <- ifelse(avg.df.2.full$type == 'Logit', 'Logit (correct model)', 
			     avg.df.2.full$type)
avg.df.2.full$type <- factor(avg.df.2.full$type, levels = c('Logit (correct model)', 'Probit', 
							'Scale High', 'Scale Low', 
							'Linear'), 
			   ordered = TRUE)
avg.df.2.full$scenario <- as.numeric(avg.df.2.full$scenario)
avg.df.2.full$spatial <- ifelse(avg.df.2.full$scenario %in% c(1, 5, 9, 13), 'A', 
			      ifelse(avg.df.2.full$scenario %in% c(2, 6, 10, 14), 'B', 
				     ifelse(avg.df.2.full$scenario %in% c(3, 7, 11, 15), 'C', 'D')))
avg.df.2.full$time <- ifelse(avg.df.2.full$scenario %in% c(1, 2, 3, 4), 'A', 
			      ifelse(avg.df.2.full$scenario %in% c(5, 6, 7, 8), 'B', 
				     ifelse(avg.df.2.full$scenario %in% c(9, 10, 11, 12), 'C', 'D')))
avg.df.2.full$spatial <- factor(avg.df.2.full$spatial, levels = c('A', 'B', 'C', 'D'), 
			 labels = spatial.labs)
avg.df.2.full$time <- factor(avg.df.2.full$time, levels = c('A', 'B', 'C', 'D'), 
		      labels = time.labs)
# Generate Figure 4
bias.plot.2 <- ggplot(data = avg.df.2.full) +
  theme_light(base_size = 14) +
  facet_grid(time ~ spatial, 
	     labeller = label_parsed) +
  geom_abline(slope = 1, intercept = 0, col = 'grey', lty = 2) +
  geom_smooth(aes(x = val.avg, y = est.avg, col = type, group = type), 
	      se = FALSE, lineend = 'round', lwd = 0.5) +
  scale_color_colorblind() +
  labs(x = 'True Occupancy Probability', y = 'Estimated Occupancy Probability', col = '') +
  theme(legend.position = 'bottom',
        strip.text.y = element_text(color = 'black'),
        strip.text.x = element_text(color = 'black'),
        text = element_text(family="LM Roman 10"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10))
bias.plot.2
ggsave(file = 'figures/Figure-4.png', device = 'png', width = 8.5, height = 7, 
       units = 'in')

# Mixed results -----------------------------------------------------------
# load("results/sim-mixed-stPGOcc-results.rda")
# 
# plot.df <- as.data.frame.table(psi.true)
# colnames(plot.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# psi.mean.df <- as.data.frame.table(psi.mean.samples)
# colnames(psi.mean.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# plot.df$est <- psi.mean.df$val
# plot.df$diff <- plot.df$val - plot.df$est
# avg.df.mixed <- plot.df %>%
#   group_by(scenario) %>%
#   summarize(diff.avg = mean(abs(diff)), 
# 	    diff.quant.low = quantile(abs(diff), 0.025),
# 	    diff.quant.high = quantile(abs(diff), 0.975))
# avg.df.mixed$type <- 'Logit'
# avg.2.mixed.df <- plot.df %>%
#   group_by(scenario, sim) %>%
#   arrange(val, .by_group = TRUE) %>%
#   mutate(fake.id = 1:n()) %>%
#   ungroup() %>%
#   group_by(scenario, fake.id) %>%
#   summarize(val.avg = mean(val),
# 	    est.avg = mean(est)) %>%
#   ungroup()
# avg.2.mixed.df$type <- 'Logit'
# save(avg.df.mixed, avg.2.mixed.df, file = 'results/mixed-bias-summary.rda')

load('results/mixed-bias-summary.rda')

# Calculate coverage ------------------
# J <- nrow(psi.high.samples)
# n.time <- ncol(psi.high.samples)
# n.sims <- dim(psi.high.samples)[3]
# psi.covered.sv <- array(NA, dim = dim(psi.low.samples))
# for (i in 1:n.sims) { # simulations
#   print(i)
#   for (k in 1:n.time) { # time
#     for (j in 1:n.scenarios) { # scenarios
#       psi.covered.sv[, k, i, j] <- ifelse((psi.true[, k, i, j] >
#           				   psi.low.samples[, k, i, j]) &
#           			          (psi.true[, k, i, j] <
#           			           psi.high.samples[, k, i, j]),
#           		                   1, 0)
#     } # j
#   } # k
# } # i
# psi.coverage.sv <- apply(psi.covered.sv, c(3, 4), mean)
# coverage.df[2, 1] <- mean(psi.coverage.sv)
# rm(psi.high.samples, psi.true, psi.mean.samples, psi.low.samples)
# gc()

# Mixed probit results ----------------------------------------------
# load("results/sim-mixed-probit-results.rda")
# 
# plot.df <- as.data.frame.table(psi.true)
# colnames(plot.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# psi.mean.df <- as.data.frame.table(psi.mean.samples)
# colnames(psi.mean.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# plot.df$est <- psi.mean.df$val
# plot.df$diff <- plot.df$val - plot.df$est
# avg.df.probit <- plot.df %>%
#   group_by(scenario) %>%
#   summarize(diff.avg = mean(abs(diff)), 
# 	    diff.quant.low = quantile(abs(diff), 0.025),
# 	    diff.quant.high = quantile(abs(diff), 0.975))
# avg.df.probit$type <- 'Probit'
# avg.2.mixed.probit.df <- plot.df %>%
#   group_by(scenario, sim) %>%
#   arrange(val, .by_group = TRUE) %>%
#   mutate(fake.id = 1:n()) %>%
#   ungroup() %>%
#   group_by(scenario, fake.id) %>%
#   summarize(val.avg = mean(val),
# 	    est.avg = mean(est)) %>%
#   ungroup()
# avg.2.mixed.probit.df$type <- 'Probit'
# save(avg.df.probit, avg.2.mixed.probit.df, file = 'results/mixed-probit-bias-summary.rda')

load('results/mixed-probit-bias-summary.rda')

# Calculate coverage ------------------
# J <- nrow(psi.high.samples)
# n.time <- ncol(psi.high.samples)
# n.sims <- dim(psi.high.samples)[3]
# psi.covered.sv <- array(NA, dim = dim(psi.low.samples))
# for (i in 1:n.sims) { # simulations
#   print(i)
#   for (k in 1:n.time) { # time
#     for (j in 1:n.scenarios) { # scenarios
#       psi.covered.sv[, k, i, j] <- ifelse((psi.true[, k, i, j] >
#           				   psi.low.samples[, k, i, j]) &
#           			          (psi.true[, k, i, j] <
#           			           psi.high.samples[, k, i, j]),
#           		                   1, 0)
#     } # j
#   } # k
# } # i
# psi.coverage.sv <- apply(psi.covered.sv, c(3, 4), mean)
# coverage.df[2, 2] <- mean(psi.coverage.sv)
# rm(psi.high.samples, psi.true, psi.mean.samples, psi.low.samples)
# gc()

# Mixed scale-low results ----------------------------------------------
# load("results/sim-mixed-scale-low-results.rda")
# 
# plot.df <- as.data.frame.table(psi.true)
# colnames(plot.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# psi.mean.df <- as.data.frame.table(psi.mean.samples)
# colnames(psi.mean.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# plot.df$est <- psi.mean.df$val
# plot.df$diff <- plot.df$val - plot.df$est
# avg.df.scale.low <- plot.df %>%
#   group_by(scenario) %>%
#   summarize(diff.avg = mean(abs(diff)), 
# 	    diff.quant.low = quantile(abs(diff), 0.025),
# 	    diff.quant.high = quantile(abs(diff), 0.975))
# avg.df.scale.low$type <- 'Scale Low'
# avg.2.mixed.scale.low.df <- plot.df %>%
#   group_by(scenario, sim) %>%
#   arrange(val, .by_group = TRUE) %>%
#   mutate(fake.id = 1:n()) %>%
#   ungroup() %>%
#   group_by(scenario, fake.id) %>%
#   summarize(val.avg = mean(val),
# 	    est.avg = mean(est)) %>%
#   ungroup()
# avg.2.mixed.scale.low.df$type <- 'Scale Low'
# save(avg.df.scale.low, avg.2.mixed.scale.low.df, file = 'results/mixed-scale-low-bias-summary.rda')

load('results/mixed-scale-low-bias-summary.rda')

# Calculate coverage ------------------
# J <- nrow(psi.high.samples)
# n.time <- ncol(psi.high.samples)
# n.sims <- dim(psi.high.samples)[3]
# psi.covered.sv <- array(NA, dim = dim(psi.low.samples))
# for (i in 1:n.sims) { # simulations
#   print(i)
#   for (k in 1:n.time) { # time
#     for (j in 1:n.scenarios) { # scenarios
#       psi.covered.sv[, k, i, j] <- ifelse((psi.true[, k, i, j] >
#           				   psi.low.samples[, k, i, j]) &
#           			          (psi.true[, k, i, j] <
#           			           psi.high.samples[, k, i, j]),
#           		                   1, 0)
#     } # j
#   } # k
# } # i
# psi.coverage.sv <- apply(psi.covered.sv, c(3, 4), mean)
# coverage.df[2, 4] <- mean(psi.coverage.sv)
# rm(psi.high.samples, psi.true, psi.mean.samples, psi.low.samples)
# gc()

# Mixed scale-high results ----------------------------------------------
# load("results/sim-mixed-scale-high-results.rda")
# 
# plot.df <- as.data.frame.table(psi.true)
# colnames(plot.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# psi.mean.df <- as.data.frame.table(psi.mean.samples)
# colnames(psi.mean.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# plot.df$est <- psi.mean.df$val
# plot.df$diff <- plot.df$val - plot.df$est
# 
# avg.df.scale.high <- plot.df %>%
#   group_by(scenario) %>%
#   summarize(diff.avg = mean(abs(diff)), 
# 	    diff.quant.low = quantile(abs(diff), 0.025),
# 	    diff.quant.high = quantile(abs(diff), 0.975))
# avg.df.scale.high$type <- 'Scale High'
# avg.2.mixed.scale.high.df <- plot.df %>%
#   group_by(scenario, sim) %>%
#   arrange(val, .by_group = TRUE) %>%
#   mutate(fake.id = 1:n()) %>%
#   ungroup() %>%
#   group_by(scenario, fake.id) %>%
#   summarize(val.avg = mean(val),
# 	    est.avg = mean(est)) %>%
#   ungroup()
# avg.2.mixed.scale.high.df$type <- 'Scale High'
# save(avg.df.scale.high, avg.2.mixed.scale.high.df, file = 'results/mixed-scale-high-bias-summary.rda')

load('results/mixed-scale-high-bias-summary.rda')

# Calculate coverage ------------------
# J <- nrow(psi.high.samples)
# n.time <- ncol(psi.high.samples)
# n.sims <- dim(psi.high.samples)[3]
# psi.covered.sv <- array(NA, dim = dim(psi.low.samples))
# for (i in 1:n.sims) { # simulations
#   print(i)
#   for (k in 1:n.time) { # time
#     for (j in 1:n.scenarios) { # scenarios
#       psi.covered.sv[, k, i, j] <- ifelse((psi.true[, k, i, j] >
#           				   psi.low.samples[, k, i, j]) &
#           			          (psi.true[, k, i, j] <
#           			           psi.high.samples[, k, i, j]),
#           		                   1, 0)
#     } # j
#   } # k
# } # i
# psi.coverage.sv <- apply(psi.covered.sv, c(3, 4), mean)
# coverage.df[2, 3] <- mean(psi.coverage.sv)
# rm(psi.high.samples, psi.true, psi.mean.samples, psi.low.samples)
# gc()

# Mixed line results ----------------------------------------------
# load("results/sim-mixed-line-results.rda")
# 
# plot.df <- as.data.frame.table(psi.true)
# colnames(plot.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# psi.mean.df <- as.data.frame.table(psi.mean.samples)
# colnames(psi.mean.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# plot.df$est <- psi.mean.df$val
# plot.df$diff <- plot.df$val - plot.df$est
# avg.df.line <- plot.df %>%
#   group_by(scenario) %>%
#   summarize(diff.avg = mean(abs(diff)), 
# 	    diff.quant.low = quantile(abs(diff), 0.025),
# 	    diff.quant.high = quantile(abs(diff), 0.975))
# avg.df.line$type <- 'Linear'
# avg.2.mixed.line.df <- plot.df %>%
#   group_by(scenario, sim) %>%
#   arrange(val, .by_group = TRUE) %>%
#   mutate(fake.id = 1:n()) %>%
#   ungroup() %>%
#   group_by(scenario, fake.id) %>%
#   summarize(val.avg = mean(val),
# 	    est.avg = mean(est)) %>%
#   ungroup()
# avg.2.mixed.line.df$type <- 'Linear'
# save(avg.df.line, avg.2.mixed.line.df, file = 'results/mixed-line-bias-summary.rda')

load('results/mixed-line-bias-summary.rda')

# Calculate coverage ------------------
# J <- nrow(psi.high.samples)
# n.time <- ncol(psi.high.samples)
# n.sims <- dim(psi.high.samples)[3]
# psi.covered.sv <- array(NA, dim = dim(psi.low.samples))
# for (i in 1:n.sims) { # simulations
#   print(i)
#   for (k in 1:n.time) { # time
#     for (j in 1:n.scenarios) { # scenarios
#       psi.covered.sv[, k, i, j] <- ifelse((psi.true[, k, i, j] >
#           				   psi.low.samples[, k, i, j]) &
#           			          (psi.true[, k, i, j] <
#           			           psi.high.samples[, k, i, j]),
#           		                   1, 0)
#     } # j
#   } # k
# } # i
# psi.coverage.sv <- apply(psi.covered.sv, c(3, 4), mean)
# coverage.df[2, 5] <- mean(psi.coverage.sv)
# rm(psi.high.samples, psi.true, psi.mean.samples, psi.low.samples)
# gc()

# Generate plot -----------------------------------------------------------
avg.df.full <- rbind(avg.df.mixed, avg.df.probit, avg.df.scale.low, 
		     avg.df.scale.high, avg.df.line)
avg.df.full$type <- ifelse(avg.df.full$type == 'Logit', 'Logit (correct model)', avg.df.full$type)
avg.df.full$type <- factor(avg.df.full$type, levels = c('Logit (correct model)', 'Probit', 
							'Scale High', 'Scale Low', 
							'Linear'), 
			   ordered = TRUE)
avg.df.full$scenario <- as.numeric(avg.df.full$scenario)
avg.df.full$spatial <- ifelse(avg.df.full$scenario %in% c(1, 5, 9, 13), 'A', 
			      ifelse(avg.df.full$scenario %in% c(2, 6, 10, 14), 'B', 
				     ifelse(avg.df.full$scenario %in% c(3, 7, 11, 15), 'C', 'D')))
avg.df.full$time <- ifelse(avg.df.full$scenario %in% c(1, 2, 3, 4), 'A', 
			      ifelse(avg.df.full$scenario %in% c(5, 6, 7, 8), 'B', 
				     ifelse(avg.df.full$scenario %in% c(9, 10, 11, 12), 'C', 'D')))
spatial.labs <- c(expression(paste(sigma, " "^2, " Low, ", phi, "  High")), 
                  expression(paste(sigma, " "^2, " High, ", phi, "  High")),
                  expression(paste(sigma, " "^2, " Low, ", phi, "  Low")),
                  expression(paste(sigma, " "^2, " High, ", phi, "  Low")))
avg.df.full$spatial <- factor(avg.df.full$spatial, levels = c('A', 'B', 'C', 'D'), 
			 labels = spatial.labs)
time.labs <- c(expression(paste(sigma, " "[T]^2, " Low, ", rho, "  Low")), 
               expression(paste(sigma, " "[T]^2, " Low, ", rho, "  High")), 
               expression(paste(sigma, " "[T]^2, " High, ", rho, "  Low")), 
               expression(paste(sigma, " "[T]^2, " High, ", rho, "  High"))) 
avg.df.full$time <- factor(avg.df.full$time, levels = c('A', 'B', 'C', 'D'), 
		      labels = time.labs)
# Generate Supplemental Figure S6
bias.plot <- ggplot(data = avg.df.full, aes(x = type, y = diff.avg, col = type)) +
  scale_color_colorblind() +
  theme_light(base_size = 12) +
  facet_grid(time ~ spatial, 
	     labeller = label_parsed) +
  geom_segment(aes(x = type, y = diff.quant.low, xend = type, yend = diff.quant.high), 
	       lineend = 'butt', linewidth = 0.7) +
  geom_point(size = 3) +
  scale_y_continuous(limits = c(0, 0.66)) +
  labs(x = 'Link Function', y = 'Absolute Bias', 
       col = '') + 
  theme(legend.position = 'bottom', 
        strip.text.y = element_text(color = 'black'),
        strip.text.x = element_text(color = 'black'), 
        text = element_text(family="LM Roman 10"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1, size = 10), 
        axis.text.y = element_text(size = 10))
bias.plot
ggsave(file = 'figures/Figure-S6.png', device = 'png', width = 8.5, height = 7)

# Generate second plot ----------------------------------------------------
avg.df.2.full <- rbind(avg.2.mixed.df, avg.2.mixed.probit.df, avg.2.mixed.scale.low.df, 
		     avg.2.mixed.scale.high.df, avg.2.mixed.line.df)
avg.df.2.full$type <- ifelse(avg.df.2.full$type == 'Logit', 'Logit (correct model)', 
			     avg.df.2.full$type)
avg.df.2.full$type <- factor(avg.df.2.full$type, levels = c('Logit (correct model)', 'Probit', 
							'Scale High', 'Scale Low', 
							'Linear'), 
			   ordered = TRUE)
avg.df.2.full$scenario <- as.numeric(avg.df.2.full$scenario)
avg.df.2.full$spatial <- ifelse(avg.df.2.full$scenario %in% c(1, 5, 9, 13), 'A', 
			      ifelse(avg.df.2.full$scenario %in% c(2, 6, 10, 14), 'B', 
				     ifelse(avg.df.2.full$scenario %in% c(3, 7, 11, 15), 'C', 'D')))
avg.df.2.full$time <- ifelse(avg.df.2.full$scenario %in% c(1, 2, 3, 4), 'A', 
			      ifelse(avg.df.2.full$scenario %in% c(5, 6, 7, 8), 'B', 
				     ifelse(avg.df.2.full$scenario %in% c(9, 10, 11, 12), 'C', 'D')))
avg.df.2.full$spatial <- factor(avg.df.2.full$spatial, levels = c('A', 'B', 'C', 'D'), 
			 labels = spatial.labs)
avg.df.2.full$time <- factor(avg.df.2.full$time, levels = c('A', 'B', 'C', 'D'), 
		      labels = time.labs)
# Generate Figure 5
bias.plot.2 <- ggplot(data = avg.df.2.full) +
  theme_light(base_size = 14) +
  facet_grid(time ~ spatial, 
	     labeller = label_parsed) +
  geom_abline(slope = 1, intercept = 0, col = 'grey', lty = 2) +
  geom_smooth(aes(x = val.avg, y = est.avg, col = type, group = type), 
	      se = FALSE, lineend = 'round', lwd = 0.5) +
  scale_color_colorblind() +
  labs(x = 'True Occupancy Probability', y = 'Estimated Occupancy Probability', col = '') +
  theme(legend.position = 'bottom',
        strip.text.y = element_text(color = 'black'),
        strip.text.x = element_text(color = 'black'),
        text = element_text(family="LM Roman 10"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10))
bias.plot.2
ggsave(file = 'figures/Figure-5.png', device = 'png', width = 8.5, height = 7, 
       units = 'in')

# Double-visit results ----------------------------------------------------
# load("results/sim-dv-stPGOcc-results.rda")
# 
# plot.df <- as.data.frame.table(psi.true)
# colnames(plot.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# psi.mean.df <- as.data.frame.table(psi.mean.samples)
# colnames(psi.mean.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# plot.df$est <- psi.mean.df$val
# plot.df$diff <- plot.df$val - plot.df$est
# avg.df.dv <- plot.df %>%
#   group_by(scenario) %>%
#   summarize(diff.avg = mean(abs(diff)), 
# 	    diff.quant.low = quantile(abs(diff), 0.025),
# 	    diff.quant.high = quantile(abs(diff), 0.975))
# avg.df.dv$type <- 'Logit'
# avg.2.dv.df <- plot.df %>%
#   group_by(scenario, sim) %>%
#   arrange(val, .by_group = TRUE) %>%
#   mutate(fake.id = 1:n()) %>%
#   ungroup() %>%
#   group_by(scenario, fake.id) %>%
#   summarize(val.avg = mean(val),
# 	    est.avg = mean(est)) %>%
#   ungroup()
# avg.2.dv.df$type <- 'Logit'
# save(avg.df.dv, avg.2.dv.df, file = 'results/dv-bias-summary.rda')

load("results/dv-bias-summary.rda")

# Calculate coverage ------------------
# J <- nrow(psi.high.samples)
# n.time <- ncol(psi.high.samples)
# n.sims <- dim(psi.high.samples)[3]
# psi.covered.sv <- array(NA, dim = dim(psi.low.samples))
# for (i in 1:n.sims) { # simulations
#   print(i)
#   for (k in 1:n.time) { # time
#     for (j in 1:n.scenarios) { # scenarios
#       psi.covered.sv[, k, i, j] <- ifelse((psi.true[, k, i, j] >
#           				   psi.low.samples[, k, i, j]) &
#           			          (psi.true[, k, i, j] <
#           			           psi.high.samples[, k, i, j]),
#           		                   1, 0)
#     } # j
#   } # k
# } # i
# psi.coverage.sv <- apply(psi.covered.sv, c(3, 4), mean)
# coverage.df[3, 1] <- mean(psi.coverage.sv)
# rm(psi.high.samples, psi.true, psi.mean.samples, psi.low.samples)
# gc()

# Double-visit probit results ----------------------------------------------
# load("results/sim-dv-probit-results.rda")
# 
# plot.df <- as.data.frame.table(psi.true)
# colnames(plot.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# psi.mean.df <- as.data.frame.table(psi.mean.samples)
# colnames(psi.mean.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# plot.df$est <- psi.mean.df$val
# plot.df$diff <- plot.df$val - plot.df$est
# avg.df.probit <- plot.df %>%
#   group_by(scenario) %>%
#   summarize(diff.avg = mean(abs(diff)), 
# 	    diff.quant.low = quantile(abs(diff), 0.025),
# 	    diff.quant.high = quantile(abs(diff), 0.975))
# avg.df.probit$type <- 'Probit'
# # save(avg.df.probit, file = 'results/dv-probit-bias-df.rda')
# # load('results/dv-probit-bias-df.rda')
# avg.2.dv.probit.df <- plot.df %>%
#   group_by(scenario, sim) %>%
#   arrange(val, .by_group = TRUE) %>%
#   mutate(fake.id = 1:n()) %>%
#   ungroup() %>%
#   group_by(scenario, fake.id) %>%
#   summarize(val.avg = mean(val),
# 	    est.avg = mean(est)) %>%
#   ungroup()
# avg.2.dv.probit.df$type <- 'Probit'
# save(avg.df.probit, avg.2.dv.probit.df, file = 'results/dv-probit-bias-summary.rda')

load("results/dv-probit-bias-summary.rda")

# Calculate coverage ------------------
# J <- nrow(psi.high.samples)
# n.time <- ncol(psi.high.samples)
# n.sims <- dim(psi.high.samples)[3]
# psi.covered.sv <- array(NA, dim = dim(psi.low.samples))
# for (i in 1:n.sims) { # simulations
#   print(i)
#   for (k in 1:n.time) { # time
#     for (j in 1:n.scenarios) { # scenarios
#       psi.covered.sv[, k, i, j] <- ifelse((psi.true[, k, i, j] >
#           				   psi.low.samples[, k, i, j]) &
#           			          (psi.true[, k, i, j] <
#           			           psi.high.samples[, k, i, j]),
#           		                   1, 0)
#     } # j
#   } # k
# } # i
# psi.coverage.sv <- apply(psi.covered.sv, c(3, 4), mean)
# coverage.df[3, 2] <- mean(psi.coverage.sv)
# rm(psi.high.samples, psi.true, psi.mean.samples, psi.low.samples)
# gc()

# Double-visit scale-low results ----------------------------------------------
# load("results/sim-dv-scale-low-results.rda")
# 
# plot.df <- as.data.frame.table(psi.true)
# colnames(plot.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# psi.mean.df <- as.data.frame.table(psi.mean.samples)
# colnames(psi.mean.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# plot.df$est <- psi.mean.df$val
# plot.df$diff <- plot.df$val - plot.df$est
# avg.df.scale.low <- plot.df %>%
#   group_by(scenario) %>%
#   summarize(diff.avg = mean(abs(diff)), 
# 	    diff.quant.low = quantile(abs(diff), 0.025),
# 	    diff.quant.high = quantile(abs(diff), 0.975))
# avg.df.scale.low$type <- 'Scale Low'
# # load('results/dv-scale-low-bias-df.rda')
# avg.2.dv.scale.low.df <- plot.df %>%
#   group_by(scenario, sim) %>%
#   arrange(val, .by_group = TRUE) %>%
#   mutate(fake.id = 1:n()) %>%
#   ungroup() %>%
#   group_by(scenario, fake.id) %>%
#   summarize(val.avg = mean(val),
# 	    est.avg = mean(est)) %>%
#   ungroup()
# avg.2.dv.scale.low.df$type <- 'Scale Low'
# save(avg.df.scale.low, avg.2.dv.scale.low.df, file = 'results/dv-scale-low-bias-summary.rda')

load("results/dv-scale-low-bias-summary.rda")

# Calculate coverage ------------------
# J <- nrow(psi.high.samples)
# n.time <- ncol(psi.high.samples)
# n.sims <- dim(psi.high.samples)[3]
# psi.covered.sv <- array(NA, dim = dim(psi.low.samples))
# for (i in 1:n.sims) { # simulations
#   print(i)
#   for (k in 1:n.time) { # time
#     for (j in 1:n.scenarios) { # scenarios
#       psi.covered.sv[, k, i, j] <- ifelse((psi.true[, k, i, j] >
#           				   psi.low.samples[, k, i, j]) &
#           			          (psi.true[, k, i, j] <
#           			           psi.high.samples[, k, i, j]),
#           		                   1, 0)
#     } # j
#   } # k
# } # i
# psi.coverage.sv <- apply(psi.covered.sv, c(3, 4), mean)
# coverage.df[3, 4] <- mean(psi.coverage.sv)
# rm(psi.high.samples, psi.true, psi.mean.samples, psi.low.samples)
# gc()

# Double-visit scale-high results ----------------------------------------------
# load("results/sim-dv-scale-high-results.rda")
# 
# plot.df <- as.data.frame.table(psi.true)
# colnames(plot.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# psi.mean.df <- as.data.frame.table(psi.mean.samples)
# colnames(psi.mean.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# plot.df$est <- psi.mean.df$val
# plot.df$diff <- plot.df$val - plot.df$est
# 
# avg.df.scale.high <- plot.df %>%
#   group_by(scenario) %>%
#   summarize(diff.avg = mean(abs(diff)), 
# 	    diff.quant.low = quantile(abs(diff), 0.025),
# 	    diff.quant.high = quantile(abs(diff), 0.975))
# avg.df.scale.high$type <- 'Scale High'
# # save(avg.df.scale.high, file = 'results/dv-scale-high-bias-df.rda')
# # load('results/dv-scale-high-bias-df.rda')
# avg.2.dv.scale.high.df <- plot.df %>%
#   group_by(scenario, sim) %>%
#   arrange(val, .by_group = TRUE) %>%
#   mutate(fake.id = 1:n()) %>%
#   ungroup() %>%
#   group_by(scenario, fake.id) %>%
#   summarize(val.avg = mean(val),
# 	    est.avg = mean(est)) %>%
#   ungroup()
# avg.2.dv.scale.high.df$type <- 'Scale High'
# save(avg.df.scale.high, avg.2.dv.scale.high.df, file = 'results/dv-scale-high-bias-summary.rda')

load("results/dv-scale-high-bias-summary.rda")

# Calculate coverage ------------------
# J <- nrow(psi.high.samples)
# n.time <- ncol(psi.high.samples)
# n.sims <- dim(psi.high.samples)[3]
# psi.covered.sv <- array(NA, dim = dim(psi.low.samples))
# for (i in 1:n.sims) { # simulations
#   print(i)
#   for (k in 1:n.time) { # time
#     for (j in 1:n.scenarios) { # scenarios
#       psi.covered.sv[, k, i, j] <- ifelse((psi.true[, k, i, j] >
#           				   psi.low.samples[, k, i, j]) &
#           			          (psi.true[, k, i, j] <
#           			           psi.high.samples[, k, i, j]),
#           		                   1, 0)
#     } # j
#   } # k
# } # i
# psi.coverage.sv <- apply(psi.covered.sv, c(3, 4), mean)
# coverage.df[3, 3] <- mean(psi.coverage.sv)
# rm(psi.high.samples, psi.true, psi.mean.samples, psi.low.samples)
# gc()

# Double-visit line results ----------------------------------------------
# load("results/sim-dv-line-results.rda")
# 
# plot.df <- as.data.frame.table(psi.true)
# colnames(plot.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# psi.mean.df <- as.data.frame.table(psi.mean.samples)
# colnames(psi.mean.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# plot.df$est <- psi.mean.df$val
# plot.df$diff <- plot.df$val - plot.df$est
# avg.df.line <- plot.df %>%
#   group_by(scenario) %>%
#   summarize(diff.avg = mean(abs(diff)), 
# 	    diff.quant.low = quantile(abs(diff), 0.025),
# 	    diff.quant.high = quantile(abs(diff), 0.975))
# avg.df.line$type <- 'Linear'
# # save(avg.df.line, file = 'results/dv-line-bias-df.rda')
# # load('results/dv-line-bias-df.rda')
# avg.2.dv.line.df <- plot.df %>%
#   group_by(scenario, sim) %>%
#   arrange(val, .by_group = TRUE) %>%
#   mutate(fake.id = 1:n()) %>%
#   ungroup() %>%
#   group_by(scenario, fake.id) %>%
#   summarize(val.avg = mean(val),
# 	    est.avg = mean(est)) %>%
#   ungroup()
# avg.2.dv.line.df$type <- 'Linear'
# save(avg.df.line, avg.2.dv.line.df, file = 'results/dv-line-bias-summary.rda')

load("results/dv-line-bias-summary.rda")

# Calculate coverage ------------------
# J <- nrow(psi.high.samples)
# n.time <- ncol(psi.high.samples)
# n.sims <- dim(psi.high.samples)[3]
# psi.covered.sv <- array(NA, dim = dim(psi.low.samples))
# for (i in 1:n.sims) { # simulations
#   print(i)
#   for (k in 1:n.time) { # time
#     for (j in 1:n.scenarios) { # scenarios
#       psi.covered.sv[, k, i, j] <- ifelse((psi.true[, k, i, j] >
#           				   psi.low.samples[, k, i, j]) &
#           			          (psi.true[, k, i, j] <
#           			           psi.high.samples[, k, i, j]),
#           		                   1, 0)
#     } # j
#   } # k
# } # i
# psi.coverage.sv <- apply(psi.covered.sv, c(3, 4), mean)
# coverage.df[3, 5] <- mean(psi.coverage.sv)
# rm(psi.high.samples, psi.true, psi.mean.samples, psi.low.samples)
# gc()

# Generate plot -----------------------------------------------------------
avg.df.full <- rbind(avg.df.dv, avg.df.probit, avg.df.scale.low, 
		     avg.df.scale.high, avg.df.line)
avg.df.full$type <- ifelse(avg.df.full$type == 'Logit', 'Logit (correct model)', avg.df.full$type)
avg.df.full$type <- factor(avg.df.full$type, levels = c('Logit (correct model)', 'Probit', 
							'Scale High', 'Scale Low', 
							'Linear'), 
			   ordered = TRUE)
avg.df.full$scenario <- as.numeric(avg.df.full$scenario)
avg.df.full$spatial <- ifelse(avg.df.full$scenario %in% c(1, 5, 9, 13), 'A', 
			      ifelse(avg.df.full$scenario %in% c(2, 6, 10, 14), 'B', 
				     ifelse(avg.df.full$scenario %in% c(3, 7, 11, 15), 'C', 'D')))
avg.df.full$time <- ifelse(avg.df.full$scenario %in% c(1, 2, 3, 4), 'A', 
			      ifelse(avg.df.full$scenario %in% c(5, 6, 7, 8), 'B', 
				     ifelse(avg.df.full$scenario %in% c(9, 10, 11, 12), 'C', 'D')))
spatial.labs <- c(expression(paste(sigma, " "^2, " Low, ", phi, "  High")), 
                  expression(paste(sigma, " "^2, " High, ", phi, "  High")),
                  expression(paste(sigma, " "^2, " Low, ", phi, "  Low")),
                  expression(paste(sigma, " "^2, " High, ", phi, "  Low")))
avg.df.full$spatial <- factor(avg.df.full$spatial, levels = c('A', 'B', 'C', 'D'), 
			 labels = spatial.labs)
time.labs <- c(expression(paste(sigma, " "[T]^2, " Low, ", rho, "  Low")), 
               expression(paste(sigma, " "[T]^2, " Low, ", rho, "  High")), 
               expression(paste(sigma, " "[T]^2, " High, ", rho, "  Low")), 
               expression(paste(sigma, " "[T]^2, " High, ", rho, "  High"))) 
avg.df.full$time <- factor(avg.df.full$time, levels = c('A', 'B', 'C', 'D'), 
		      labels = time.labs)
# Supplemental Figure S7
bias.plot <- ggplot(data = avg.df.full, aes(x = type, y = diff.avg, col = type)) +
  scale_color_colorblind() +
  theme_light(base_size = 12) +
  facet_grid(time ~ spatial, 
	     labeller = label_parsed) +
  geom_segment(aes(x = type, y = diff.quant.low, xend = type, yend = diff.quant.high), 
	       lineend = 'butt', linewidth = 0.7) +
  geom_point(size = 3) +
  labs(x = 'Link Function', y = 'Absolute Bias', 
       col = '') + 
  scale_y_continuous(limits = c(0, 0.66)) +
  theme(legend.position = 'bottom', 
        strip.text.y = element_text(color = 'black'),
        strip.text.x = element_text(color = 'black'), 
        text = element_text(family="LM Roman 10"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1, size = 10), 
        axis.text.y = element_text(size = 10))
bias.plot
ggsave(file = 'figures/Figure-S7.png', device = 'png', width = 8.5, height = 7)

# Generate second plot ----------------------------------------------------
avg.df.2.full <- rbind(avg.2.dv.df, avg.2.dv.probit.df, avg.2.dv.scale.low.df,
		     avg.2.dv.scale.high.df, avg.2.dv.line.df)
avg.df.2.full$type <- ifelse(avg.df.2.full$type == 'Logit', 'Logit (correct model)', 
			     avg.df.2.full$type)
avg.df.2.full$type <- factor(avg.df.2.full$type, levels = c('Logit (correct model)', 'Probit', 
							'Scale High', 'Scale Low', 
							'Linear'), 
			   ordered = TRUE)
avg.df.2.full$scenario <- as.numeric(avg.df.2.full$scenario)
avg.df.2.full$spatial <- ifelse(avg.df.2.full$scenario %in% c(1, 5, 9, 13), 'A',
			      ifelse(avg.df.2.full$scenario %in% c(2, 6, 10, 14), 'B',
				     ifelse(avg.df.2.full$scenario %in% c(3, 7, 11, 15), 'C', 'D')))
avg.df.2.full$time <- ifelse(avg.df.2.full$scenario %in% c(1, 2, 3, 4), 'A',
			      ifelse(avg.df.2.full$scenario %in% c(5, 6, 7, 8), 'B',
				     ifelse(avg.df.2.full$scenario %in% c(9, 10, 11, 12), 'C', 'D')))
avg.df.2.full$spatial <- factor(avg.df.2.full$spatial, levels = c('A', 'B', 'C', 'D'),
			 labels = spatial.labs)
avg.df.2.full$time <- factor(avg.df.2.full$time, levels = c('A', 'B', 'C', 'D'),
		      labels = time.labs)
# Figure S1
bias.plot.2 <- ggplot(data = avg.df.2.full) +
  theme_light(base_size = 14) +
  facet_grid(time ~ spatial,
	     labeller = label_parsed) +
  geom_abline(slope = 1, intercept = 0, col = 'grey', lty = 2) +
  geom_smooth(aes(x = val.avg, y = est.avg, col = type, group = type),
	      se = FALSE, lineend = 'round', lwd = 0.5) +
  scale_color_colorblind() +
  labs(x = 'True Occupancy Probability', y = 'Estimated Occupancy Probability', col = '') +
  theme(legend.position = 'bottom',
        strip.text.y = element_text(color = 'black'),
        strip.text.x = element_text(color = 'black'),
        text = element_text(family="LM Roman 10"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10))
bias.plot.2
ggsave(file = 'figures/Figure-S1.png', device = 'png', width = 8.5, height = 7,
       units = 'in')

# Five-visit results ------------------------------------------------------
# load("results/sim-five-visits-stPGOcc-results.rda")
# 
# plot.df <- as.data.frame.table(psi.true)
# colnames(plot.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# psi.mean.df <- as.data.frame.table(psi.mean.samples)
# colnames(psi.mean.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# plot.df$est <- psi.mean.df$val
# plot.df$diff <- plot.df$val - plot.df$est
# avg.df.fv <- plot.df %>%
#   group_by(scenario) %>%
#   summarize(diff.avg = mean(abs(diff)), 
# 	    diff.quant.low = quantile(abs(diff), 0.025),
# 	    diff.quant.high = quantile(abs(diff), 0.975))
# avg.df.fv$type <- 'Logit'
# # save(avg.df.fv, file = 'results/fv-bias-df.rda')
# # load('results/fv-bias-df.rda')
# avg.2.fv.df <- plot.df %>%
#   group_by(scenario, sim) %>%
#   arrange(val, .by_group = TRUE) %>%
#   mutate(fake.id = 1:n()) %>%
#   ungroup() %>%
#   group_by(scenario, fake.id) %>%
#   summarize(val.avg = mean(val),
# 	    est.avg = mean(est)) %>%
#   ungroup()
# avg.2.fv.df$type <- 'Logit'
# save(avg.df.fv, avg.2.fv.df, file = 'results/fv-bias-summary.rda')

load("results/fv-bias-summary.rda")
# Calculate coverage ------------------
# J <- nrow(psi.high.samples)
# n.time <- ncol(psi.high.samples)
# n.sims <- dim(psi.high.samples)[3]
# psi.covered.sv <- array(NA, dim = dim(psi.low.samples))
# for (i in 1:n.sims) { # simulations
#   print(i)
#   for (k in 1:n.time) { # time
#     for (j in 1:n.scenarios) { # scenarios
#       psi.covered.sv[, k, i, j] <- ifelse((psi.true[, k, i, j] >
#           				   psi.low.samples[, k, i, j]) &
#           			          (psi.true[, k, i, j] <
#           			           psi.high.samples[, k, i, j]),
#           		                   1, 0)
#     } # j
#   } # k
# } # i
# psi.coverage.sv <- apply(psi.covered.sv, c(3, 4), mean)
# coverage.df[4, 1] <- mean(psi.coverage.sv)
# rm(psi.high.samples, psi.true, psi.mean.samples, psi.low.samples)
# gc()

# Five-visit probit results ----------------------------------------------
# load("results/sim-five-visit-probit-results.rda")
# 
# plot.df <- as.data.frame.table(psi.true)
# colnames(plot.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# psi.mean.df <- as.data.frame.table(psi.mean.samples)
# colnames(psi.mean.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# plot.df$est <- psi.mean.df$val
# plot.df$diff <- plot.df$val - plot.df$est
# avg.df.probit <- plot.df %>%
#   group_by(scenario) %>%
#   summarize(diff.avg = mean(abs(diff)), 
# 	    diff.quant.low = quantile(abs(diff), 0.025),
# 	    diff.quant.high = quantile(abs(diff), 0.975))
# avg.df.probit$type <- 'Probit'
# # save(avg.df.probit, file = 'results/fv-probit-bias-df.rda')
# # load('results/fv-probit-bias-df.rda')
# avg.2.fv.probit.df <- plot.df %>%
#   group_by(scenario, sim) %>%
#   arrange(val, .by_group = TRUE) %>%
#   mutate(fake.id = 1:n()) %>%
#   ungroup() %>%
#   group_by(scenario, fake.id) %>%
#   summarize(val.avg = mean(val),
# 	    est.avg = mean(est)) %>%
#   ungroup()
# avg.2.fv.probit.df$type <- 'Probit'
# save(avg.df.probit, avg.2.fv.probit.df, file = 'results/fv-probit-bias-summary.rda')

load("results/fv-probit-bias-summary.rda")

# Calculate coverage ------------------
# J <- nrow(psi.high.samples)
# n.time <- ncol(psi.high.samples)
# n.sims <- dim(psi.high.samples)[3]
# psi.covered.sv <- array(NA, dim = dim(psi.low.samples))
# for (i in 1:n.sims) { # simulations
#   print(i)
#   for (k in 1:n.time) { # time
#     for (j in 1:n.scenarios) { # scenarios
#       psi.covered.sv[, k, i, j] <- ifelse((psi.true[, k, i, j] >
#           				   psi.low.samples[, k, i, j]) &
#           			          (psi.true[, k, i, j] <
#           			           psi.high.samples[, k, i, j]),
#           		                   1, 0)
#     } # j
#   } # k
# } # i
# psi.coverage.sv <- apply(psi.covered.sv, c(3, 4), mean)
# coverage.df[4, 2] <- mean(psi.coverage.sv)
# rm(psi.high.samples, psi.true, psi.mean.samples, psi.low.samples)
# gc()

# Five-visit scale-low results ----------------------------------------------
# load("results/sim-five-visits-scale-low-results.rda")
# 
# plot.df <- as.data.frame.table(psi.true)
# colnames(plot.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# psi.mean.df <- as.data.frame.table(psi.mean.samples)
# colnames(psi.mean.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# plot.df$est <- psi.mean.df$val
# plot.df$diff <- plot.df$val - plot.df$est
# avg.df.scale.low <- plot.df %>%
#   group_by(scenario) %>%
#   summarize(diff.avg = mean(abs(diff)), 
# 	    diff.quant.low = quantile(abs(diff), 0.025),
# 	    diff.quant.high = quantile(abs(diff), 0.975))
# avg.df.scale.low$type <- 'Scale Low'
# save(avg.df.scale.low, file = 'results/fv-scale-low-bias-df.rda')
# # load('results/fv-scale-low-bias-df.rda')
# avg.2.fv.scale.low.df <- plot.df %>%
#   group_by(scenario, sim) %>%
#   arrange(val, .by_group = TRUE) %>%
#   mutate(fake.id = 1:n()) %>%
#   ungroup() %>%
#   group_by(scenario, fake.id) %>%
#   summarize(val.avg = mean(val),
# 	    est.avg = mean(est)) %>%
#   ungroup()
# avg.2.fv.scale.low.df$type <- 'Scale Low'
# save(avg.df.scale.low, avg.2.fv.scale.low.df, file = 'results/fv-scale-low-bias-summary.rda')

load("results/fv-scale-low-bias-summary.rda")

# Calculate coverage ------------------
# J <- nrow(psi.high.samples)
# n.time <- ncol(psi.high.samples)
# n.sims <- dim(psi.high.samples)[3]
# psi.covered.sv <- array(NA, dim = dim(psi.low.samples))
# for (i in 1:n.sims) { # simulations
#   print(i)
#   for (k in 1:n.time) { # time
#     for (j in 1:n.scenarios) { # scenarios
#       psi.covered.sv[, k, i, j] <- ifelse((psi.true[, k, i, j] >
#           				   psi.low.samples[, k, i, j]) &
#           			          (psi.true[, k, i, j] <
#           			           psi.high.samples[, k, i, j]),
#           		                   1, 0)
#     } # j
#   } # k
# } # i
# psi.coverage.sv <- apply(psi.covered.sv, c(3, 4), mean)
# coverage.df[4, 4] <- mean(psi.coverage.sv)
# rm(psi.high.samples, psi.true, psi.mean.samples, psi.low.samples)
# gc()

# Five-visit scale-high results ----------------------------------------------
# load("results/sim-five-visits-scale-high-results.rda")
# 
# plot.df <- as.data.frame.table(psi.true)
# colnames(plot.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# psi.mean.df <- as.data.frame.table(psi.mean.samples)
# colnames(psi.mean.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# plot.df$est <- psi.mean.df$val
# plot.df$diff <- plot.df$val - plot.df$est
# 
# avg.df.scale.high <- plot.df %>%
#   group_by(scenario) %>%
#   summarize(diff.avg = mean(abs(diff)), 
# 	    diff.quant.low = quantile(abs(diff), 0.025),
# 	    diff.quant.high = quantile(abs(diff), 0.975))
# avg.df.scale.high$type <- 'Scale High'
# # save(avg.df.scale.high, file = 'results/fv-scale-high-bias-df.rda')
# # load('results/fv-scale-high-bias-df.rda')
# avg.2.fv.scale.high.df <- plot.df %>%
#   group_by(scenario, sim) %>%
#   arrange(val, .by_group = TRUE) %>%
#   mutate(fake.id = 1:n()) %>%
#   ungroup() %>%
#   group_by(scenario, fake.id) %>%
#   summarize(val.avg = mean(val),
# 	    est.avg = mean(est)) %>%
#   ungroup()
# avg.2.fv.scale.high.df$type <- 'Scale High'
# save(avg.df.scale.high, avg.2.fv.scale.high.df, file = 'results/fv-scale-high-bias-summary.rda')

load("results/fv-scale-high-bias-summary.rda")
# Calculate coverage ------------------
# J <- nrow(psi.high.samples)
# n.time <- ncol(psi.high.samples)
# n.sims <- dim(psi.high.samples)[3]
# psi.covered.sv <- array(NA, dim = dim(psi.low.samples))
# for (i in 1:n.sims) { # simulations
#   print(i)
#   for (k in 1:n.time) { # time
#     for (j in 1:n.scenarios) { # scenarios
#       psi.covered.sv[, k, i, j] <- ifelse((psi.true[, k, i, j] >
#           				   psi.low.samples[, k, i, j]) &
#           			          (psi.true[, k, i, j] <
#           			           psi.high.samples[, k, i, j]),
#           		                   1, 0)
#     } # j
#   } # k
# } # i
# psi.coverage.sv <- apply(psi.covered.sv, c(3, 4), mean)
# coverage.df[4, 3] <- mean(psi.coverage.sv)
# rm(psi.high.samples, psi.true, psi.mean.samples, psi.low.samples)
# gc()

# Five-visit line results ----------------------------------------------
# load("results/sim-five-visits-line-results.rda")
# 
# plot.df <- as.data.frame.table(psi.true)
# colnames(plot.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# psi.mean.df <- as.data.frame.table(psi.mean.samples)
# colnames(psi.mean.df) <- c('site', 'year', 'sim', 'scenario', 'val')
# plot.df$est <- psi.mean.df$val
# plot.df$diff <- plot.df$val - plot.df$est
# avg.df.line <- plot.df %>%
#   group_by(scenario) %>%
#   summarize(diff.avg = mean(abs(diff)), 
# 	    diff.quant.low = quantile(abs(diff), 0.025),
# 	    diff.quant.high = quantile(abs(diff), 0.975))
# avg.df.line$type <- 'Linear'
# avg.2.fv.line.df <- plot.df %>%
#   group_by(scenario, sim) %>%
#   arrange(val, .by_group = TRUE) %>%
#   mutate(fake.id = 1:n()) %>%
#   ungroup() %>%
#   group_by(scenario, fake.id) %>%
#   summarize(val.avg = mean(val),
# 	    est.avg = mean(est)) %>%
#   ungroup()
# avg.2.fv.line.df$type <- 'Linear'
# save(avg.df.line, avg.2.fv.line.df, file = 'results/fv-line-bias-summary.rda')

load("results/fv-line-bias-summary.rda")

# Calculate coverage ------------------
# J <- nrow(psi.high.samples)
# n.time <- ncol(psi.high.samples)
# n.sims <- dim(psi.high.samples)[3]
# psi.covered.sv <- array(NA, dim = dim(psi.low.samples))
# for (i in 1:n.sims) { # simulations
#   print(i)
#   for (k in 1:n.time) { # time
#     for (j in 1:n.scenarios) { # scenarios
#       psi.covered.sv[, k, i, j] <- ifelse((psi.true[, k, i, j] >
#           				   psi.low.samples[, k, i, j]) &
#           			          (psi.true[, k, i, j] <
#           			           psi.high.samples[, k, i, j]),
#           		                   1, 0)
#     } # j
#   } # k
# } # i
# psi.coverage.sv <- apply(psi.covered.sv, c(3, 4), mean)
# coverage.df[4, 5] <- mean(psi.coverage.sv)
# rm(psi.high.samples, psi.true, psi.mean.samples, psi.low.samples)
# gc()

# Generate plot -----------------------------------------------------------
avg.df.full <- rbind(avg.df.fv, avg.df.probit, avg.df.scale.low, 
		     avg.df.scale.high, avg.df.line)
avg.df.full$type <- ifelse(avg.df.full$type == 'Logit', 'Logit (correct model)', avg.df.full$type)
avg.df.full$type <- factor(avg.df.full$type, levels = c('Logit (correct model)', 'Probit', 
							'Scale High', 'Scale Low', 
							'Linear'), 
			   ordered = TRUE)
avg.df.full$scenario <- as.numeric(avg.df.full$scenario)
avg.df.full$spatial <- ifelse(avg.df.full$scenario %in% c(1, 5, 9, 13), 'A', 
			      ifelse(avg.df.full$scenario %in% c(2, 6, 10, 14), 'B', 
				     ifelse(avg.df.full$scenario %in% c(3, 7, 11, 15), 'C', 'D')))
avg.df.full$time <- ifelse(avg.df.full$scenario %in% c(1, 2, 3, 4), 'A', 
			      ifelse(avg.df.full$scenario %in% c(5, 6, 7, 8), 'B', 
				     ifelse(avg.df.full$scenario %in% c(9, 10, 11, 12), 'C', 'D')))
spatial.labs <- c(expression(paste(sigma, " "^2, " Low, ", phi, "  High")), 
                  expression(paste(sigma, " "^2, " High, ", phi, "  High")),
                  expression(paste(sigma, " "^2, " Low, ", phi, "  Low")),
                  expression(paste(sigma, " "^2, " High, ", phi, "  Low")))
avg.df.full$spatial <- factor(avg.df.full$spatial, levels = c('A', 'B', 'C', 'D'), 
			 labels = spatial.labs)
time.labs <- c(expression(paste(sigma, " "[T]^2, " Low, ", rho, "  Low")), 
               expression(paste(sigma, " "[T]^2, " Low, ", rho, "  High")), 
               expression(paste(sigma, " "[T]^2, " High, ", rho, "  Low")), 
               expression(paste(sigma, " "[T]^2, " High, ", rho, "  High"))) 
avg.df.full$time <- factor(avg.df.full$time, levels = c('A', 'B', 'C', 'D'), 
		      labels = time.labs)
# Supplemental Figure S6
bias.plot <- ggplot(data = avg.df.full, aes(x = type, y = diff.avg, col = type)) +
  scale_color_colorblind() +
  theme_light(base_size = 12) +
  facet_grid(time ~ spatial, 
	     labeller = label_parsed) +
  geom_segment(aes(x = type, y = diff.quant.low, xend = type, yend = diff.quant.high), 
	       lineend = 'butt', linewidth = 0.7) +
  geom_point(size = 3) +
  labs(x = 'Link Function', y = 'Absolute Bias', 
       col = '') + 
  scale_y_continuous(limits = c(0, 0.66)) +
  theme(legend.position = 'bottom', 
        strip.text.y = element_text(color = 'black'),
        strip.text.x = element_text(color = 'black'), 
        text = element_text(family="LM Roman 10"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1, size = 10), 
        axis.text.y = element_text(size = 10))
bias.plot
ggsave(file = 'figures/Figure-S8.png', device = 'png', width = 8.5, height = 7)

# Generate second plot ----------------------------------------------------
avg.df.2.full <- rbind(avg.2.fv.df, avg.2.fv.probit.df, avg.2.fv.scale.low.df,
		     avg.2.fv.scale.high.df, avg.2.fv.line.df)
avg.df.2.full$type <- ifelse(avg.df.2.full$type == 'Logit', 'Logit (correct model)', 
			     avg.df.2.full$type)
avg.df.2.full$type <- factor(avg.df.2.full$type, levels = c('Logit (correct model)', 'Probit', 
							'Scale High', 'Scale Low', 
							'Linear'), 
			   ordered = TRUE)
avg.df.2.full$scenario <- as.numeric(avg.df.2.full$scenario)
avg.df.2.full$spatial <- ifelse(avg.df.2.full$scenario %in% c(1, 5, 9, 13), 'A',
			      ifelse(avg.df.2.full$scenario %in% c(2, 6, 10, 14), 'B',
				     ifelse(avg.df.2.full$scenario %in% c(3, 7, 11, 15), 'C', 'D')))
avg.df.2.full$time <- ifelse(avg.df.2.full$scenario %in% c(1, 2, 3, 4), 'A',
			      ifelse(avg.df.2.full$scenario %in% c(5, 6, 7, 8), 'B',
				     ifelse(avg.df.2.full$scenario %in% c(9, 10, 11, 12), 'C', 'D')))
avg.df.2.full$spatial <- factor(avg.df.2.full$spatial, levels = c('A', 'B', 'C', 'D'),
			 labels = spatial.labs)
avg.df.2.full$time <- factor(avg.df.2.full$time, levels = c('A', 'B', 'C', 'D'),
		      labels = time.labs)
# Supplemental Figure S2
bias.plot.2 <- ggplot(data = avg.df.2.full) +
  theme_light(base_size = 14) +
  facet_grid(time ~ spatial,
	     labeller = label_parsed) +
  geom_abline(slope = 1, intercept = 0, col = 'grey', lty = 2) +
  geom_smooth(aes(x = val.avg, y = est.avg, col = type, group = type),
	      se = FALSE, lineend = 'round', lwd = 0.5) +
  scale_color_colorblind() +
  labs(x = 'True Occupancy Probability', y = 'Estimated Occupancy Probability', col = '') +
  theme(legend.position = 'bottom',
        strip.text.y = element_text(color = 'black'),
        strip.text.x = element_text(color = 'black'),
        text = element_text(family="LM Roman 10"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10))
bias.plot.2
ggsave(file = 'figures/Figure-S2.png', device = 'png', width = 8.5, height = 7,
       units = 'in')
