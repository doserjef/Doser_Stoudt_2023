# summary.R: this script summarizes results from the case study and 
#            generates the results shown in Figure 6 of the manuscript and
#            Table S1 in the supplemental material.
# Author: Jeffrey W. Doser
# Approximate run time: <1 min
rm(list = ls())
library(coda)
library(tidyverse)
library(ggpubr)
library(ggthemes)

# Load in data set --------------------------------------------------------
load("data/hbef-spOccupancy-data.rda")

# Load in results ---------------------------------------------------------
load("results/hbef-analysis-results.rda")
beta.quants.sv <- apply(beta.samples.sv, 2, quantile, c(0.025, 0.5, 0.975))
beta.quants.dv.20 <- apply(beta.samples.dv.20, 2, quantile, c(0.025, 0.5, 0.975))
beta.quants.dv.40 <- apply(beta.samples.dv.40, 2, quantile, c(0.025, 0.5, 0.975))
beta.quants.dv.60 <- apply(beta.samples.dv.60, 2, quantile, c(0.025, 0.5, 0.975))
beta.quants.dv.80 <- apply(beta.samples.dv.80, 2, quantile, c(0.025, 0.5, 0.975))
beta.quants.dv <- apply(beta.samples.dv, 2, quantile, c(0.025, 0.5, 0.975))
beta.quants.tv.20 <- apply(beta.samples.tv.20, 2, quantile, c(0.025, 0.5, 0.975))
beta.quants.tv.40 <- apply(beta.samples.tv.40, 2, quantile, c(0.025, 0.5, 0.975))
beta.quants.tv.60 <- apply(beta.samples.tv.60, 2, quantile, c(0.025, 0.5, 0.975))
beta.quants.tv.80 <- apply(beta.samples.tv.80, 2, quantile, c(0.025, 0.5, 0.975))
beta.quants.tv <- apply(beta.samples.tv, 2, quantile, c(0.025, 0.5, 0.975))
alpha.quants.sv <- apply(alpha.samples.sv, 2, quantile, c(0.025, 0.5, 0.975))
alpha.quants.dv.20 <- apply(alpha.samples.dv.20, 2, quantile, c(0.025, 0.5, 0.975))
alpha.quants.dv.40 <- apply(alpha.samples.dv.40, 2, quantile, c(0.025, 0.5, 0.975))
alpha.quants.dv.60 <- apply(alpha.samples.dv.60, 2, quantile, c(0.025, 0.5, 0.975))
alpha.quants.dv.80 <- apply(alpha.samples.dv.80, 2, quantile, c(0.025, 0.5, 0.975))
alpha.quants.dv <- apply(alpha.samples.dv, 2, quantile, c(0.025, 0.5, 0.975))
alpha.quants.tv.20 <- apply(alpha.samples.tv.20, 2, quantile, c(0.025, 0.5, 0.975))
alpha.quants.tv.40 <- apply(alpha.samples.tv.40, 2, quantile, c(0.025, 0.5, 0.975))
alpha.quants.tv.60 <- apply(alpha.samples.tv.60, 2, quantile, c(0.025, 0.5, 0.975))
alpha.quants.tv.80 <- apply(alpha.samples.tv.80, 2, quantile, c(0.025, 0.5, 0.975))
alpha.quants.tv <- apply(alpha.samples.tv, 2, quantile, c(0.025, 0.5, 0.975))
n.models <- 11

plot.df <- data.frame(med = c(beta.quants.sv[2, ], 
                              beta.quants.dv.20[2, ], 
                              beta.quants.dv.40[2, ], 
                              beta.quants.dv.60[2, ], 
                              beta.quants.dv.80[2, ], 
                              beta.quants.dv[2, ], 
                              beta.quants.tv.20[2, ], 
                              beta.quants.tv.40[2, ], 
                              beta.quants.tv.60[2, ], 
                              beta.quants.tv.80[2, ], 
                              beta.quants.tv[2, ]),
                      low = c(beta.quants.sv[1, ], 
                              beta.quants.dv.20[1, ], 			      
                              beta.quants.dv.40[1, ], 
                              beta.quants.dv.60[1, ], 
                              beta.quants.dv.80[1, ], 
                              beta.quants.dv[1, ], 
                              beta.quants.tv.20[1, ], 
                              beta.quants.tv.40[1, ], 
                              beta.quants.tv.60[1, ], 
                              beta.quants.tv.80[1, ], 
                              beta.quants.tv[1, ]),
                      high = c(beta.quants.sv[3, ], 
                               beta.quants.dv.20[3, ], 			      
                               beta.quants.dv.40[3, ], 
                               beta.quants.dv.60[3, ], 
                               beta.quants.dv.80[3, ], 
                               beta.quants.dv[3, ], 
                               beta.quants.tv.20[3, ], 
                               beta.quants.tv.40[3, ], 
                               beta.quants.tv.60[3, ], 
                               beta.quants.tv.80[3, ], 
                               beta.quants.tv[3, ]), 
                      param = factor(rep(c('Intercept', 'Elevation (Linear)', 
				    'Elevation (Quadratic)', 'Precipitation', 
				    'Maximum Temperature'), n.models), 
				     ordered = TRUE, 
				     levels = c('Intercept', 'Elevation (Linear)', 
						'Elevation (Quadratic)', 'Precipitation', 
						'Maximum Temperature')),
		      avg.visits = rep(mean.visits, each = ncol(beta.samples.sv)))

beta.plot <- ggplot(data = plot.df, aes(x = avg.visits, y = med, col = param)) + 
  theme_bw(base_size = 15) +
  geom_point(size = 2) + 
  geom_line(linewidth = 0.8) + 
  scale_color_colorblind() +
  labs(x = 'Average Number of Visits per Site/Year', 
       y = 'Estimate (Posterior Median)', col = 'Parameter', 
       title = '(A) Occupancy') +
  theme(text = element_text(family="LM Roman 10"), 
        legend.position = 'right')


plot.df <- data.frame(med = c(alpha.quants.sv[2, ], 
                              alpha.quants.dv.20[2, ], 
                              alpha.quants.dv.40[2, ], 
                              alpha.quants.dv.60[2, ], 
                              alpha.quants.dv.80[2, ], 
                              alpha.quants.dv[2, ], 
                              alpha.quants.tv.20[2, ], 
                              alpha.quants.tv.40[2, ], 
                              alpha.quants.tv.60[2, ], 
                              alpha.quants.tv.80[2, ], 
                              alpha.quants.tv[2, ]),
                      low = c(alpha.quants.sv[1, ], 
                              alpha.quants.dv.20[1, ], 			      
                              alpha.quants.dv.40[1, ], 
                              alpha.quants.dv.60[1, ], 
                              alpha.quants.dv.80[1, ], 
                              alpha.quants.dv[1, ], 
                              alpha.quants.tv.20[1, ], 
                              alpha.quants.tv.40[1, ], 
                              alpha.quants.tv.60[1, ], 
                              alpha.quants.tv.80[1, ], 
                              alpha.quants.tv[1, ]),
                      high = c(alpha.quants.sv[3, ], 
                               alpha.quants.dv.20[3, ], 			      
                               alpha.quants.dv.40[3, ], 
                               alpha.quants.dv.60[3, ], 
                               alpha.quants.dv.80[3, ], 
                               alpha.quants.dv[3, ], 
                               alpha.quants.tv.20[3, ], 
                               alpha.quants.tv.40[3, ], 
                               alpha.quants.tv.60[3, ], 
                               alpha.quants.tv.80[3, ], 
                               alpha.quants.tv[3, ]), 
                      param = factor(rep(c('Intercept', 'Day (Linear)', 
				    'Day (Quadratic)', 'Time of Day'), n.models), 
				     ordered = TRUE, 
				     levels = c('Intercept', 'Day (Linear)', 
						'Day (Quadratic)', 'Time of Day')),
		      avg.visits = rep(mean.visits, each = ncol(alpha.samples.sv)))

alpha.plot <- ggplot(data = plot.df, aes(x = avg.visits, y = med, col = param)) + 
  theme_bw(base_size = 15) +
  geom_point(size = 2) + 
  geom_line(linewidth = 0.8) + 
  scale_color_colorblind() +
  labs(x = 'Average Number of Visits per Site/Year', 
       y = 'Estimate (Posterior Median)', col = 'Parameter', 
       title = '(B) Detection') +
  theme(text = element_text(family="LM Roman 10"), 
	legend.position = 'right')
full.plot <- ggarrange(beta.plot, alpha.plot, ncol = 1)
ggsave(full.plot, file = 'figures/Figure-6.png', device = 'png', width = 10.5, height = 8,
       units = 'in')

