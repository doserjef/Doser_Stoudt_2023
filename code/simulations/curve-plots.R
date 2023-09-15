# curve-plots.R: script that generates Figure 2 in the manuscript, which
#                shows the functional forms of the different link functions
#                used in the mis-specification scenarios
# Authors: Jeffrey W. Doser
rm(list = ls())
library(tidyverse)
library(ggthemes)
library(scales)

# Logit -------------------------------------------------------------------
beta <- c(0, 0.5)
n <- 10000
x <- seq(from = -6, to = 6, length.out = n)
psi.logit <- plogis(beta[1] + beta[2] * x)

# Probit ------------------------------------------------------------------
psi.probit <- pnorm(beta[1] + beta[2] * x)

# Scaled logistic low -----------------------------------------------------
alpha.s <- 0.5
psi.scale.low <- alpha.s * plogis(beta[1] + beta[2] * x)

# Scaled logistic high ----------------------------------------------------
alpha.s <- 0.8
psi.scale.high <- alpha.s * plogis(beta[1] + beta[2] * x)

# Linear ------------------------------------------------------------------
beta <- c(0.5, 0.1)
psi.linear <- beta[1] + beta[2] * x

my.labels <- c('Logit', 'Probit', 
	       expression(paste("Scale High (", alpha[s], " = 0.8)")), 
	       expression(paste("Scale Low (", alpha[s], " = 0.5)")), 
               'Linear')

plot.df <- data.frame(vals = c(psi.logit, psi.probit, psi.scale.high, 
			       psi.scale.low, psi.linear), 
		      type = factor(rep(c('Logit', 'Probit', 'Scale High', 
				   'Scale Low', 'Linear'), each = n),
				    levels = c('Logit', 'Probit', 'Scale High', 
					       'Scale Low', 'Linear'), 
				    labels = my.labels), 
                      x = rep(x, times = 5))
# Generate Figure 2
png('figures/Figure-2.png', width = 9, height = 5, units = 'in', res = 300)
ggplot(plot.df, aes(x = x, y = vals, col = type)) + 
  geom_line(linewidth = 0.8, lineend = 'round') + 
  theme_bw(base_size = 18) + 
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), 
		     labels = c(0, 0.2, 0.4, 0.6, 0.8, 1)) + 
  scale_color_colorblind(labels = label_parse(), breaks = levels(plot.df$type)) + 
  labs(x = 'Covariate', y = 'Occupancy Probability', color = 'Link') + 
  theme(text = element_text(family = 'LM Roman 10'), 
	legend.text.align = 0)
dev.off()
