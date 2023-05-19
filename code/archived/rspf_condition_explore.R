#RSPF condition - Solymos & Lele 2016

# both occurrence and detection must follow this condition to be identifiable

## the function log(___) needs to be nonlinear and involve all components of the parameter vector

## derivative with respect to covariate of log(___) depends on all components of the parameter vector


z = seq(-2, 10, by = .25)
alpha <- c(1/14, 2/14, 3/14)
beta <- c(5/28, 10/28, 2/28)


## visually does it look nonlinear in the range of interest?
plot(z, log(beta[1] * z + alpha[1]), ylim=c(-3, 0))
## just look at part constrained to not get above 1
lines(z, log(beta[1] * z + alpha[1]))
for(i in 1:3){
  for(j in 1:3){
    lines(z, log(beta[i] * z + alpha[j] ))
  }
}


# d/dx log(\alpha*x + \beta) = \frac{\alpha}{\beta + \alpha*x}


## so is this case technically in the RSPF?

## model still mis-specified, but plausible 
