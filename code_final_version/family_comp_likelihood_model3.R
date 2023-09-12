# Question 3: Do couples "wait" for a boy?

negloglik.mod3 <- function(x, nboy, ngirl, nstop.b1, nnostop.b1, nstop.notb1, nnostop.notb1)
{ # x is a vector containing initial guesses about the parameter values
  pb = x[1]     # pb is the first entry of x
  ps.b1 = x[2]  # ps.b1 is second entry of x
  ps.notb1 = x[3] # ps.notb1 is third entry of x
  
  loglik = nboy*log(pb) + ngirl*log(1-pb) +    # biology contribution
    nstop.b1*log(ps.b1) + nnostop.b1*log(1-ps.b1) +  # first boy contribution
    nstop.notb1*log(ps.notb1) + nnostop.notb1*log(1-ps.notb1) # other children
  return(-loglik)
}

optim(par=c(0.5, 0.5, 0.5), # initial "guesses" for parameters
      negloglik.mod3, # function to minimize
      nboy=5416, ngirl = 5256,  # data
      nstop.b1 = 1721, nnostop.b1 = 2265, # more data
      nstop.notb1 = 3905, nnostop.notb1=2781, # more data
      method="BFGS") # methods used - keep as BFGS unless you know something better
