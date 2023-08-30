negloglik.mod2 <- function(x, nb.n, ng.n, nb.bbias, ng.bbias, nb.gbias, ng.gbias)
{ # x is a vector containing the parameter values
  pb.n = x[1]  # pb.n is the first entry of x
  pb.bbias = x[2] # pb.bbias is the second entry of x
  pb.gbias = x[3] # pb.gbias is the third entry of x
  loglik = nb.n*log(pb.n) + ng.n*log(1-pb.n) + # contributions from neutral fams
    nb.bbias*log(pb.bbias) + ng.bbias*log(1-pb.bbias) + # contrib from bbias 
    nb.gbias*log(pb.gbias) + ng.gbias*log(1-pb.gbias)   # contrib from gbias
  return(-loglik) # optim is a minimization function (so need to give is negative log likelihood)
}

# optim is a minimization function (so need to give is negative log likelihood)
optim(par=c(0.5, 0.5, 0.5), 
      negloglik.mod2,nb.n = 3161, ng.n = 3119, 
      nb.bbias = 1131, ng.bbias = 1164, 
      nb.gbias = 1124, ng.gbias = 973, 
      method="BFGS")
