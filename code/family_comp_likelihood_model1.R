# Model 1
library(ggplot2) # used for graph of logL


# Numerical approximation for MLE
mle.pb <- function(nboys, ngirls, nGrid)
{
  # nboys and ngirls are the number of boys and girls in the dataset, respectively
  # nGrid is the number of possible pb values to explore
  # this function uses the log likelihood
  pb = seq(0,1,length=nGrid)
  loglik.pb = nboys*log(pb) + ngirls*log(1-pb)
  whichpb = which.max(loglik.pb)
  mle = pb[whichpb]
  maxloglik = loglik.pb[whichpb]
  return(list(mle=mle,maxll=maxloglik))
}


# Running the function for our data
mle.pb(nboys = 5416, ngirls = 5256, nGrid = 1000)


######################
# Graphically 


lik.fun <- function(pb, nboys, ngirls)
{
  lik = pb^nboys * (1-pb)^ngirls
  return(lik)
}

loglik.fun <- function(pb, nboys, ngirls)
{
  loglik = nboys*log(pb) + ngirls*log(1-pb)
  return(loglik)
}

pb = seq(0,1,length=1000)

likpb = lik.fun(pb, nboys=5416, ngirls=5256)
loglikpb = loglik.fun(pb, nboys=5416, ngirls=5256)

dfpb = data.frame(pb, likpb, loglikpb)

ggplot(dfpb, aes(x=pb, y=likpb)) + geom_line(color="blue")
ggplot(dfpb, aes(x=pb, y=loglikpb)) + geom_line(color="blue")

