plotPoissonDevRes <- function( mod )
{
  lfitteda = predict(mod) # log scale
  lresida = resid(mod)  # linear model
  lresid.df = data.frame(lfitteda,lresida)
  
  ggplot(lresid.df,aes(x=lfitteda, y=lresida)) +
    geom_point()+
    geom_smooth(method = "loess", size = 1.5)+
    geom_line(y=0, size=1.5, col="red")+
    xlab("Fitted values") +
    ylab("Deviance Residuals") 
  
}