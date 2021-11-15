#2.1
load("data.Rdata")
#y <- as.data.frame(data)
#View(df)



#2.2

# name<-function(pars,object)f
# declarations
# logl<-loglikelihood function
# return(-logl)



normal.lik1v <- function(theta,x)
  { 
  
  mu <- theta[1]
  sigma2 <- theta[2]
  n <- nrows(x)
  logl <- (-.5*n*log(2*pi) -.5*n*log(sigma2) + (-1/(2*sigma2))*sum((x-mu)**2))
  
  return(-logl)
}


#2.3

#without gradient specified
result<- optim(par = c(0,1), normal.lik1v, x = data, method = "BFGS")
result

#with gradient specified

