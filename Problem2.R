load("data.Rdata")
str(data)
data

hist(data)

likelihood_norm <- function(x, mu, sig) {
  y = 1/(2*pi*sig^2)*exp((-1/(2*sig^2))*(x-mu)^2)
}



loglikelihood <- function(theta, x) {
  mu = theta[1]
  sig = theta[2]
  n = length(x)
  lglklhood = -(n/2)*(log(2*pi*sig^2)) + (-1/(2*sig^2))*sum((x-mu)^2)
  return(-lglklhood)
}

optim_norm <- optim(par = c(0,1), loglikelihood, x = data, method = "BFGS")
optim_norm

optim_norm1 <- optim(par = c(0,1), loglikelihood, x = data, method = "CG")
optim_norm1


mean(data)
sd(data)

optim_norm2 <- optim(par = c(0,1), loglikelihood, x = data, method = "BFGS")
optim_norm2


gradient <- function(x) {
  
}





