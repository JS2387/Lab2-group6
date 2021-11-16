load("data.Rdata")
str(data)
data

hist(data)

likelihood_norm <- function(x, mu, sig) {
  y = 1/(2*pi*sig^2)*exp((-1/(2*sig^2))*(x-mu)^2)
}

plot(x= data, likelihood_norm(data, mu = mean(data), sig = sd(data)))
theta <- c(mean(data),sd(data))



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



gradient <- function(par, x) {
  mu = par[1]
  sigma = par[2]
  n = length(x)
  mu_est = -(sum(x) - n*mu)/sigma^2
  sigma_est = n/sigma - sum((x - mu)^2)/sigma^3
  return(c(mu_est, sigma_est))
}


optim_norm2 <- optim(par = c(0,1), gr = gradient, loglikelihood, x = data, method = "BFGS")
optim_norm2

optim_norm3 <- optim(par = c(0,1), gr = gradient, loglikelihood, x = data, method = "CG")
optim_norm3


optim_norm$par
optim_norm1$par
optim_norm2$par
optim_norm3$par

optim_norm$counts
optim_norm1$counts
optim_norm2$counts
optim_norm3$counts

optim_norm$value
optim_norm1$value
optim_norm2$value
optim_norm3$value


nll.normal <- function(data, par) {
  return(-sum(log(dnorm(data, mean=par[1], sd=par[2]))))
}
optim(par=c(0, 1), fn=nll.normal, data=data, method = "BFGS")
