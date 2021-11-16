routine = function(f, points){
  df = data.frame(x=points, y=sapply(points, f))
  f_hat = function(x, par) par[1] + par[2]*x + par[3]*x^2
  loss = function(data, par) with(data, sum((f_hat(x, par) - y)^2))
  a = optim(par = rep(0.5,3), fn = loss, data = df ,  method = "BFGS")
  return(a$par)
}

test <- routine(function(x) x^3, c(0.1, 0.55, 0.8))
test

inter_function <- function(f, n) {
  df <- data.frame(min = c(0:(n-1)),
                   mid = c(1:n),
                   max = c(1:n),
                   a0 = c(1:n),
                   a1 = c(1:n),
                   a2 = c(1:n),
                   stringsAsFactors=FALSE)
  
  df$min <- df$min/n
  df$max <- df$max/n
  df$mid <- (df$min+df$max)/2
  
  for(i in 1:nrow(df)) {
    ai_par <- routine(f, c(df[i,1] , df[i,2] , df[i,3]) )
                      df[i,4] <- ai_par[1]
                      df[i,5] <- ai_par[2]
                      df[i,6] <- ai_par[3]
  }
  return(df)
}


fn1 = function(x) -x*(1-x)
func1 <- inter_function(fn1, n = 100)
func1


f1 <- data.frame(x1 = seq(from = 0 , to = 1 , by = 0.01))

true_f1 <- (-f1$x1*(1-f1$x1))
plot(y=true_f1, 
     x=f1$x1,
     xlab = "x",
     ylab = "f(x)",
     title("Actual f(x) = -x(1-x)"),
     t = 'l')



f1_hat <- c()
data1 <- c()
for( i in 1:100 ) {
  x= c(func1[i,1], func1[i,2] , func1[i,3])
  y = func1[i,4] + func1[i,5]*x+ func1[i,6]*x**2
  f1_hat = c(f1_hat, y)
  data1 = c(data1, x)
}

plot(y=f1_hat , x=data1, title("Interpolated f(x), n=100", xlab = "x", ylab = "f(x) = -x(1-x)"))


fn2 = function(x) (-x*sin(10*pi*x))
func2 <- inter_function(fn2, n = 100)
func2


true_f2 <- -f1$x1*sin(10*pi*f1$x1)
plot(y=true_f2, 
     x=f1$x1,
     xlab = "x",
     ylab = "f(x)",
     title("Actual f(x) = -x*sin(10*pi*x)"),
     t = 'l')


f2_hat <- c()
data2 <- c()
for( i in 1:100 ) {
  x= c(func2[i,1], func2[i,2] , func2[i,3])
  y = func2[i,4] + func2[i,5]*x+ func2[i,6]*x**2
  f2_hat = c(f2_hat , y)
  data2 = c(data2 , x )
}

plot(y=f2_hat , x=data2, title("Interpolated f(x), n=100 "), xlab = "x", ylab = "f(x) = -x*sin(10*pi*x)" )


#n=1000
f1_1 <- data.frame(x1 = seq(from = 0 , to = 1 , by = 0.001))

fn2 = function(x) (-x*sin(10*pi*x))
func3 <- inter_function(fn2, n = 1000)
func3


true_f3 <- -f1_1$x1*sin(10*pi*f1_1$x1)
plot(y=true_f3, 
     x=f1_1$x1,
     xlab = "x",
     ylab = "f(x)",
     title("Actual f(x) = -x*sin(10*pi*x)"))


f3_hat <- c()
data3 <- c()
for( i in 1:1000 ) {
  x= c(func3[i,1], func3[i,2] , func3[i,3])
  y = func3[i,4] + func3[i,5]*x+ func3[i,6]*x**2
  f3_hat = c(f3_hat , y)
  data3 = c(data3 , x )
}

plot(y=f3_hat , x=data3, title("Interpolated f(x), n=1000 "), xlab = "x", ylab = "f(x) = -x*sin(10*pi*x)", t='l')


#n=10
f1_2 <- data.frame(x1 = seq(from = 0 , to = 1 , by = 0.1))
func4 <- inter_function(fn1, n = 10)

true_f4 <- (-f1_2$x1*(1-f1_2$x1))
plot(y=true_f4, 
     x=f1$x1,
     xlab = "x",
     ylab = "f(x)",
     title("Actual f(x) = -x(1-x)"),
     t = 'l')



f4_hat <- c()
data4 <- c()
for( i in 1:10 ) {
  x= c(func4[i,1], func4[i,2] , func4[i,3])
  y = func4[i,4] + func4[i,5]*x+ func4[i,6]*x**2
  f4_hat = c(f4_hat, y)
  data4 = c(data4, x)
}

plot(y=f4_hat , x=data4, title("Interpolated f(x), n=10", xlab = "x", ylab = "f(x) = -x(1-x)"), t= 'l')


#n=3
f1_3 <- data.frame(x1 = seq(from = 0 , to = 1 , by = 1/3))
func5 <- inter_function(fn1, n = 3)

true_f5 <- (-f1_3$x1*(1-f1_3$x1))
plot(y=true_f5, 
     x=f1_3$x1,
     xlab = "x",
     ylab = "f(x)",
     title("Actual f(x) = -x(1-x)"),
     t = 'l')



f5_hat <- c()
data5 <- c()
for( i in 1:3 ) {
  x= c(func5[i,1], func5[i,2] , func5[i,3])
  y = func5[i,4] + func5[i,5]*x+ func5[i,6]*x**2
  f5_hat = c(f5_hat, y)
  data5 = c(data5, x)
}

plot(y=f5_hat , x=data5, title("Interpolated f(x), n=3", xlab = "x", ylab = "f(x) = -x(1-x)"), t= 'l')
