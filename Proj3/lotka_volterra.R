lotka_volterra <- function(x_0, y_0, a, b, c, d, dt, n_t){
  library(pracma)
  x <- replicate(n_t+1, 0)
  y <- replicate(n_t+1, 0)
  x[1] = x_0
  y[1] = y_0  
  for (i in 1:(n_t)) {
    x[i+1] = x[i] + dt*x[i]*(a - b*y[i])
    y[i+1] = y[i] + dt*y[i]*(c*x[i] - d)
  }
  r <-list()
  r$x = x
  r$y = y
  return(r)
}