logistic <- function(n_0, r, m, dt, n_t){
  library(pracma)
  t = linspace(0, n_t*dt, n_t+1)
  n <- replicate(n_t+1, 0)
  n[1] = n_0
  for (i in 1:(n_t)) {
    n[i+1] = n[i] + dt*r*(1-n[i]/m)*n[i]
  }
  r <-list()
  r$t = t
  r$n = n
  return(r)
}