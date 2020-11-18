malthusian <- function(n_0, r, dt, n_t){
  library(pracma)
  t = linspace(0, n_t*dt, n_t+1)
  n <- replicate(n_t+1, 0)
  n[1] = n_0
  for (i in 1:(n_t)) {
    n[i+1] = n[i] + r*dt*n[i]
  }
  r <-list()
  r$t = t
  r$n = n
  return(r)
}

m = malthusian(0.1, 1, 0.1, 10)

plot(m$t, m$n)