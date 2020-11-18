simulated_anealing <- function(f, n, Ix, Iy, D0=0.6, T0=1000, d=0.3){
  cz = Inf
  r <- list()
  r$X = c()
  r$Y = c()
  r$Z = c()
  T = seq(0, T0, length.out = n)
  p = c(runif(1, Ix), runif(1, Iy))
  for (i in seq(n)) {
    Di = D0*(1 - i/n)
    p1 = p - (gradient(f, p[1], p[2]) - runif(2, -d, d))*Di/sqrt(sum(p^2))
    f0 = f(p[1], p[2])
    f1 = f(p1[1], p1[2])
    if (f1 < f0 | runif(1) > exp((f0-f1)*T[i])) {
      p = p1
    }
    zi = f(p[1], p[2])
    if (zi < cz) {
      cx = p[1]
      cy = p[2]
      cz = zi
    }
    r$X <- c(r$X, cx)
    r$Y <- c(r$Y, cy)
    r$Z <- c(r$Z, cz)
  }
  return(r)
}