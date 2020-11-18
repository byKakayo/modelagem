random_search <- function(f, n, Ix, Iy) {
  cz = Inf
  r <- list()
  r$X = c()
  r$Y = c()
  r$Z = c()
  for (i in seq(n)) {
    xi <- runif(1, Ix[1], Ix[2])
    yi <- runif(1, Iy[1], Iy[2])
    zi = f(xi, yi)
    if (zi < cz) {
      cx = xi
      cy = yi
      cz = zi
    }
    r$X <- c(r$X, cx)
    r$Y <- c(r$Y, cy)
    r$Z <- c(r$Z, cz)
  }
  return(r)
}