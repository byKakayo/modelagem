gradient <- function(f, x, y, h=0.001) {
  dx <- (f(x+h,y)-f(x,y))/h
  dy <- (f(x,y+h)-f(x,y))/h
  return(c(dx,dy))
}

gradient_descent <- function(f, n, Ix, Iy, D0=0.1) {
  cz = Inf
  r <- list()
  r$X = c()
  r$Y = c()
  r$Z = c()
  xi <- runif(1, Ix[1], Ix[2])
  yi <- runif(1, Iy[1], Iy[2])
  p <- c(xi, yi)
  for (i in seq(n)) {
    Di = D0*(1 - i/n)
    p = p - gradient(f, p[1], p[2])*Di/sqrt(sum(p^2))
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
