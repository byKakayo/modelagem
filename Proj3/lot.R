library(colorspace)
colors = rainbow_hcl(3)

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

l = lotka_volterra(2,2,1.5,1,1,1, 0.005, 10000)

plot(l$x, l$y)

vectorfield <- function(fun, xlim, ylim, n = 16,
                        scale = 0.05, col = "green", ...) {
  stopifnot(is.numeric(xlim), length(xlim) == 2,
            is.numeric(ylim), length(ylim) == 2)
  
  xpts <- linspace(xlim[1], xlim[2], n)
  ypts <- linspace(ylim[1], ylim[2], n)
  
  M <- meshgrid(xpts, ypts)
  x <- M$X
  y <- M$Y
  
  f <- function(x,y) x-x*y
  
  px = f(x,y);
  py = fun(x,y);#fun(x, y);
  
  plot(xlim, ylim, type="n", xaxs="i", yaxs="i", xlab = "", ylab = ""); grid()
  quiver(x, y, px, py, scale = scale, col = col, ...)
}

f <- function(x,y) x*y-y
xx <- c(0, 4); yy <- c(0, 4)
vectorfield(f, xx, yy, scale = 0.03, col = colors[1], n=20)
points(l$x, l$y, type = "l", lwd = 2, col = colors[3])