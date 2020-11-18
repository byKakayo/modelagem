library(colorspace)
colors = rainbow_hcl(5)

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

l = logistic(2.5, 1, 1, 0.1, 1000)
l2 = logistic(0.4, 1, 1, 0.1, 1000)
l3 = logistic(0, 1, 1, 0.1, 1000)
l4 = logistic(-0.4, 1, 1, 0.1, 1000)

vectorfield <- function(fun, xlim, ylim, n = 16,
                        scale = 0.05, col = "green", ...) {
  stopifnot(is.numeric(xlim), length(xlim) == 2,
            is.numeric(ylim), length(ylim) == 2)
  
  xpts <- linspace(xlim[1], xlim[2], n)
  ypts <- linspace(ylim[1], ylim[2], n)
  
  M <- meshgrid(xpts, ypts)
  x <- M$X
  y <- M$Y
  
  px = matrix(1, nrow=n , ncol=n)
  py = fun(x, y);
  
  plot(xlim, ylim, type="n", xaxs="i", yaxs="i", xlab = "", ylab=""); grid()
  quiver(x, y, px, py, scale = scale, col = col, ...)
}

f <- function(x, y) y*(1-y)
xx <- c(0, 5); yy <- c(-2.5, 2.5)
vectorfield(f, xx, yy, scale = 0.07, col = colors[1], n=15)
points(l$t, l$n, type = "l", col = colors[2], lwd = 2)
points(l2$t, l2$n, type = "l", col = colors[3], lwd = 2)
points(l3$t, l3$n, type = "l", col = colors[4], lwd = 2)
points(l4$t, l4$n, type = "l", col = colors[5], lwd = 2)
legend("bottom", legend = c("n = 2.5", "n = 0.4", "n = 0", "n = -0.4"),
       col = colors[2:5], lwd = 2,
       inset = -0.25, xpd = TRUE, horiz = TRUE)