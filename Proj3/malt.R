library(colorspace)
colors = rainbow_hcl(5)

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

m = malthusian(0.1, 1, 0.1, 11)
m2 = malthusian(0.4, 1, 0.1, 11)
m3 = malthusian(-0.1, 1, 0.1, 11)
m4 = malthusian(-0.4, 1, 0.1, 11)

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

f <- function(x, y) y
xx <- c(0, 1); yy <- c(-1, 1)
vectorfield(f, xx, yy, scale = 0.04, col = colors[1], n=15)
points(m$t, m$n, type = "l", col = colors[3], lwd = 2)
points(m2$t, m2$n, type = "l", col = colors[2], lwd = 2)
points(m3$t, m3$n, type = "l", col = colors[4], lwd = 2)
points(m4$t, m4$n, type = "l", col = colors[5], lwd = 2)
legend("bottom", legend = c("n = 0.4", "n = 0.1", "n = -0.1", "n = -0.3"),
       col = colors[2:5], lwd = 2,
       inset = -0.25, xpd = TRUE, horiz = TRUE)
