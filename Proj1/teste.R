source('LLS.R')

data = c(-0.47, 1.14,
         -0.26, 1.21,
          0.15, 1.28,
          0.82, 1.47,
         -0.60, 0.93 )
data <-matrix(data = data, byrow = T, ncol = 2)
LLS2D(data, 1)

data2 = c(-0.47,  0.12,
          -0.26,  0.25,
           0.15,  0.18,
           0.82,  0.26,
          -0.60, -0.11 )
data2 <-matrix(data = data2, byrow = T, ncol = 2)
LLS2D(data2, 3)

library(plot3D)

x <- mtcars$wt
y <- mtcars$disp
z <- mtcars$mpg

dt <-matrix(c(x, y, z), byrow = F, ncol = 3)

fit <- LLS3D(dt, 1)

pred <- function(fit, x, y) {
  z = fit[1] + fit[2]*x + fit[3]*y + fit[4]*x*y 
  return(z)
}

grid.lines = 32
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)
z.pred <- pred(fit, xy$x, xy$y)
z.pred = matrix(z.pred, ncol = 32)
fitpoints = pred(fit, x, y)
scatter3D(x, y, z, pch = 18, cex = 2, 
          theta = 45, phi = 20, ticktype = "detailed",
          xlab = "wt", ylab = "disp", zlab = "mpg",  
          surf = list(x = x.pred, y = y.pred, z = z.pred,  
          facets = NA, fit = fitpoints), main = "")

lg <- c(paste0("a3 = ", round(fit[4], 2)), paste0("a2 = ", round(fit[3], 2)), paste0("a1 = ", round(fit[2], 2)), paste0("a0 = ", round(fit[1], 2)))
legend("right", legend=rev(lg), cex = 1, x.intersp=-0.1)