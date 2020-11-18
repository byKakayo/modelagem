library(plot3D)
library(colorspace)
colors = rainbow_hcl(3)

g <- function(x,y) {
  return(x^2 + y^2)
}

g2 <- function(x,y) {
  return(-exp(-((x-1)**2+(y-1)**2)/0.3**2)-1.5*exp(-16*((x-2)**2+(y-2)**2)))
}

h2 <- function(x, y) {
  
}

rs <- function(f, n, Ix, Iy) {
  cz = Inf
  result <- list()
  result$X = c()
  result$Y = c()
  result$Z = c()
  
  for (i in seq(n)) {
    xi <- runif(1, Ix[1], Ix[2])
    yi <- runif(1, Iy[1], Iy[2])
    zi = f(xi, yi)
    
    if (zi < cz) {
      cx = xi
      cy = yi
      cz = zi
    }
    result$X <- c(result$X, cx)
    result$Y <- c(result$Y, cy)
    result$Z <- c(result$Z, cz)
  }
  
  return(result)
}

gradient <- function(f, x, y, h=0.001) {
  dx <- (f(x+h,y)-f(x,y))/h
  dy <- (f(x,y+h)-f(x,y))/h
  return(c(dx,dy))
}

gda <- function(f, n, Ix, Iy, D0=0.1) {
  cz = Inf
  result <- list()
  result$X = c()
  result$Y = c()
  result$Z = c()
  
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
    result$X <- c(result$X, cx)
    result$Y <- c(result$Y, cy)
    result$Z <- c(result$Z, cz)
  }
  
  return(result)
}

sgda <- function(f, n, Ix, Iy, D0=0.1) {
  cz = Inf
  result <- list()
  result$X = c()
  result$Y = c()
  result$Z = c()
  
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
    result$X <- c(result$X, cx)
    result$Y <- c(result$Y, cy)
    result$Z <- c(result$Z, cz)
  }
  
  return(result)
}

meansd_method <- function(method, f, m, n, Ix, Iy) {
  data <- list()
  for (i in seq(m)) {
    saida <- method(f, n, Ix, Iy)
    data <- rbind(data, saida$Z)
  }
  data <- matrix(unlist(data), ncol = n, byrow = FALSE)
  res <- list()
  res$mean <- as.vector(colMeans(data))
  res$sd <- as.vector(sqrt(diag(var(data))))
  return(res)
}

plot3d_f <- function(f, Ix, Iy, Iz, path) {
  x <- seq(Ix[1], Ix[2], by = 0.1)
  y <- seq(Iy[1], Iy[2], by = 0.1)
  grid <- mesh(x,y)
  z <- with(grid, f(x,y))
  scatter3D(path$X, path$Y, path$Z, theta = 20, phi = 30,
            bty = "b2", type = "l",col = ramp.col(c("dark green", "khaki", "dark red")), ticktype = "detailed",
            xlim = Ix, ylim = Iy, zlim = Iz, lwd = 4,
            surf = list(z = z, x = x, y = y, alpha = 0.5)
  )
}

m = 300; n = 100; Ix = c(-1,1); Iy = c(-1,1);
mediadvrs <- meansd_method(rs, g, m, n, Ix, Iy)
mediadvgda <- meansd_method(gda, g, m, n, Ix, Iy)

vals <- c(2, 8, 15, 20)
vals1 <- c(1, 3, 6)
sd_rs = mediadvrs$mean-mediadvrs$sd
sd_rs1 = mediadvrs$mean+mediadvrs$sd
sd_gd = mediadvgda$mean-mediadvgda$sd
sd_gd1 = mediadvgda$mean+mediadvgda$sd

plot(x=seq(n), y=mediadvrs$mean, col=colors[1], type="l",  xaxs="i",  xlab = "iterações", ylab="Mínimo", xlim=c(0,25), ylim = c(0,0.8), lwd= 2);
lines(x=seq(n), y=mediadvgda$mean, col=colors[2], type="l", lwd = 2);
arrows(x0=vals, y0=c(sd_rs[2], sd_rs[8], sd_rs[15], sd_rs[25]),
       x1=vals, y1=c(sd_rs1[2], sd_rs1[8], sd_rs1[15], sd_rs1[25]), col=colors[1], lwd=1, length=0.05, angle=90, code=3);
arrows(x0=vals1, y0=c(sd_gd[1], sd_gd[3], sd_gd[6]), 
       x1=vals1, y1=c(sd_gd1[1], sd_gd1[3], sd_gd1[6]), col=colors[2], lwd=1, length=0.05, angle=90, code=3);
legend("topright", legend = c("Random Search", "Gradient Descemt"),
       col = colors[1:2], lwd = 2,
       inset = 0, xpd = TRUE, horiz = TRUE)




x <- y <- seq(-1, 1, length= 20)
z <- outer(x, y, g)
test = rs(g, n, Ix, Iy)
test2 = gda(g, n, Ix, Iy)

pmat <-persp(x, y, z, phi = 60, theta = 100, shade = 0.3)
lines(trans3d(test$X, test$Y, test$Z, pmat=pmat), lwd = 2, col = "red")

pmat <-persp(x, y, z, phi = 60, theta = 100, shade = 0.3)
lines(trans3d(test2$X, test2$Y, test2$Z, pmat=pmat), lwd = 2, col = "red")

gsa <- function(f, n, Ix, Iy, D0=0.6, T0=1000, d=0.3){
  cz = Inf
  result <- list()
  result$X = c()
  result$Y = c()
  result$Z = c()
  
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
    result$X <- c(result$X, cx)
    result$Y <- c(result$Y, cy)
    result$Z <- c(result$Z, cz)
  }
  
  return(result)
}

n = 200; Ix = c(0,3); Iy = c(0,3); Iz = c(-2,0.1);
saidagsa <- gsa(g2, n, Ix, Iy)

f <-function(x,y){return(-exp(-((x-1)^2+(y-1)^2)/(0.3^2))-0.5*(exp(-16*((x-2)^2+(y-2)^2))))}
x <- y <- seq(-0, 3, length= 50)
z <- outer(x, y, f)
pmat <-persp(x, y, z, phi = 30, theta = 45, shade = 0.01)
lines(trans3d(saidagsa$X, saidagsa$Y, saidagsa$Z, pmat=pmat), lwd = 2, col = "red")

mediadvrs <- meansd_method(rs, f, m, n, Ix, Iy)
mediadvgsa <- meansd_method(gsa, f, m, n, Ix, Iy)

vals <- c(2, 10, 30, 50)
vals1 <- c(6, 15, 20)
sd_rs = mediadvrs$mean-mediadvrs$sd
sd_rs1 = mediadvrs$mean+mediadvrs$sd
sd_gd = mediadvgsa$mean-mediadvgsa$sd
sd_gd1 = mediadvgsa$mean+mediadvgsa$sd

plot(x=seq(n), y=mediadvrs$mean, col=colors[1], type="l",  xaxs="i",  xlab = "iterações", ylab="Mínimo", xlim=c(0.,60), ylim = c(-1,0.2), lwd= 2);
lines(x=seq(n), y=mediadvgsa$mean, col=colors[2], type="l", lwd = 2);
arrows(x0=vals, y0=c(sd_rs[2], sd_rs[10], sd_rs[30], sd_rs[50]),
       x1=vals, y1=c(sd_rs1[2], sd_rs1[10], sd_rs1[30], sd_rs1[50]), col=colors[1], lwd=1, length=0.05, angle=90, code=3);
arrows(x0=vals1, y0=c(sd_gd[6], sd_gd[15], sd_gd[20]), 
       x1=vals1, y1=c(sd_gd1[6], sd_gd1[15], sd_gd1[20]), col=colors[2], lwd=1, length=0.05, angle=90, code=3);
legend("topright", legend = c("Random Search", "Gradient with Simulated Anealing"),
       col = colors[1:2], lwd = 2,
       inset = 0, xpd = TRUE, horiz = TRUE)