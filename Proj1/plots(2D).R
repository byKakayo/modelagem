source('LLS.R')
library(colorspace)
library(SciViews)

data = c(2.2 ,	0.49,
         4.6 ,	1.01,
         6.9 ,	1.51,
         9.5 ,	2.08,
         11.4,	2.49,
         15.8,	3.45,
         18.5,	4.03,
         23.3,	5.07,
         28  ,	6.1,
         31.3,	6.82,
         33  ,	7.18,
         36.4,	7.92,
         37.3,	8.12,
         40.5,  8.82,
         42.9,	9.35)

data <-matrix(data = data, byrow = T, ncol = 2)
p = LLS2D(data, 1)

fit = function(x){p[1] + p[2]*x}

lg <- c(paste0("a1 = ", round(p[2], 2)), paste0("a0 = ", round(p[1], 3)))

colors = rainbow_hcl(4)

plot(NULL, 
     main = "",
     axes = T,
     ylab = "Corrente",
     xlab = "Tensão",
     type = "l",
     xlim = c(0,45),
     ylim = c(0,10))
#axis(1, pos = 0)
#axis(2, pos = 0)
lines(fit(1:44), col = colors[3], lwd = 2)
points(data, col = colors[1], pch=19, cex=1.5)
legend("top", legend=rev(lg), cex = 1, x.intersp=-0.1)

data2 = c(2.31, 0.01e-3,
          2.36, 0.05e-3,
          2.42, 0.15e-3,
          2.43, 0.19e-3,
          2.44, 0.23e-3,
          2.46, 0.30e-3,
          2.47, 0.37e-3,
          2.48, 0.42e-3,
          2.49, 0.50e-3,
          2.51, 0.61e-3,
          2.52, 0.71e-3,
          2.54, 0.89e-3,
          2.55, 1.07e-3,
          2.56, 1.12e-3,
          2.58, 1.33e-3,
          2.61, 1.81e-3,
          2.62, 1.91e-3,
          2.65, 2.38e-3,
          2.69, 3.18e-3,
          2.76, 4.66e-3)

datan = c(1.74, 0.27e-3,
          1.77, 0.33e-3,
          1.79, 0.5e-3,
          1.82, 0.79e-3,
          1.86, 1.6e-3,
          1.89, 2.6e-3,
          1.92, 3.5e-3,
          1.96, 4.7e-3,
          1.98, 5.6e-3,
          2.02, 7.4e-3,
          2.05, 8.39e-3,
          2.06, 9.0e-3,
          2.08, 9.5e-3,
          2.09, 10.0e-3)


datan <-matrix(data = datan, byrow = T, ncol = 2)

pn = LLS2D(datan, 1, TRUE)

fitn = function(x){exp(pn[1])*exp(pn[2]*x)}

x = 1.7
y = fitn(x)
point = matrix(c(x,y), ncol = 2)

while (x < 2.1) {
        x = x + 0.05
        y = fitn(x)
        point <-rbind(point, c(x,y))
}

lgn <- c(paste0("b = ", round(pn[2], 2)), paste0("a = ", round(exp(pn[1]), 2)))

plot(NULL, 
     main = "",
     axes = T,
     ylab = "Tensão",
     xlab = "Corrente",
     type = "l",
     xlim = c(1.7,2),
     ylim = c(0,0.003))
#axis(1, pos = 0)
axis(2, pos = 0)
#lines(fitn(0:3), col = colors[3], lwd = 2)
points(point, col = colors[2], lwd = 2, type = 'l')
points(datan, col = colors[4], pch=19, cex=1.5)
legend("top", legend=rev(lgn), cex = 1, x.intersp=-0.1)
