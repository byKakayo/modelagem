source('geneticAlgorithm.R')

#Gera um gráfico circular com distribuição normalizada 
circunferencenormaldistribution <- function(n, r=1) {
  # inicializa os vetores x e y
  x <- c()
  y <- c()
  
  #Loop para preencher os vetores x e y
  while (length(x) < n) {
    #Gera um ponto aleatorio
    p = runif(1, min=-pi, max=pi)
    
    x <- c(x, r*cos(p))
    y <- c(y, r*sin(p))
  }
  
  return(data.frame("x"=x, "y"=y))
}

n = 15


#cities = circunferencenormaldistribution(n)


cities = read.table("test.dat", sep = ",")


library(colorspace)

colors = rainbow_hcl(3)

#res = geneticAlgorithm(cities, popSize = 100, eliteSize = 10, mutRate = 0.05, generations = 200)
#plot(cities, 
#     cex = 1.2, pch = 19,
#     col = colors[1],
#     xlim = c(-1,1), ylim = c(-1,1), 
#     main = "Percurso",
#     mai = c(0,0,0,0),
#     axes = F,
#     xlab = "", ylab = "")
#axis(1, pos = -1.15)
#axis(2, pos = -1.1)
#text(cities, labels = c(1:15), pos = 1, cex = 0.8, font = 2)
#lines(cities$x[res$bestInd], cities$y[res$bestInd], col = colors[3], lwd = 2)

#plot(res$listMinDist, 
#     type = "l",
#     lwd = 2,
#     col = colors[2],
#     main = "Otimização",
#     ylim = c(5,15),
#     xlim = c(0,200),
#     mai = c(0,0,0,0),
#     axes = T,
#     xlab = "Geração", ylab = "Distância mínima")

res = geneticAlgorithm(cities, popSize = 100, eliteSize = 10, mutRate = 0.05, generations = 1000)

plot(cities, 
     cex = 1.2, pch = 19,
     col = colors[1],
     xlim = c(0,2000), ylim = c(0,1400), 
     main = "Percurso",
     mai = c(0,0,0,0),
     axes = F,
     xlab = "", ylab = "")
axis(1, pos = -75)
axis(2, pos = -50)
text(cities, labels = c(1:52), pos = 1, cex = 0.8, font = 2)
lines(cities$V1[res$bestInd], cities$V2[res$bestInd], col = colors[3], lwd = 2)

plot(res$listMinDist, 
     type = "l",
     lwd = 2,
     col = colors[2],
     main = "Otimização",
#    xlim = c(0,200), ylim = c(5,15), 
     mai = c(0,0,0,0),
     axes = T,
     xlab = "Geração", ylab = "Distância mínima")