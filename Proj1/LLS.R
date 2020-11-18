#Linear Least Squares
#Mínimos quadráticos

#Função inv()
library(matlib)
library(SciViews)

#Fitting 2D
LLS2D <- function(data, grau, log=FALSE) {
  #d: qtd de pontos e dimensão 
  d = dim(data)
  #Matrix A
  A <-matrix(rep(1, len = d[1]), nrow = d[1], ncol = 1)
  for (i in 1:grau-2+d[2]) {
    A <-cbind(A, data[,1]^i)
  }
  #Vetor y
  y <-data[,2]
  if(log){y = ln(y)}
  p = inv(t(A)%*%A)%*%t(A)%*%y
  return(p)
}

#Fitting 3D
LLS3D <- function(data, grau) {
  #d: qtd de pontos e dimensão 
  d = dim(data)
  #Matrix A
  A <-matrix(c(rep(1, len = d[1]), data[,1], data[,2], data[,1]*data[,2]), nrow = d[1], ncol = 4)
  if (grau > 1) {
    for (i in 2:grau) {
      A <-cbind(A, data[,1]^i)
      A <-cbind(A, data[,2]^i)
    }    
  }
  #Vetor z
  z <-data[,3]
  p = inv(t(A)%*%A)%*%t(A)%*%z
  return(p)
}