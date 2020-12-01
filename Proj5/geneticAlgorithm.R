geneticAlgorithm <- function(coords, eliteSize, popSize, mutRate, generations){
  #Número de genes
  n = dim(coords)[1]
  
  #Inicializar vetor com melhor indivíduo
  bestInd <-c()
  
  #Inicializa vetor com a distância do melhor indivíduo de cada geração
  listMinDist <-c()
  
  #Calculo das distâncias entre genes
  #dist: função da biblioteca stats
  d = as.matrix(dist(coords, method = 'euclidean'))

  #Gera a população inicial
  pop = initializePopulation(popSize, n)
  
  #Calcula distância da população
  distPop = calculateDistance(pop, popSize, d, n)
  
  #Seta o melhor indivíduo da primeira geração
  bestInd <- pop[sort(distPop, index.return=TRUE)$ix[1],]
  
  #Adiciona a distância do melhor indivíduo da primeira geração
  listMinDist <-append(listMinDist, min(distPop))
  
  #Operadores genéticos  
  for (j in 2:generations) {
    #Seleciona os pais para reprodução
    parents = selectParents(n, popSize, eliteSize, pop, distPop)
    
    #Calcula distância dos pais
    distPar = calculateDistance(parents, popSize, d, n) 
    
    #Gera os filhos
    children = breed(n, popSize, eliteSize, parents, distPar)
    
    #Aplica mutação
    pop = mutate(popSize, n, children, mutRate)
    
    #Calcula distãncia da nova população
    distPop = calculateDistance(pop, popSize, d, n)
    
    #Menor distância
    minDist = min(distPop)
    
    #Se for a menor distância encontrada até agora, atualizar melhor indivíduo
    if (minDist < min(listMinDist)) {
      bestInd = pop[which.min(distPop),]
    }
    
    #Adicionat a menor distância a lista
    listMinDist <-append(listMinDist, minDist)
  }
  
  #Formata o resultado
  res <-list()
  res$bestInd = bestInd
  res$distBestInd = min(listMinDist)
  res$listMinDist = listMinDist
  
  return(res)
}


initializePopulation <- function(popSize, n){
  #Inicializa a população
  pop <-matrix(, ncol = n+1, nrow = 0)
  
  #Inicializa o indivíduo
  ind <-c()
  
  for(i in 1:popSize){
    #Gera indivíduo aleatório
    ind = sample(n)
    
    #Obrigatoriedade de acabar onde começou
    ind[n+1] = ind[1]
    
    #Adicionar indivíduo à população 
    pop <-rbind(pop, ind)
  }
  
  return(pop)
}


calculateDistance <- function(population, popSize, distance, n){
  #Inicializa vetor de distãncias
  d <-c()
  
  #Percorre a população
  for (i in 1:popSize) {
    #Auxiliar para somar as distãncias
    aux = 0
    
    for (j in 1:n) {
      #Soma a distãncia entre atual e próximo
      aux = aux + distance[population[i,j], population[i,j+1]]
    }
    
    #Adiciona ao vetor
    d <-append(d, aux)
  }  
  
  return(d)
}


selectParents <- function(n, popSize, elitSize, population, distance){
  #Calcula aptidão da população
  fit = fitness(distance)
  
  #Inicializa população apta
  fitPop <- matrix(, ncol = n+1, nrow = 0)
  
  #Refaz a população com os mais aptos
  ##Aplicar o elitismo
  eliteInd <- sort(fit, index.return=TRUE, decreasing = TRUE)$ix
  for (i in 1:elitSize) {
    #Adiciona o indivíduo da elite na nova população
    fitPop <-rbind(fitPop, population[eliteInd[i],])
  }
  
  ##Aplicar a roleta
  #Transformar os valores de aptidão em porcentagem
  fit = fit/sum(fit) 
  for (i in (elitSize+1):popSize) {
    #Gera valor aleatório para escolher os individuos
    rand = runif(1, 0, 1)
    
    #Seleciona o indivíduo
    j = 1
    while(rand > fit[j]){
      fit[j+1] = fit[j+1] + fit[j]
      j = j + 1
    }
    
    #Adiciona o indivíduo escolhido a nova população
    fitPop <-rbind(fitPop, population[j,])
  }
  
  return(fitPop)
}


fitness <- function(distance){
  #Como nesse caso o único fator relevante é o valor da distância
  fit = 1/distance
  
  return(fit)
}


breed <- function(n, popSize, elitSize, parents, fitness){
  #Inicializa o conjunto de filhos
  children <-matrix(, ncol = n+1, nrow = 0)
  
  #Aplica o elitismo
  elitInd <- sort(fitness, index.return=TRUE)$ix
  for (i in 1:elitSize) {
    children <- rbind(children, parents[elitInd[i],])
  }
  
  #Percorre os pais
  for (i in 1:((popSize-elitSize)/2)) {
    #Seleciona 3 pais
    parent1 = parents[i,]
    parent2 = parents[(popSize/2+1),]
    parent3 = parents[(popSize+1-i),]
    
    #Reprodução do 1 com o 2 
    child1 = crossover(n, parent1, parent2)
    #Reprodução do 1 com o 3
    child2 = crossover(n, parent1, parent3)
    
    #Adiciona os filhos
    children <-rbind(children, child1)
    children <-rbind(children, child2)
  }
  
  return(children)
}


crossover <- function(n, parent1, parent2){
  #Inicializa o filho
  child <-c()
  
  #Define aleatoriamente o corte de um pai
  slice = sort(sample(1:n, 2))
  
  #Adiciona o corte ao filho
  for (j in slice[1]:slice[2]) {
    child <-append(child, parent1[j])
  }
  
  #Adiciona o restante dos genes
  child = c(child, setdiff(parent2, child))
  
  #Adiciona o primeiro gene no fim
  child <-append(child, child[1])
  
  return(child)
}


mutate <- function(popSize, n, population, mutRate){
  #Percorre cada individuo da população
  for (i in 1:popSize) {
    #Gera valor aleatório 
    rand = runif(1, 0, 1)
    
    #Se for menor que probabilidade de mutação
    if (rand < mutRate) {
      #Escolhe aleatoriamente os genes que iram mudar
      genes = sort(sample(1:n, 2))
      
      #Auxiliar para guardar valor de um dos genes que irá mudar
      aux = population[i,genes[1]]
      
      population[i,genes[1]] = population[i,genes[2]]
      #Se um dos genes que irá mudar for o primeiro, alterar o último também 
      if (genes[1] == 1) {
        population[i, n+1] = population[i,genes[2]]
      }
      population[i,genes[2]] = aux
    }
  }
  
  return(population)
}