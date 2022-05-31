library(tidyverse)
library(ggpubr)

#Dados iniciais
nAmostras <- 1400
expSeed <- 1788
dimVector <- c(2,30,73)
limMin <- 11
limMax <- 15

#Alocação
mMedia <- data.frame(matrix(0,nAmostras,length(dimVector)))
mValVar <- matrix(0,1,length(dimVector))
x <- seq(from = limMin,to = limMax,len = nAmostras/2)

#Cálculos intemédios
valEsp <- ((limMax+limMin)/2) #Valor esperado
valVar <- (((limMax-limMin)^2)/12)

i <- 0
for (dim in dimVector) {
  i <- i + 1
  #Gerar Amostra
  set.seed(seed = expSeed)
  amostra <- replicate(nAmostras, runif(n = dim, min = limMin, max = limMax))
  mMedia[,i] <- apply(amostra,2,median)
  mValVar[i] <- (valVar/dim)
}

par(mfrow=c(1,length(dimVector)))
for (i in 1:length(dimVector)){
  graf <- ggplot(mMedia, aes(x = mMedia[,i])) +
  geom_histogram(aes(y = after_stat(count / sum(count))), bins = 10) +
  scale_y_continuous(labels = scales::percent)+
  stat_function(fun = dnorm, args = list(mean = valEsp,sd = mValVar[i]))+
    labs(title = 'Amostras de uma populacao, X, com distribuicao Uniforme ',x='',y='Frequencia relativa')
  if (i==1){
    grafArray <- ggarrange(graf)
  } else {
    grafArray <- ggarrange(graf,grafArray)
  }
}
grafArray