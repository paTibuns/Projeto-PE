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
  geom_histogram(aes(y = after_stat(count / sum(count))), bins = 20) +
  scale_y_continuous(labels = scales::percent)+
  stat_function(fun = dnorm, args = list(mean = valEsp,sd = mValVar[i]))
  #geom_line(data = data.frame(dnorm(x,mean=valEsp, sd=mValVar[i])))#, aes(group = 1), size = 1.25, color = "black")
  if (i==1){
    grafArray <- ggarrange(graf)
  } else {
    grafArray <- ggarrange(graf,grafArray)
  }
}

grafArray

ggplot(mMedia,aes(mMedia[,1]))+
geom_histogram(aes(y = ..density..),alpha = 0.5, position = "identity",binwidth = 10)+
stat_function(fun = dnorm, args = list(mean = valEsp,sd = mValVar[1]))

#ggplot(mMedia, aes(x = mMedia[,3])) +
#  geom_histogram(aes(y = after_stat(count / sum(count))), bins = 20) +
# scale_y_continuous(labels = scales::percent)