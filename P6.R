library(tidyverse)

#Dados iniciais
nAmostras <- 1400
expSeed <- 1788
dimVector <- c(2,30,73)
limMin <- 11
limMax <- 15
valEsp <- ((limMax+limMin)/2)

#Alocação
mMedia <- matrix(0,nAmostras,length(dimVector))
mValVar <- matrix(0,1,length(dimVector))

i <- 0
for (dim in dimVector) {
  i <- i + 1
  #Gerar Amostra
  set.seed(seed = expSeed)
  amostra <- replicate(nAmostras, runif(n = dim, min = limMin, max = limMax))

  mMedia[,i] <- apply(amostra,2,median)
  mValVar[i] <- (((limMax-limMin)^2/12)/dim)
}

#Construir Gráfico
#  ggplot() +
#    aes(mMedia$)+
#    geom_histogram(alpha = 0.5, position = "identity",binwidth = 5)+
#  #TODO Mexer no binwidth para aumentar ou diminuir o intervalo dos valores representados
#    theme(legend.position = "top")+
#    scale_fill_brewer(palette = "Dark2")+
#    scale_color_brewer(palette = "Dark2")+
#    labs(title = 'Niveis de ozono registados nas estacoes de Mem-Martins e Antas-Espinho em 2020',
#       caption = 'Dados obtidos de qualar.apambiente.pt',
#       x = 'Valores dos niveis de ozono registados [microgramas por metro cubico]',
#       y = 'Numero de observacoes registadas')
  #geom_freqpoly(binwidth = 10)
  #theme_minimal()
