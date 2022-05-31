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

for (i in 1:length(dimVector)){
  graf <- ggplot(mMedia, aes(x = mMedia[,i])) +
    geom_histogram(aes(y = after_stat(count / sum(count))), bins = 10, color="black", fill="darkgreen") +
    scale_y_continuous(labels = scales::percent)+
    stat_function(fun = dnorm, args = list(mean = valEsp,sd = mValVar[i]))+
    labs(x = 'Intervalo de valores',y = 'Frequencia Relativa')

  toDisc <- paste0("graf", i)
  assign(toDisc,graf)
}
histGraf <- ggarrange(graf1,graf2,graf3,ncol = 3,labels = c('n = 2','n = 30','n = 73'),hjust = -0.1,
          font.label = list(size = 14, face = "bold", color ="#00ad00"))
histGraf
#Para meter um titulo no graf usar:
#annotate_figure(histGraf, top = text_grob("Histograma da frequência relativa da média de X e distribuição normal",
                                          #color = "red", face = "bold", size = 14))