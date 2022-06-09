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
stdDev <- matrix(0,1,length(dimVector))

#Cálculos intemédios
valEsp <- ((limMax+limMin)/2)
valVar <- (((limMax-limMin)^2)/12)

i <- 0
for (dim in dimVector) {
  i <- i + 1
  #Gerar Amostra
  set.seed(seed = expSeed)
  amostra <- replicate(nAmostras, runif(n = dim, min = limMin, max = limMax))
  mMedia[,i] <- apply(amostra,2,mean)
  stdDev[i] <- sqrt(valVar/dim)
  graf <- ggplot(mMedia, aes(x = mMedia[,i])) +
    geom_histogram(aes(y = after_stat(count / sum(count))), bins = 16, color="black", fill="darkgreen") +
    stat_function(fun = dnorm, args = list(mean = valEsp,sd = stdDev[i]))+
    scale_y_continuous(labels = scales::percent)+
    labs(x = 'Intervalo de valores',y = 'Frequencia Relativa')

  toDisc <- paste0("graf", i)
  assign(toDisc,graf)
}
histGraf <- ggarrange(graf1,graf2,graf3,ncol = 3,labels = c('n = 2','n = 30','n = 73'),hjust = -0.1, vjust = 1.1,
          font.label = list(size = 14, face = "bold", color ="#00ad00"))
annotate_figure(histGraf, top = text_grob("Histograma da frequencia relativa da media de X e distribuicao normal",
                                          color = "black", face = "bold", size = 12))