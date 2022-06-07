library(tidyverse)
#Dados iniciais
expSeed <- 270
nAmostras <- 500
dimArray <- seq(100, 5000, by = 100)
lamb <- 1.32
lvlConf <- 0.97

alpha <- 1 - lvlConf
quantAlpha <- qnorm(1-alpha/2)

dfGraf <- data.frame(matrix(nrow = length(dimArray),ncol = 2))
colnames(dfGraf) <- c('dim','MAn')
j <- 0
for (dim in dimArray){
  j <- j + 1
  set.seed(seed = expSeed)
  amostra <- replicate(nAmostras, rexp(n = dim, rate = lamb))
  media <- apply(amostra,2,mean)
  limAmp <- array (dim = nAmostras)
  for (i in 1:length(media)){
    limInf <- ((1-(quantAlpha/sqrt(dim)))/media[i])
    limSup <- ((1+(quantAlpha/sqrt(dim)))/media[i])
    limAmp[i] <- limSup - limInf
  }
  dfGraf[j,1] <- dim
  dfGraf[j,2] <- mean(limAmp)
}

ggplot(dfGraf,aes(x = dim, y = MAn))+
  geom_point(size = 5, alpha = 0.7, colour = 4)+
  theme_minimal()+
  labs(title = 'Media da amplitude do intervalo de confianca vs dimensao da amostra',
       caption = 'Semente = 270   m = 500   lambda = 1.32   (1 - alpha = 0.97)',
       x = 'Dimensao das m amostras',
       y = 'Media da amplitude do intervalo de confianca das m amostras')