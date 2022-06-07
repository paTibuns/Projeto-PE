#Dados iniciais
expSeed <- 214
nAmostras <- 600
dim <- 1068
lamb <- 0.3
lvlConf <- 0.97

set.seed(seed = expSeed)
amostra <- replicate(nAmostras, rexp(n = dim, rate = lamb))
media <- apply(amostra,2,mean)

alpha <- 1 - lvlConf
quantAlpha <- qnorm(1-alpha/2)

limAmp <- array (dim = nAmostras)
for (i in 1:length(media)){
  limInf <- ((1-(quantAlpha/sqrt(dim)))/media[i])
  limSup <- ((1+(quantAlpha/sqrt(dim)))/media[i])
  limAmp[i] <- limSup - limInf
}
res <- mean(limAmp)
format(round(res, 6), nsmall = 6)