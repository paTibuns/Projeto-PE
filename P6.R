#Dados iniciais
nAmostras <- 1400
expSeed <- 1788
dimVector <- c(2,30,73)
limMin <- 11
limMax <- 15

#Gerar Amostra e Média
for (dim in dimVector) {
  #Gerar Amostra
  set.seed(seed = expSeed)
  amostra <- replicate(nAmostras, runif(n = dim, min = limMin, max = limMax))
  toDisc <- paste0("amostra_n", dim)
  assign(toDisc, amostra)
  #Média
  media <- apply(amostra,2,median)
  toDisc <- paste("media_n",dim)
  assign(toDisc,media)
}
