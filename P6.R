#Dados iniciais
nAmostras <- 1400
expSeed <- 1788
dim <- c(2,30,73)
limMin <- 11
limMax <- 15

#Gerar Amostra
for (i in dim) {
  set.seed(seed = expSeed)
  amostra <- replicate(nAmostras, runif(n = i, min = limMin, max = limMax))
  dvn <- paste0("amostra_n", i)
  assign(dvn, amostra)
}

#Média das Amostras
for (i in dim) {

}