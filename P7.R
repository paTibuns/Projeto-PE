#Dados iniciais
nAmostras <- 6105
expSeed <- 201
dim <- 15
nBern <- 50
pSuccess <- 0.46
nDecimais <- 6

#Gerar Amostra
set.seed(seed = expSeed)
amostra <- replicate(nAmostras, rbinom(n = dim, size = nBern, prob = pSuccess))
mMedia <- apply(amostra,2,mean)

#Resultado
valEspMed <- mean(mMedia)
valEspX <- nBern*pSuccess
res <- abs(valEspMed-valEspX)
format(round(res, nDecimais), nsmall = nDecimais)
