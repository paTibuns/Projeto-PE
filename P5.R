#Dados iniciais
expSeed <- 1040
dim <- 476
lamb <- 0.29
tempEsp <- 4
nDecimais <- 6

#Gerar Amostra
set.seed(seed = expSeed)
amostra <- rexp(n = dim, rate = lamb)

#Probabilidade empirica
eDist <- ecdf(amostra)
eProb <- 1 - eDist(tempEsp)

#Probabilidade teorica
tProb <- exp(-(lamb*tempEsp))  #--> Cosiderado que P(X>=x)=P(X>x) porque distribuicao é continua

#Diferença entre probabilidades
res <- abs(tProb-eProb)
format(round(res, nDecimais), nsmall = nDecimais)