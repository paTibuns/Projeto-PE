#Dados iniciais
expSeed <- 1040
dim <- 476
lamb <- 0.29
tempEsp <- 4

#Gerar Amostra
set.seed(seed = expSeed)
amostra <- rexp(n = dim, rate = lamb)
#Probabilidade empirica
eDist <- ecdf(amostra)
eProb <- 1 - eDist(tempEsp)   #P(X>4) = 1 - P(X<=4)
#Probabilidade teorica
tProb <- exp(-(lamb*tempEsp))  #Considerado que P(X>=x) = P(X>x) porque distribuicao é continua
#Diferença entre probabilidades
res <- abs(tProb-eProb)
format(round(res, 6), nsmall = 6)