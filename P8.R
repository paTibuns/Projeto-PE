#Dados iniciai
expSeed <- 214
nAmostras <- 600
dim <- 1068
lamb <- 0.3

set.seed(seed = expSeed)
amostra <- replicate(nAmostras, rexp(n = dim, rate = lamb))