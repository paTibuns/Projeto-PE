library(tidyverse)
library(readxl)

emv <- read_excel('EsperancaVida.xlsx',
                  range = 'A9:CY70',
                  col_names = FALSE)

for (i in 2:ncol(emv)) {
  emv[1,i] <- substr(emv[1, i], start = 1 , stop = 2)
}

tiblEmv <- as_tibble(emv)
colNames <- tiblEmv[1,]
tiblEmv <- slice(tiblEmv,-1)
colnames(tiblEmv) <- colNames


tiblEmv %>%
  pivot_longer(slice(tiblEmv,n=3), names_to = "Grupo", values_to = "EMV")

#dfEMV <- data.frame(Grupo = rep(0,length.out = 2*nrow(residuos)),
#                    Ano = rep(c('2004','2018'),length.out = 2*nrow(residuos)),
#                    Sexo = rep(0,length.out = 2*nrow(residuos)),
#                    EMV = rep(0,))