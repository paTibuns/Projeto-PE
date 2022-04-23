library(tidyverse)
library(readxl)

emv <- read_excel('EsperancaVida.xlsx',
                  range = 'A9:CY70',
                  col_names = FALSE)

tibblEmv <- as_tibble(emv)

#TODO SUBSTITUIR O LOOP POR UMA FORMA MAIS COMPACTA
#tibblEmv[1,] <-  map_at(tibblEmv,.at = 2:length(tibblEmv[1,]),as.list(sapply(strsplit(as.character(tibblEmv[1,]),split = " "), `[`, 1)))
#tibblEmv[1,3]
#map(tibblEmv[1,],str_c(tibblEmv[1,],'T',sep = '_'))

for (i in 2:ncol(tibblEmv)) {
  tibblEmv[1,i] <- sapply(strsplit(as.character(tibblEmv[1,i]),split = " "), `[`, 1)
  if(i<36) {
    tibblEmv[1,i] <- str_c(tibblEmv[1,i],'Total',sep = '_')
  }
  if(i>=36 & i<70 ) {
    tibblEmv[1,i] <- str_c(tibblEmv[1,i],'Homens',sep = '_')
  }
  if (i>=70){
    tibblEmv[1,i] <- str_c(tibblEmv[1,i],'Mulheres',sep = '_')
  }
}

#colNames <- tibblEmv[1,]
#tibblEmv <- slice(tiblEmv,-1)
#colnames(tibblEmv) <- colNames

#tibblEmv <- tibblEmv %>% replace_na(list('Ano')) %>%
#  print(tibblEmv)
#replace_na(tibblEmv[1,],list('Ano'))
#replace(tibblEmv[1,],1,'Ano')
colnames(tibblEmv) <- tibblEmv[1,]
colnames(tibblEmv)[1] <- 'Ano'
tibblEmv <- slice(tibblEmv,-1)

tibblEmv %>%
  pivot_longer(UE27_Total:CH_Mulheres,names_to = 'GS',values_to = 'EMV') %>%
  separate(GS,c('Grupo','Sexo'),sep = '_') %>%
  filter((Grupo == 'ES'| Grupo == 'GR'|Grupo == 'HU') &
           (between(Ano,2002,2019)) &
           (Sexo == 'Homens'|Sexo == 'Mulheres')) %>%
  ggplot()









#tiblEmv <- tibble(Grupo = rep('l',length.out = 3*nrow(emv)),
#                  Ano = rep(0,length.out = 3*nrow(emv)),
#                  Sexo =  rep(c('Total','Homens','Mulheres'),length.out = 3*nrow(emv)),
#                  EMV = rep(0,length.out = 3*nrow(emv)))

#for (i in 2:nrow(tiblEmv)) {
#  if(i==36) {
#    break
#  }
#  tiblEmv[3*(i-1)-2,1] <- emv[1,1+i]
#  tiblEmv[3*(i-1)-1,1] <- emv[1,1+i]
#  tiblEmv[3*(i-1),1] <- emv[1,1+i]
#}

#tiblEmv <- as_tibble(emv)
#colNames <- tiblEmv[1,]
#tiblEmv <- slice(tiblEmv,-1)
#colnames(tiblEmv) <- colNames

#tiblEmv %>%
#  pivot_longer(c(Homens,Mulheres),names_to = 'Sexo')
  #pivot_longer(select(tiblEmv,UE27:CH), names_to = "Grupo", values_to = "EMV")
  #pivot_longer(x, cols = c(lipids, density), names_to = c('.value', 'trait'),
         #names_sep = '[.]', values_drop_na = TRUE)


#dfEMV <- data.frame(Grupo = rep(0,length.out = 2*nrow(residuos)),
#                    Ano = rep(c('2004','2018'),length.out = 2*nrow(residuos)),
#                    Sexo = rep(0,length.out = 2*nrow(residuos)),
#                    EMV = rep(0,))