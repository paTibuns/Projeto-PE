library(tidyverse)
library(readxl)

residuos <- read_excel('ResiduosPerCapita.xlsx',
                       range = 'A14:C43',
                       col_types = c('text','numeric','numeric'),
                       col_names = c('Grupo','2004','2018'))
# Organizar os dados
for (i in 1:nrow(residuos)) {
  residuos[i,1] <- substr(residuos[i, 1], start = 1 , stop = 2)
}

dfResiduos <- data.frame(Grupo = rep(0,length.out = 2*nrow(residuos)),
                         Ano = rep(c('2004','2018'),length.out = 2*nrow(residuos)),
                         Residuos = rep(0,length.out = 2*nrow(residuos)))

for(i in 1:nrow(residuos)){
  dfResiduos[2*i-1,1] <- residuos[i, 1]
  dfResiduos[2*i,1] <- residuos[i, 1]

  dfResiduos[2*i-1,3] <- residuos[i,2]
  dfResiduos[2*i,3] <- residuos[i,3]
}

# Fazer o gráfico
dfResiduos %>%
  filter(Grupo == 'IT'|
           Grupo == 'CZ'|
           Grupo == 'MT') %>%
  ggplot(aes(x = Grupo, y = Residuos, fill = Ano)) +
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label=Residuos), vjust=1.6, color="white",
            position = position_dodge(0.9), size=5)+
  scale_fill_manual(values=c('#abd4a3',"#78d466"))+
  theme_minimal()
