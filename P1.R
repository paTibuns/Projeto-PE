library(tidyverse)
library(readxl)

residuos <- read_excel('ResiduosPerCapita.xlsx',
                       range = 'A14:C43',
                       col_types = c('text','numeric','numeric'),
                       col_names = c('Grupo','2004','2018'))
# Reduzir os nomes dos Grupos às suas iniciais
for (i in 1:nrow(residuos)) {
  residuos[i,1] <- sapply(strsplit(as.character(residuos[i,1]),split = " "), `[`, 1)
}

tibleResiduos <- as_tibble(residuos)
tibleResiduos %>%
  pivot_longer(c(`2004`, `2018`), names_to = "Ano", values_to = "Residuos") %>%
  filter(Grupo == 'IT'| Grupo == 'CZ'| Grupo == 'MT') %>%
  ggplot(aes(x = Grupo, y = Residuos, fill = Ano)) +
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label=Residuos), vjust=1.6, color="white",
            position = position_dodge(0.9), size=5)+
  scale_fill_manual(values=c('#abd4a3',"#78d466"))+
  theme_minimal()
