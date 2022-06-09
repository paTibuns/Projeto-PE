library(tidyverse)
library(readxl)
library(plyr)

dfO3lvl <- read_excel('QualidadeARO3.xlsx',col_types = rep('numeric',10))
tibblO3 <- as_tibble(dfO3lvl)

tibblO3 %>%
  pivot_longer(everything(),names_to = 'Local',values_to = 'O3lvl') %>%
  filter((Local == 'Mem-Martins' | Local == 'Antas-Espinho')) %>%
  ggplot(aes(x = O3lvl, color = Local,fill = Local))+
  geom_histogram(alpha = 0.5, position = "identity",binwidth = 5)+
  #TODO Mexer no binwidth para aumentar ou diminuir o intervalo dos valores representados
  theme(legend.position = "top")+
  scale_fill_brewer(palette = "Dark2")+
  scale_color_brewer(palette = "Dark2")+
  labs(title = 'Niveis de ozono registados nas estacoes de Mem-Martins e Antas-Espinho em 2020',
       caption = 'Dados obtidos de qualar.apambiente.pt',
       x = 'Valores dos niveis de ozono registados [microgramas por metro cubico]',
       y = 'Numero de observacoes registadas')