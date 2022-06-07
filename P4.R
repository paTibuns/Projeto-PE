library(tidyverse)
library(readxl)

dfUten <- read_excel('Utentes.xlsx',col_types = rep('numeric',4))
tibblUten <- as_tibble(dfUten)

tibblUten %>%
  ggplot(aes(x = IMC,y = TAD))+
  geom_point(size = 3)+
  geom_density_2d()+ #---> Desenho de isolinhas
  #geom_smooth(method = "auto", se = TRUE, fullrange = FALSE, level = 0.95)+ # ---> Regressao linear
  labs(title = 'Grafico de dispersao entre as variaveis IMC e TAD',
       x = 'IMC - Indice de Massa Corporal',
       y = 'TAD - Tensao Arterial Diastolica')
