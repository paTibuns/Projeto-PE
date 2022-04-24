library(tidyverse)
library(readxl)
library(lubridate)

dfO3lvl <- read_excel('QualidadeARO3.xlsx',col_types = rep('numeric',10))
tibblO3 <- as_tibble(dfO3lvl)

#Criacao de time stamp
h_y <- 24*366 #---> total de horas em 2020
dttm <- make_datetime(year = 2020, hour = 0:(h_y-1))

tibblO3 %>%
  add_column(dttm,.before = 'Antas-Espinho') %>%
  pivot_longer(!dttm,names_to = 'Local',values_to = 'O3lvl') %>%
  filter((Local == 'Mem-Martins' | Local == 'Antas-Espinho')) %>% #&
          # (month(dttm)==1)) %>%
  ggplot(aes(x = dttm,color = Local))+
  geom_histogram()+
  #geom_freqpoly(binwidth = 90600)+
  theme_minimal()
