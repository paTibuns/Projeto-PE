library(tidyverse)
library(readxl)
library(ggrepel)

emv <- read_excel('EsperancaVida.xlsx', range = 'A9:CY70', col_names = FALSE)
tibblEmv <- as_tibble(emv)

#Reduzir os nomes dos paises as suas iniciais e adicionar uma coluna com o sexo e pais correspondente ao valor da emv
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

colnames(tibblEmv) <- tibblEmv[1,]
colnames(tibblEmv)[1] <- 'Ano'
tibblEmv <- slice(tibblEmv,-1)

tibblEmv %>%
  #Separar a coluna dos sexos e paises em 2 (uma coluna para cada)
  pivot_longer(UE27_Total:CH_Mulheres,names_to = 'GS',values_to = 'EMV',
               values_transform = list(EMV = as.numeric)) %>%
  separate(GS,c('Grupo','Sexo'),sep = '_') %>%
  
  filter((Grupo == 'ES'| Grupo == 'GR'|Grupo == 'HU') &
           (between(Ano,2002,2019)) &
           (Sexo == 'Homens'|Sexo == 'Mulheres')) %>%
  ggplot(aes(x = Ano, y = EMV,colour = Grupo,shape = Sexo))+
  geom_point(size = 5, alpha = 0.7)+
  geom_smooth(se = F)+
  theme_minimal()+
  scale_color_manual(values=c("#ad1519", "#001489",'#008724'))+
  labs(title = 'Esperanca media de vida por grupo e sexo entre 2002 e 2019',
       caption = 'Dados obtidos de pordata.pt',
       x = 'Ano da estimativa',
       y = 'Esperanca Media de Vida [Anos]')