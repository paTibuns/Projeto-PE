library(tidyverse)
library(readxl)
library(ggrepel)

emv <- read_excel('EsperancaVida.xlsx', range = 'A9:CY70', col_names = FALSE)
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

colnames(tibblEmv) <- tibblEmv[1,]
colnames(tibblEmv)[1] <- 'Ano'
tibblEmv <- slice(tibblEmv,-1)

tibblEmv %>%
  pivot_longer(UE27_Total:CH_Mulheres,names_to = 'GS',values_to = 'EMV',values_transform = list(EMV = as.numeric)) %>%
    separate(GS,c('Grupo','Sexo'),sep = '_') %>%
  filter((Grupo == 'ES'| Grupo == 'GR'|Grupo == 'HU') &
           (between(Ano,2002,2019)) &
           (Sexo == 'Homens'|Sexo == 'Mulheres')) %>%
  ggplot(aes(x = Ano, y = EMV,Group = Grupo,colour = Grupo,shape = Sexo))+
  #ggrepel::geom_label_repel(aes(label =  max(tibblEmv$EMV)),  ---> tentativa de criar labels com os
  #                          size = 6,                              valores dos pontos
  #                          label.size = 0,
  #                          segment.color = NA)
  geom_point(size = 5, alpha = 0.7)+
  geom_smooth(se = F)+
  theme_minimal()+
  labs(title = 'Esperanca media de vida por grupo e sexo aumentou ao longo dos anos',
       caption = 'Dados obtidos de pordata.pt',
       y = 'Esperanca Media de Vida')
