library(tidyverse)
library(readxl)

dfO3lvl <- read_excel('QualidadeARO3.xlsx',col_types = rep('numeric',10))
tibblO3 <- as_tibble(dfO3lvl)