library(tidyverse)
library(readxl)

o3lvl <- read_excel('QualidadeARO3.xlsx',col_types = rep('numeric',10))
tibblo3 <- as_tibble(o3lvl)

