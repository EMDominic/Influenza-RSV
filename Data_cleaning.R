rm(list = ls())
# Libraries ---------------------------------------------------------------

library(here)
library(tidyverse)

# Import raw data ---------------------------------------------------------

COD1997_2016 <- read_csv(here("COD1997!2016!COD1997!2016_F1.csv"))

# Correct miscoding of death type -----------------------------------------

COD1997_2016_p95 <- COD1997_2016 %>% 
  filter(CauseA == "P95" | CauseB == "P95" | CauseC == "P95" | CauseD == "P95") %>% 
  select(DeathYear, DeathType)


