library(googlesheets4)
library(tidyverse)

growth_nov17 <- read_sheet("https://docs.google.com/spreadsheets/d/182rgDMPHf_xGNtQmHtT9jk5FqsD-fq0RiLlcx57eSE8/edit#gid=1444597239", sheet = "leaf_counts_nov17")

growth_dec1 <- read_sheet("https://docs.google.com/spreadsheets/d/182rgDMPHf_xGNtQmHtT9jk5FqsD-fq0RiLlcx57eSE8/edit#gid=1444597239", sheet = "leaf_counts_dec1")

# growth_x <- uncomment and add other datasheets

all_growth <- left_join(growth_nov17, growth_dec1) #%>% 
  # left_join(., growth_x) can add additional datasheets


summary(growth_nov17)
summary(growth_dec1)
summary(all_growth)
