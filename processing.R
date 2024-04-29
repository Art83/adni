suppressPackageStartupMessages(library(dplyr))



demo <- read.csv("D:/AZ/adni/raw/demo.csv", stringsAsFactors = F)


ss <- demo %>%
  filter(VISCODE == "sc") %>%
  select(RID, VISDATE, PTDOB, PTHAND, PTMARRY, PTEDUCAT, PTNOTRT, PTHOME, PTPLANG, PTETHCAT, PTRACCAT) %>%
  mutate(VISDATE_new = zoo::as.yearmon(as.Date(VISDATE)) ) %>%
  mutate(PTDOB_new = zoo::as.yearmon(as.Date(paste("01", PTDOB, sep = "/"), "%d/%m/%Y")) ) %>%
  mutate(age = VISDATE_new - PTDOB_new) %>%
  select(-c(VISDATE_new, PTDOB_new, VISDATE, PTDOB)) %>%
  filter(!is.na(age))
  


apply(ss, 2, table)  
apply(apply(ss, 2, is.na), 2, sum)  
