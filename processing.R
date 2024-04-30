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
  


# Family history
adni3_1 <- read.csv("D:/AZ/adni/raw/family/adni3_fam_2.csv", stringsAsFactors = F)
adni_r_1 <- read.csv("D:/AZ/adni/raw/family/adni1_2_go_fam_2.csv", stringsAsFactors = F)




  


adni3 <- adni3_1 %>%
  filter(VISCODE == "sc") %>%
  select(RID,MOTHDEM, MOTHAD, FATHDEM, FATHAD) %>%
  rename(FHQMOM = MOTHDEM,
         FHQMOMAD = MOTHAD,
         FHQDAD = FATHDEM,
         FHQDADAD = FATHAD) %>%
  filter(!is.na(FHQMOM),
         !is.na(FHQDAD)) %>%
  filter(FHQMOM %in% c(0,1),
         FHQDAD %in% c(0,1)) %>%
  mutate(across(c(FHQMOMAD, FHQDADAD), 
                ~ replace(., is.na(.) | . < 0, 0))) %>%
  filter(FHQMOMAD %in% c(0,1),
         FHQDADAD %in% c(0,1)) %>%
  mutate(mother = FHQMOM + FHQMOMAD,
         father = FHQDAD + FHQDADAD) %>%
  select(RID, mother, father)


adnir <- adni_r_1 %>%
  filter(VISCODE == "sc") %>%
  select(RID, FHQMOM, FHQMOMAD, FHQDAD, FHQDADAD) %>%
  filter(!is.na(FHQMOM),
         !is.na(FHQDAD)) %>%
  filter(FHQMOM %in% c(0,1),
         FHQDAD %in% c(0,1)) %>%
  mutate(across(c(FHQMOMAD, FHQDADAD), 
                ~ replace(., is.na(.) | . < 0, 0))) %>%
  filter(FHQMOMAD %in% c(0,1),
         FHQDADAD %in% c(0,1)) %>%
  mutate(mother = FHQMOM + FHQMOMAD,
         father = FHQDAD + FHQDADAD) %>%
  select(RID, mother, father)


family <- rbind(as.data.frame(adni3),
                as.data.frame(adnir))
  
  
  

