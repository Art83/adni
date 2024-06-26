suppressPackageStartupMessages(library(dplyr))



demo <- read.csv("D:/AZ/adni/raw/demo.csv", stringsAsFactors = F)


demo <- demo %>%
  filter(VISCODE == "sc") %>%
  select(RID, VISDATE, PTDOB, PTHAND, PTMARRY, PTEDUCAT, PTNOTRT, PTHOME, PTPLANG, PTETHCAT, PTRACCAT) %>%
  mutate(VISDATE_new = zoo::as.yearmon(as.Date(VISDATE)) ) %>%
  mutate(PTDOB_new = zoo::as.yearmon(as.Date(paste("01", PTDOB, sep = "/"), "%d/%m/%Y")) ) %>%
  mutate(age = VISDATE_new - PTDOB_new) %>%
  select(-c(VISDATE_new, PTDOB_new, VISDATE, PTDOB)) %>%
  filter(!is.na(age))
  

apply(apply(ss,2,is.na),2, sum)
apply(apply(ss,2,function(x) x < 0),2, sum)


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


apply(apply(family,2,is.na),2, sum)
apply(apply(family,2,function(x) x < 0),2, sum)


# Medical history

# Vitals
vitals <- read.csv("D:/AZ/adni/raw/medical_history/VITALS.csv", stringsAsFactors = F)


vitals <- vitals %>%
  select(RID, VISCODE, VSWEIGHT,VSWTUNIT,VSHEIGHT, VSHTUNIT, VSBPSYS, VSBPDIA, VSPULSE, VSRESP, VSTEMP, VSTMPUNT) %>%
  group_by(RID) %>%
  filter(VISCODE == "sc") %>%
  mutate(weight = case_when(VSWTUNIT == 2 ~ VSWEIGHT,
                            VSWTUNIT == 1 ~ VSWEIGHT*0.453592,
                            .default = NA)) %>%
  mutate(height = case_when(VSHTUNIT == 2 ~ VSHEIGHT,
                            VSHTUNIT == 1 ~ VSHEIGHT*2.54,
                            .default = NA)) %>%
  mutate(BMI = weight / (height)^2 * 10000 ) %>%
  mutate(temperature = case_when(VSTMPUNT == 2 ~ VSTEMP,
                            VSTMPUNT == 1 ~ ((VSTEMP-32)*5)/9,
                            .default = NA)) %>%
  select(RID, weight, height, BMI, temperature, VSBPSYS, VSBPDIA, VSPULSE, VSRESP)


apply(apply(vitals,2,is.na),2, sum)
apply(apply(vitals,2,function(x) x < 0),2, sum)

# Physical
physical <- read.csv("D:/AZ/adni/raw/medical_history/PHYSICAL.csv", stringsAsFactors = F)

physical <- physical %>%
  select(RID, VISCODE, PXGENAPP:PXABNORM) %>%
  group_by(RID) %>%
  filter(VISCODE == "sc") %>%
  select(-c(VISCODE, PXBACK,PXOTHER, PXABNORM) )

apply(apply(physical,2,is.na),2, sum)
apply(apply(physical,2,function(x) x < 0),2, sum)


# Neuroexam

neuroexam <- read.csv("D:/AZ/adni/raw/medical_history/NEUROEXM.csv", stringsAsFactors = F)

neuroexam <- neuroexam %>%
  select(RID, VISCODE, NXVISUAL:NXOTHER) %>%
  group_by(RID) %>%
  filter(VISCODE == "sc") %>%
  select(-c(NXOTHER,VISCODE, NXCONSCI) )

apply(apply(neuroexam,2,is.na),2, sum)
apply(apply(neuroexam,2,function(x) x < 0),2, sum)

# BLScheck

blscheck <- read.csv("D:/AZ/adni/raw/medical_history/BLSCHECK.csv", stringsAsFactors = F)

blscheck <- blscheck %>%
  select(RID, VISCODE, BCNAUSEA:BCOTHER) %>%
  group_by(RID) %>%
  filter(VISCODE == "bl") %>%
  select(-VISCODE)

apply(apply(blscheck,2,is.na),2, sum)
apply(apply(blscheck,2,function(x) x < 0),2, sum)

# MEDhist
medhist <- read.csv("D:/AZ/adni/raw/medical_history/MEDHIST.csv", stringsAsFactors = F)

medhist <- medhist %>%
  select(RID, VISCODE, MHSOURCE:MH19OTHR) %>%
  group_by(RID) %>%
  filter(VISCODE == "sc") %>%
  select(-c(VISCODE, MH14AALCH, MH14BALCH, MH14CALCH, MH15ADRUG, MH15BDRUG, MH16ASMOK, MH16BSMOK, MH16CSMOK))

apply(apply(medhist,2,is.na),2, sum)
apply(apply(medhist,2,function(x) x < 0),2, sum)

#Psych Q
neurobat <- read.csv("D:/AZ/adni/raw/psych_eval/NEUROBAT.csv", stringsAsFactors = F)


neurobat <- neurobat %>%
  group_by(RID) %>%
  filter(VISCODE == 'sc') %>%
  select(RID, LIMMTOTAL, LDELTOTAL)

apply(apply(neurobat,2,is.na),2, sum)


npiq <- read.csv("D:/AZ/adni/raw/psych_eval/NPIQ.csv", stringsAsFactors = F)


npiq <- npiq %>%
  group_by(RID) %>%
  filter(VISCODE == 'bl') %>%
  select(!ends_with("SEV")) %>%
  select(RID, NPIA:NPISCORE)

apply(apply(npiq,2,is.na),2, sum)


modnach <- read.csv("D:/AZ/adni/raw/psych_eval/MODHACH.csv", stringsAsFactors = F)


modnach <- modnach %>%
  group_by(RID) %>%
  filter(VISCODE == 'sc') %>%
  select(RID, HMSCORE)

apply(apply(modnach,2,is.na),2, sum)
apply(apply(modnach,2,function(x) x < 0),2, sum)


gdscale <- read.csv("D:/AZ/adni/raw/psych_eval/GDSCALE.csv", stringsAsFactors = F)


gdscale <- gdscale %>%
  group_by(RID) %>%
  filter(VISCODE == 'sc') %>%
  select(RID, GDSATIS:GDTOTAL) %>%
  mutate(across(GDSATIS:GDTOTAL, ~ replace(., . == -4 | . == -1, NA)))
  

apply(apply(gdscale,2,is.na),2, sum)



faq <- read.csv("D:/AZ/adni/raw/psych_eval/FAQ.csv", stringsAsFactors = F)


faq <- faq %>%
  group_by(RID) %>%
  filter(VISCODE == 'bl') %>%
  select(RID, FAQFINAN:FAQTOTAL) 


apply(apply(faq,2,is.na),2, sum)


adas_adni_go23 <- read.csv("D:/AZ/adni/raw/psych_eval/ADAS_ADNIGO23.csv", stringsAsFactors = F)
adas_adni1 <- read.csv("D:/AZ/adni/raw/psych_eval/ADAS_ADNI1.csv", stringsAsFactors = F)

adas_adni_go23 <- adas_adni_go23 %>%
  group_by(RID) %>%
  filter(VISCODE == 'bl') %>%
  select(RID, Q13TASKA, Q13TASKB, Q13TASKC) %>%
  rename(CONMCXLA = Q13TASKA,
         CONMCXLB = Q13TASKB,
         CONMCXLC = Q13TASKC)

adas_all <- adas_adni1 %>%
  group_by(RID) %>%
  filter(VISCODE == 'bl') %>%
  select(RID, CONMCXLA, CONMCXLB, CONMCXLC) %>%
  rbind(adas_adni_go23)

apply(apply(adas_all,2,is.na),2, sum)


# Lab data
lab.data <- read.csv("D:/AZ/adni/raw/lab_data/LABDATA.csv", stringsAsFactors = F)


cols <- c("BAT126",  "HMT10",   "HMT100",  "HMT102",  "HMT11",   "HMT12",   "HMT13",   "HMT15",   "HMT16",   "HMT17",   "HMT18",  
          "HMT19",  "HMT2",    "HMT3",    "HMT4",    "HMT40",   "HMT7",    "HMT8",    "HMT9",    "RCT1",    "RCT11",   "RCT12",  
          "RCT13",   "RCT14",   "RCT1407", "RCT1408", "RCT183",  "RCT19",   "RCT20",   "RCT29",   "RCT3",  "RCT392",  "RCT4", 
          "RCT5",    "RCT6",    "RCT8",    "RCT9")

lab.data <- lab.data %>%
  mutate(EXAMDATE = as.Date(EXAMDATE)) %>%
  mutate(across(all_of(cols), as.numeric)) %>%
  group_by(RID) %>%
  arrange(EXAMDATE, .by_group = T) %>%
  slice(1) %>%
  select(RID, cols) %>%
  mutate(across(all_of(cols), ~replace(., .<0, NA)) )


apply(apply(lab.data,2,is.na),2, sum)


# CSF
csf.1 <- read.csv("D:/AZ/adni/raw/CSF/LOCLAB.csv", stringsAsFactors = F)


csf.1 <- csf.1 %>%
  filter(PHASE != "ADNI4") %>%
  group_by(RID) %>%
  filter(VISCODE == 'bl') %>%
  select(RID, CTWHITE:GLUCOSE) %>%
  mutate(across(CTWHITE:GLUCOSE, ~ replace(., . < 0, NA)))


apply(apply(csf.1,2,is.na),2, sum)


# Peptides
tau <- read.csv("D:/AZ/adni/raw/CSF/TAU.csv", stringsAsFactors = F)

tau <- tau %>%
  group_by(RID) %>%
  filter(VISCODE2 == 'bl') %>%
  select(RID,ABETA42, TAU, PTAU)


apply(apply(tau,2,is.na),2, sum)


# Diagnosis
diag <- read.csv("D:/AZ/adni/raw/outcome.csv", stringsAsFactors = F)


diag <- diag %>%
  group_by(RID) %>%
  filter(VISCODE == 'bl') %>%
  select(RID, DIAGNOSIS)



apoe <- read.csv("D:/AZ/adni/raw/APOERES.csv", stringsAsFactors = F)


apoeres <- apoe %>% 
  mutate(apoe = if_else(APGEN1 == 4 & APGEN2 == 4, 4,
                        if_else(APGEN1 == 4 | APGEN2 == 4, 3,
                                if_else(APGEN1 == 3 | APGEN2 == 3, 2, 1))) ) %>% 
  select(RID, apoe)


main_df <- Reduce(function(x,y) merge(x,y,by="RID"), list(demo,
                                                          family,
                                                          vitals,
                                                          physical,
                                                          neuroexam,
                                                          neurobat,
                                                          #blscheck,
                                                          medhist,
                                                          gdscale,
                                                          modnach,
                                                          #npiq,
                                                          #faq,
                                                          lab.data,
                                                          #csf.1,
                                                          #tau,
                                                          apoeres,
                                                          diag))


apply(apply(main_df, 2, is.na),2,sum)


main_df <- main_df %>%
  filter(!is.na(weight),
         !is.na(temperature),
         !is.na(PTNOTRT)) %>%
  group_by(DIAGNOSIS) %>%
  mutate(across(BAT126:RCT9, ~ifelse(is.na(.), median(., na.rm=T), .))) %>%
  select(-RID) %>%
  mutate(across(everything(), as.numeric)) %>%
  mutate(DIAGNOSIS = as.factor(case_when(DIAGNOSIS == 1 ~ "Control",
                               DIAGNOSIS == 2 ~ "MCI",
                               DIAGNOSIS == 3 ~ "AZ"))) %>%
  na.omit() %>%
  ungroup() %>%
  as.data.frame()

saveRDS(main_df, "D:/AZ/adni/objects/df_imputed_not_complete_1073")


pool <- unique(c(demo$RID,
                 family$RID,
                 vitals$RID,
                 physical$RID,
                 neuroexam$RID,
                 neurobat$RID,
                 blscheck$RID,
                 medhist$RID,
                 gdscale$RID,
                 modnach$RID,
                 npiq$RID,
                 faq$RID,
                 lab.data$RID,
                 csf.1$RID,
                 tau$RID,
                 apoeres$RID,
                 diag$RID))


df.res <- data.frame(RID = pool,
                     demo = pool %in% demo$RID,
                     family = pool %in% family$RID,
                     vitals = pool %in% vitals$RID,
                     physical = pool %in% physical$RID,
                     neuroexam = pool %in% neuroexam$RID,
                     neurobat = pool %in% neurobat$RID,
                     blscheck = pool %in% blscheck$RID,
                     medhist = pool %in% medhist$RID,
                     gdscale = pool %in% gdscale$RID,
                     modnach = pool %in% modnach$RID,
                     npiq = pool %in% npiq$RID,
                     faq = pool %in% faq$RID,
                     lab.data = pool %in% lab.data$RID,
                     csf.1 = pool %in% csf.1$RID,
                     tau = pool %in% tau$RID,
                     apoe = pool %in% apoeres$RID,
                     diag = pool %in% diag$RID)


df.res.l <- do.call(cbind, lapply(df.res[,-1], function(x) ifelse(x, 1, 0)))


UpSetR::upset(as.data.frame(df.res.l), nsets = 17)



apply(df.res, 2, sum)

