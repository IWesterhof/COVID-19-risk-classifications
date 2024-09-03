################################################
## Goal: Describe COVID-19 disease burden in Norway
## Version/date: V1.0/01-08-2024
## Authos: I. Westerhof
## R-version: 4.4.1
################################################

### Clean enviroment and memory
rm(list=ls()); gc()

### 1. Load packages
library(haven); library(dplyr); library(readxl); library(readr); library(tidyr); library(data.table); library(stringr); library(ggplot2)

### 2. Set work directory
setwd("N:/durable/Ilse/2023")

### 3. Set directory for temp files
tempdir()
Sys.setenv(TMPDIR = "N:\\durable\\Ilse\\2023\\temp")
unlink(tempdir(), recursive = TRUE)
tempdir(check = TRUE)
tempdir()

### BaselineOverview
load("N:/durable/Ilse/2023/1. Data_Classifications/BaselineOverview.RData")
BaselineOverview = BaselineOverview %>% filter(Year=="2020-01-01") %>% select(-contains(c("Morbidity_FHI", "Morbidity_ECDC")))
setnames(BaselineOverview, old = "Year", new = "ReferenceDate"); gc() 
BaselineOverviewdf = BaselineOverview %>% select(id, ReferenceDate, LastVacc, Age, Gender, Morbidity_RIVM_COVIDcat) %>% mutate(Baseline = "Yes") %>% distinct()

load("1. Data_Classifications/DiseasesPPbydiseasewithcodes.RData"); gc()
BaselineOverviewDiseases <- merge(BaselineOverview, DiseasesPP, by.x="id", by.y="PASIENTLOPENUMMER", all.x=T); gc()

BaselineOverviewDiseases$ReferenceDateMIN7 =  as.Date(BaselineOverviewDiseases$ReferenceDate)-7; gc() 
BaselineOverviewDiseases = BaselineOverviewDiseases %>% # remove diseases after 7 days before infection (-1 wk)
  mutate(beforeREFmin7 = ifelse(date<=ReferenceDateMIN7, "Yes", "No")) %>% 
  mutate(beforeREFmin7 = ifelse(is.na(beforeREFmin7), "Keep", beforeREFmin7)) %>% filter(!beforeREFmin7=="No") %>%
  select(-ReferenceDateMIN7, -beforeREFmin7); gc() 

# Step 1: remove all comorbidity records after confirmation infection
BaselineOverviewDiseases <- BaselineOverviewDiseases %>% 
  mutate(BeforeInfection = ifelse(date<=ReferenceDate, "Yes", "No")) %>%
  filter(BeforeInfection=="Yes" | is.na(BeforeInfection)); gc()

# Step 2: remove all comorbidity recodes more than 5 years before confirmation infection
BaselineOverviewDiseasesCancer <- BaselineOverviewDiseases %>% 
  mutate(ToolongbeforeInfection = ifelse(as.Date(ReferenceDate)-1825 >= as.Date(date), "Yes", "No")) %>%
  filter(Morbidity_FHI_HeamatologicalCancer==1) %>%  filter(ToolongbeforeInfection=="No")

# Step 3: Remove all comorbidity recodes more than 1 years before confirmation infection, except for other cancer which should be in the last 6 months (NO class)
BaselineOverviewDiseaseslstyear <- BaselineOverviewDiseases %>% 
  mutate(ToolongbeforeInfection = ifelse(as.Date(ReferenceDate)-(365+7) >= as.Date(date), "Yes", "No")) %>%
  filter(ToolongbeforeInfection=="No" & is.na(Morbidity_FHI_OtherCancer))

# Step 4: Cancer treatment in past 6 months (NO classification)
BaselineOverviewDiseasesCancerless6 <- BaselineOverviewDiseases %>% 
  mutate(ToolongbeforeInfection = ifelse(as.Date(ReferenceDate)- (183+7) >= as.Date(date), "Yes", "No")) %>% 
  filter(ToolongbeforeInfection=="No" &  !is.na(Morbidity_FHI_OtherCancer))
BaselineOverviewDiseases = bind_rows(BaselineOverviewDiseasesCancer, BaselineOverviewDiseaseslstyear, BaselineOverviewDiseasesCancerless6)

# Step 5: Create one row 
setDT(BaselineOverviewDiseases)
morbidity_cols <- grep("^Morbidity_", names(BaselineOverviewDiseases), value = TRUE)

# Loop over each "Morbidity_" column and apply the transformation
for (col in morbidity_cols) {
  BaselineOverviewDiseases[, (col) := get(col)[!is.na(get(col))][1L], by = .(id, ReferenceDate)]
  gc()  # Run garbage collection
}

BaselineOverviewDiseases <- BaselineOverviewDiseases %>% select(id, contains("Morbidity_")) %>% distinct() 
BaselineOverviewDiseases <- merge(BaselineOverviewdf, BaselineOverviewDiseases, by="id", all=T) %>% filter(!is.na(Baseline)) %>% select(-Baseline)

# replace NA with 0
morbidity_cols <- grep("^Morbidity_", names(BaselineOverviewDiseases), value = TRUE)
BaselineOverviewDiseases[morbidity_cols] <- lapply(BaselineOverviewDiseases[morbidity_cols], function(x) { 
  x[is.na(x)] <- 0
  return(x)
})


BaselineOverviewDiseases = BaselineOverviewDiseases %>% select(-contains(c("Delivery", "preg_", "Infection", "Vacc[1:9]", "Morbidity_RIVM_COVIDcat", "Neuromuscular_Mandatory_Additional",
                 "Morbidity_ECDC_Tuberculosis_Optional", "Morbidity_ECDC_LungDisease_Mandatory_OtherRespiratoryDisorders", "Morbidity_ECDC_Renal_Optional_Renal_dialysis",                                  
                 "Morbidity_ECDC_Hypertension_Mandatory_Hypertensivechronickidneydisease", "Morbidity_ECDC_Other disorders of arteries and arterioles" )))
BaselineOverviewDiseases <- BaselineOverviewDiseases  %>% 
  mutate(Morbidity_FHI_COVIDcat =  ifelse(if_any(ends_with("_HR"), ~ . == 1) | Morbidity_FHI_OtherCancer == 1  | Morbidity_FHI_HeamatologicalCancer == 1, "High risk",
                                          ifelse(if_any(ends_with("_MR"), ~ . == 1) , "Moderate risk", "Low risk"))) %>%  
  mutate(Morbidity_FHI_COVIDcat = factor(Morbidity_FHI_COVIDcat, levels=c("High risk", "Moderate risk", "Low risk")))  %>% 
  mutate(Morbidity_ECDC_COVIDcat =  ifelse(if_any(ends_with("_Mandatory"), ~ . == 1), "High risk",
                                     ifelse(if_any(ends_with("_Optional"), ~ . == 1) , "Moderate risk", "Low risk"))) %>%  
  mutate(Morbidity_ECDC_COVIDcat = factor(Morbidity_ECDC_COVIDcat, levels=c("High risk", "Moderate risk", "Low risk"))); gc()
save(BaselineOverviewDiseases,   file="N:/durable/Ilse/2023/1. Data_Classifications/BaselineOverview2020.RData")


