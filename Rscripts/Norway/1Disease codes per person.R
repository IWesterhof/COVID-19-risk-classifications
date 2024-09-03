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

################        Create function with comorbidity/risk group definitions        ################
### Overview Codes w explanations
#ICD-10 codes: https://www.icd10data.com/ICD10CM/Codes
#ICPC-2 codes: https://www.icgp.ie/speck/properties/asset/asset.cfm?type=Document&id=82773A16-10B1-4160-8DC42D8F5509EFA2&property=document&filename=ICPC2.pdf&revision=tip&mimetype=application%2Fpdf&app=icgp&disposition=inline
#ICPC-2 code number 70-99 applies to disease diagnoses; code number 1-29 applies to symptoms and complaints

RiskgroupDefinitions <- function(DiseaseOverview, mutation) { DiseaseOverview %>%
    
    mutate(diagnosekodeverk = ifelse(grepl("ICPC-2", diagnosekodeverk), "ICPC-2", diagnosekodeverk)) %>%
    mutate(Codeletter = substr(Code, 1, 1)) %>%  mutate(Codenr = substr(Code, 2, 3)) %>% mutate(Codenr = as.numeric(Codenr)) %>% 
    
# Norway definition FHI definition FHI march 2021
    mutate(Morbidity_FHI_ICD10 = ifelse(diagnosekodeverk=="ICD-10"  & (grepl("Z940|Z941|Z942|Z943|Z944|Z948", diagnose)), "Organtranspl_HR", NA)) %>%     # Organtranspl_HR (Organtransplantasjon)
    mutate(Morbidity_FHI_ICD10 = ifelse(diagnosekodeverk=="ICD-10"  & grepl("G1|G20|G21|G23|G24|G405|G610|G70|G71|F72|F73|Q90|G800|G802|G803|F840|F841|Q050|Q051|Q052|Q053|Q054|Q055|Q056", diagnose), "NeurologicalMusculoskeletalDisorders_HR", Morbidity_FHI_ICD10)) %>% 
    mutate(Morbidity_FHI_ICD10 = ifelse(diagnosekodeverk=="ICD-10"  & grepl("K704|K72", diagnose), "ChronicLiverDisease_HR", Morbidity_FHI_ICD10)) %>%     # ChronicLiverDisease_HR (Leversvikt)
    mutate(Morbidity_FHI_ICD10 = ifelse(diagnosekodeverk=="ICD-10"  & grepl ("E10|E11|E12|E13|E14", diagnose), "Diabetes_MR", Morbidity_FHI_ICD10)) %>% 
    mutate(Morbidity_FHI_ICD10 = ifelse(diagnosekodeverk=="ICD-10"  & grepl("J41|J42|J43|J44|J45|J46|J47|J84|J98|E84", diagnose), "ChronicPulmonaryDiseases_MR", Morbidity_FHI_ICD10)) %>% 
    mutate(Morbidity_FHI_ICD10 = ifelse(diagnosekodeverk=="ICD-10"  & Code=="E66", "Obesity_MR", Morbidity_FHI_ICD10))  %>%    # Obesity_MR (Fedme)
    mutate(Morbidity_FHI_ICD10 = ifelse(diagnosekodeverk=="ICD-10"  & grepl("C81|C82|C83|C84|C85|C86|C87|C88|C89|C90|C91|C92|C93|C94|C95|C96|D45|D47", diagnose),   "HeamatologicalCancer", Morbidity_FHI_ICD10)) %>%    # HeamatologicalCancer (HematologiskKreft)
    mutate(Morbidity_FHI_ICD10 = ifelse(diagnosekodeverk=="ICD-10"  & grepl ("D80|D81|D82|D83|D84", diagnose), "Immunodeficiency_HR", Morbidity_FHI_ICD10)) %>%     # Immunodeficiency_HR (Immunsviktsykdommer)
    mutate(Morbidity_FHI_ICD10 = ifelse(diagnosekodeverk=="ICD-10"  & grepl("I05|I06|I07|I08|I09|I2|I31|I32|I34|I35|I36|I37|I39|I40|I41|I42|I43|I46|I48|I49|I50", diagnose), "CardiovascularDiseases_MR", Morbidity_FHI_ICD10)) %>% 
    mutate(Morbidity_FHI_ICD10 = ifelse(diagnosekodeverk=="ICD-10"  & grepl ("I60|I61|I62|I63|I64|I691|I692|I693|I694|I698|I690", diagnose), "Stroke_MR", Morbidity_FHI_ICD10)) %>%     # Stroke_MR (Hjerneslag)
    mutate(Morbidity_FHI_ICD10 = ifelse(diagnosekodeverk=="ICD-10"  & grepl("F0|G30|G31", diagnose), "Dementia_MR", Morbidity_FHI_ICD10)) %>%    # Dementia_MR (Demens)
    mutate(Morbidity_FHI_ICD10 = ifelse(diagnosekodeverk=="ICD-10"  & grepl("C0|C1|C2|C3|C4|C5|C6|C7|C80", diagnose), "OtherCancer", Morbidity_FHI_ICD10)) %>%    # OtherCancer (AnnenAktivKreftsykdom), no icpc-2 codes used for this definition
    mutate(Morbidity_FHI_ICD10 = ifelse(diagnosekodeverk=="ICD-10"  & grepl("N183|N184|N185", diagnose), "ChronicKidneyLiverDisease_HR", Morbidity_FHI_ICD10)) %>%     # ChronicKidneyDisease_HR (Nyresvikt)
    mutate(Morbidity_FHI_ICD10 = ifelse(diagnosekodeverk=="ICD-10"  & grepl("G35|M05|M08|M06|M07|M09|M13|M14|K50|K51", diagnose), "CompromisedImmuneFunction_MR", Morbidity_FHI_ICD10)) %>%     # CompromisedImmuneFunction_MR (NedsattImmunforsvar), no icpc-2 codes used for this definition

 # Norway definition FHI definition FHI march 2021 - ICPC-2
    mutate(Morbidity_FHI_ICPC2 = ifelse(diagnosekodeverk=="ICPC-2"  & grepl("T89|T90", diagnose), "Diabetes_MR", NA)) %>% 
    mutate(Morbidity_FHI_ICPC2 = ifelse(diagnosekodeverk=="ICPC-2"  & grepl("R95|R96", diagnose), "ChronicPulmonaryDiseases_MR", Morbidity_FHI_ICPC2)) %>% 
    mutate(Morbidity_FHI_ICPC2 = ifelse(diagnosekodeverk=="ICPC-2"  & Code=="T82", "Obesity_MR", Morbidity_FHI_ICPC2))  %>%    # Obesity_MR (Fedme)
    mutate(Morbidity_FHI_ICPC2 = ifelse(diagnosekodeverk=="ICPC-2"  & grepl ("K74|K75|K76|K77|K78|K82|K83|K87", diagnose), "CardiovascularDiseases_MR", Morbidity_FHI_ICPC2)) %>% 
    mutate(Morbidity_FHI_ICPC2 = ifelse(diagnosekodeverk=="ICPC-2"  & grepl("K90","K91", diagnose), "Stroke_MR", Morbidity_FHI_ICPC2)) %>%     # Stroke_MR (Hjerneslag)
    mutate(Morbidity_FHI_ICPC2 = ifelse(diagnosekodeverk=="ICPC-2"  & Code=="P70", "Dementia_MR", Morbidity_FHI_ICPC2)) %>%  

 # ECDC definition - ICD10
    mutate(Morbidity_ECDC_ICD10 = ifelse((diagnosekodeverk=="ICD-10" & Code=="J45"), "Asthma_Mandatory", NA)) %>% 
    mutate(Morbidity_ECDC_ICD10 = ifelse((diagnosekodeverk=="ICD-10" & (Codeletter=="D" & Codenr>=50 & Codenr <=64)), "Anaemia_Optional", Morbidity_ECDC_ICD10)) %>% 
    mutate(Morbidity_ECDC_ICD10 = ifelse(diagnosekodeverk=="ICD-10" & (grepl("Q8901|Q206|Z9081", diagnose)), "Asplenia_Optional", Morbidity_ECDC_ICD10)) %>% 
    mutate(Morbidity_ECDC_ICD10 = ifelse((diagnosekodeverk=="ICD-10" & grepl("K70|K72|K73|K74|K754|K769", diagnose)), "ChronicLiver_Optional", Morbidity_ECDC_ICD10)) %>% 
    mutate(Morbidity_ECDC_ICD10 = ifelse((diagnosekodeverk=="ICD-10" & grepl("I05|I06|I07|I08|I09|I11|I13|I20|I21|I22|I23|I24|I25|I27|Q20|Q21|Q22|Q23|Q24|A5201|B376|B5881|I26|I970|I971|R001|T81718A|T8172XA|T82817A|T82818A|Q251|Q252|Q260|Q261|Q268|Q874|R011|R02", diagnose)  |
                                      (Codeletter=="I" & Codenr>=30 & Codenr <=51)), "CardiovascularDiseases_Mandatory", Morbidity_ECDC_ICD10)) %>%
    mutate(Morbidity_ECDC_ICD10 = ifelse(diagnosekodeverk=="ICD-10" & grepl("E10|E11", diagnose), "Diabetes_Mandatory", Morbidity_ECDC_ICD10)) %>% 
    mutate(Morbidity_ECDC_ICD10 = ifelse((diagnosekodeverk=="ICD-10" & grepl("I10|I15|I973|I270", diagnose)), "Hypertension_Mandatory", Morbidity_ECDC_ICD10)) %>% 
    mutate(Morbidity_ECDC_ICD10 = ifelse((diagnosekodeverk=="ICD-10" & (grepl("E6601|E662|E669", diagnose))), "Obesity_Mandatory", Morbidity_ECDC_ICD10)) %>% 
    mutate(Morbidity_ECDC_ICD10 = ifelse(diagnosekodeverk=="ICD-10" & grepl("B20|D80|D81|D82|D83|D84|Z21|Z94|D898|D899", diagnose), "Immunodeficiency_Mandatory", Morbidity_ECDC_ICD10)) %>%  
    mutate(Morbidity_ECDC_ICD10 = ifelse(diagnosekodeverk=="ICD-10" & grepl("G70|G737", diagnose), "Neuromuscular_Mandatory", Morbidity_ECDC_ICD10)) %>% 
    mutate(Morbidity_ECDC_ICD10 = ifelse((diagnosekodeverk=="ICD-10" & (grepl("M1030|N200|N289", diagnose) | (Codeletter=="N" & Codenr>=00 & Codenr <=19))) , "Renal_Optional", Morbidity_ECDC_ICD10)) %>% 
    mutate(Morbidity_ECDC_ICD10 = ifelse((diagnosekodeverk=="ICD-10" & grepl("F01|F03|F05|G30|G31|G91|G94", diagnose)), "Dementia_Optional", Morbidity_ECDC_ICD10)) %>% 
    mutate(Morbidity_ECDC_ICD10 = ifelse((diagnosekodeverk=="ICD-10" & grepl("G93|I69|I6783", diagnose)), "Stroke_Optional", Morbidity_ECDC_ICD10)) %>% 
    mutate(Morbidity_ECDC_ICD10 = ifelse(diagnosekodeverk=="ICD-10" & ((Codeletter=="M" & (Codenr>=30 & Codenr <=34)) | grepl("M350|M355|M358|M359|M05|M06|M08|M120", diagnose)), "Rheumatologic_Optional", Morbidity_ECDC_ICD10)) %>% 
    mutate(Morbidity_ECDC_ICD10 = ifelse((diagnosekodeverk=="ICD-10" & (Codeletter=="C" & Codenr>=00 & Codenr <=99)), "Cancer_Mandatory", Morbidity_ECDC_ICD10)) %>% 
    mutate(Morbidity_ECDC_ICD10 = ifelse((diagnosekodeverk=="ICD-10" & ((Codeletter=="J" & ((Codenr>=40 & Codenr <=47) | (Codenr>=60 & Codenr <=94))) |
                                                                    grepl("A15|J96|J99|J182|M3481|M0510", diagnose))), "LungDisease_Mandatory", Morbidity_ECDC_ICD10)) %>% 
    mutate(Morbidity_ECDC_ICD10 = ifelse((diagnosekodeverk=="ICD-10" & (Codeletter=="A" & Codenr>=15 & Codenr <=19)), "Tuberculosis_Optional", Morbidity_ECDC_ICD10))  %>% 
    mutate(Morbidity_ECDC_ICD10 = ifelse(diagnosekodeverk=="ICD-10" & grepl("Q90", diagnose), "DownSyndrome_Optional", Morbidity_ECDC_ICD10)) 
        }

################## KUHR (Primary care)        ################
table_path <- "N:/durable/VAC4EU datasets/Delivery Feb-May 2023/KUHR_complete/"
files <- list.files(path = table_path, pattern = ".dsv", full.names=TRUE)
files <- files[grepl("201[5-9]|202[0-1]", basename(files))] #  TO load years 2015-2021

# Read all files and combine into a single data table
df <- rbindlist(lapply(files, fread), use.names = TRUE, fill = TRUE); gc() #430684869 records

# Remove irrelevant columns and rows
df <- df[DIAGNOSEKODEVERK %in% c("ICD-10", "ICPC-2", "ICPC-2B") & !(is.na(DIAGNOSER) |DIAGNOSER=="")]; gc() #317043566 records
df <- df[, -c(4,7,12:14)]; gc()

# Rename variables
setnames(df, old = c("DIAGNOSER", "DIAGNOSEKODEVERK"), new = c("diagnose", "diagnosekodeverk"))
df[diagnosekodeverk == "ICPC-2B", diagnosekodeverk := "ICPC-2"] 

# add records with multiple diagnosis codes as separate rows
dfmultcodes = df[grepl(",", df$diagnose), ]; gc()  # 41278663
save(dfmultcodes,   file="1. Data_Classifications/KUHRdfmultcodes.RData"); rm(dfmultcodes); gc()

# format codes and remove irrelevant codes.  
df = df %>%    mutate(Code = substr(diagnose, 1, 3), Codeletter = substr(diagnose, 1, 1), Codenr = substr(diagnose, 2, 3), Codenr = as.numeric(Codenr))
df <- df[!(grepl("ICD-10", diagnosekodeverk) & grepl("S|T|L|O|P|U|R", diagnose))]; gc()
df <- df[!(grepl("ICPC", diagnosekodeverk) & Codenr<70)]; gc() # 220299799

## add chronic conditions to codes
df <- RiskgroupDefinitions(df, mutation); gc() # 1.2 hours
df <- df[!(is.na(Morbidity_ECDC_ICD10) & is.na(Morbidity_FHI_ICPC2) & is.na(Morbidity_FHI_ICD10))]; gc() # 29871814
df = df %>%     mutate(Morbidity_ECDCValue = ifelse(!is.na(Morbidity_ECDC_ICD10), 1, NA), 
                       Morbidity_FHI_ICPCValue = ifelse(!is.na(Morbidity_FHI_ICPC2), 1, NA), 
                       Morbidity_FHI_ICD10Value = ifelse(!is.na(Morbidity_FHI_ICD10), 1, NA)); gc()
df2 = df %>%     
  pivot_wider(names_from = Morbidity_FHI_ICD10, values_from = Morbidity_FHI_ICD10Value, names_prefix="Morbidity_FHI_", values_fill = NA) %>%  
  pivot_wider(names_from = Morbidity_FHI_ICPC2, values_from = Morbidity_FHI_ICPCValue, names_prefix="Morbidity_FHI_ICPC2_", values_fill = NA) %>%  
  pivot_wider(names_from = Morbidity_ECDC_ICD10, values_from = Morbidity_ECDCValue, names_prefix="Morbidity_ECDC_", values_fill = NA); gc() # remove memory

ref_date_2023 <- read_csv("N:/durable/vac4eu/ref_date_2023.csv") %>% distinct()
df = merge(df, ref_date_2023, by.x="PASIENTLOPENUMMER", by.y="lnr", all=T)    %>% filter(!(is.na(PASIENTLOPENUMMER)|is.na(DIFFERANSEDAGER))) %>% mutate(date=ref_date+DIFFERANSEDAGER) %>% select(-ref_date, -DIFFERANSEDAGER); gc()
KUHR = df %>% select(-ends_with("_NA"))
save(KUHR,   file="1. Data_Classifications/KUHRbydisease.RData");gc()

## KUHR multicodes
# Because of memory, we need to run the cells with multiple codes seperately.
KUHRmultcodes <- separate_rows(KUHRmultcodes, diagnose, sep = ", "); gc() # 2.1 hours
KUHRmultcodes <- separate_rows(KUHRmultcodes, diagnose, sep = ","); gc() # 1,1 hours; 95,806,649 rows

# format codes and remove irrelevant codes.  
KUHRmultcodes = KUHRmultcodes %>%    mutate(Code = substr(diagnose, 1, 3), Codeletter = substr(diagnose, 1, 1), Codenr = substr(diagnose, 2, 3), Codenr = as.numeric(Codenr))
KUHRmultcodes <- KUHRmultcodes %>% filter(!(grepl("ICD-10", diagnosekodeverk) & grepl("S|T|L|O|P|U|R", diagnose))); gc(); # 92,185,828
KUHRmultcodes <- KUHRmultcodes %>% filter(!(grepl("ICPC", diagnosekodeverk) & Codenr<70)); gc() # 69,956,909

## add chronic conditions to codes
KUHRmultcodes <- RiskgroupDefinitions(KUHRmultcodes, mutation); gc() # 21 minutes
KUHRmultcodes <- KUHRmultcodes %>% filter(!(is.na(Morbidity_ECDC_ICD10) & is.na(Morbidity_FHI_ICPC2) & is.na(Morbidity_FHI_ICD10))); gc() # 9,927,885

KUHRmultcodes = KUHRmultcodes %>%     mutate(Morbidity_ECDCValue = ifelse(!is.na(Morbidity_ECDC_ICD10), 1, NA), 
                       Morbidity_FHI_ICPCValue = ifelse(!is.na(Morbidity_FHI_ICPC2), 1, NA), 
                       Morbidity_FHI_ICD10Value = ifelse(!is.na(Morbidity_FHI_ICD10), 1, NA)); gc()
KUHRmultcodes = KUHRmultcodes %>%     
  pivot_wider(names_from = Morbidity_FHI_ICD10, values_from = Morbidity_FHI_ICD10Value, names_prefix="Morbidity_FHI_", values_fill = NA) %>%  
  pivot_wider(names_from = Morbidity_FHI_ICPC2, values_from = Morbidity_FHI_ICPCValue, names_prefix="Morbidity_FHI_ICPC2_", values_fill = NA) %>%  
  pivot_wider(names_from = Morbidity_ECDC_ICD10, values_from = Morbidity_ECDCValue, names_prefix="Morbidity_ECDC_", values_fill = NA); gc() # remove memory

ref_date_2023 <- read_csv("N:/durable/vac4eu/ref_date_2023.csv") %>% distinct()
KUHRmultcodes = merge(KUHRmultcodes, ref_date_2023, by.x="PASIENTLOPENUMMER", by.y="lnr", all=T)    %>% filter(!(is.na(PASIENTLOPENUMMER)|is.na(DIFFERANSEDAGER))) %>% mutate(date=ref_date+DIFFERANSEDAGER) %>% select(-ref_date, -DIFFERANSEDAGER); gc()
KUHRmultcodes = KUHRmultcodes %>% select(-ends_with("_NA"))
save(KUHRmultcodes,   file="1. Data_Classifications/KUHRdfmultcodes.RData"); gc()
KUHRmultcodes

################        NPR (Primary care & hospitalizations)        ################
#NPR <- read_delim("N:/durable/VAC4EU datasets/Delivery Feb-May 2023/NPR/22_8877_prosedyre_ATC_som.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
Morbiditycodes <- read_delim("N:/durable/VAC4EU datasets/Delivery Feb-May 2023/NPR/22_8877_Tilstandskoder_som.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
dates <- read_delim("N:/durable/VAC4EU datasets/Delivery Feb-May 2023/NPR/22_8877_Aktivitetsfil_SOM.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
  select(lopenr, oppholdLopenr, differansedager_InnDato ) %>% rename("oppholdlopenr" ="oppholdLopenr")

#PDB2887_NPR_03_20_30285_SOM_episoder_202207 <- read_dta("N:/durable/Stata_format/PDB2887_NPR_03_20_30285_SOM_episoder_202207.dta") %>% select(id, kobl_nokkel_origional, inn_dato)
NPR <- merge(dates, Morbiditycodes, by=c("lopenr", "oppholdlopenr"), all=T); rm(dates, Morbiditycodes); gc()
NPR <- merge(NPR, ref_date_2023, by.x="lopenr", by.y="lnr", all=T)  %>% mutate(date = ref_date+differansedager_InnDato)%>% select(-ref_date, -differansedager_InnDato) %>% filter(!(is.na(oppholdlopenr)|is.na(kode))); gc()
 
NPR <- NPR %>%  filter(date>"2015-01-01") %>% distinct() # 31353205 records 
setnames(NPR, old = c("kode", "kodenavn"), new = c("diagnose", "diagnosekodeverk"))
  
NPR <- NPR %>%    mutate(Code = substr(diagnose, 1, 3), Codeletter = substr(diagnose, 1, 1), Codenr = substr(diagnose, 2, 3),Codenr = as.numeric(Codenr)); gc()
NPR <- RiskgroupDefinitions(NPR, mutation); gc() 
NPR <- NPR %>%      
  filter(!(is.na(Morbidity_ECDC_ICD10) & is.na(Morbidity_FHI_ICPC2) & is.na(Morbidity_FHI_ICD10))) %>%
  mutate(Morbidity_ECDCValue = ifelse(!is.na(Morbidity_ECDC_ICD10), 1, NA), 
         Morbidity_FHI_ICPCValue = ifelse(!is.na(Morbidity_FHI_ICPC2), 1, NA), 
         Morbidity_FHI_ICD10Value = ifelse(!is.na(Morbidity_FHI_ICD10), 1, NA)) %>%  
  pivot_wider(names_from = Morbidity_FHI_ICD10, values_from = Morbidity_FHI_ICD10Value, names_prefix="Morbidity_FHI_", values_fill = NA) %>%  
  pivot_wider(names_from = Morbidity_FHI_ICPC2, values_from = Morbidity_FHI_ICPCValue, names_prefix="Morbidity_FHI_ICPC2_", values_fill = NA) %>%  
  pivot_wider(names_from = Morbidity_ECDC_ICD10, values_from = Morbidity_ECDCValue, names_prefix="Morbidity_ECDC_", values_fill = NA); rm(dfmultcodes)
gc() # remove memory

NPR = NPR %>% rename(PASIENTLOPENUMMER = lopenr) %>% select(-ends_with("_NA"))# 20631618
save(NPR,   file="1. Data_Classifications/NPRbydisease.RData"); gc()


### COMBINE KUHR and NPR codes
DiseasesPP <- bind_rows(KUHR, KUHRmultcodes, NPR); gc() 
rm(KUHR, NPR); gc()
DiseasesPP = DiseasesPP %>% select(-ends_with("_NA")); gc() 

save(DiseasesPP,   file="1. Data_Classifications/DiseasesPPbydiseasewithcodes.RData")
##############################################   End   ############################################## 