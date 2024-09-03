## Goal: Define risk categories for COVID-19, incl medication data. CoviChron study. Year 2020. 
## Version/date: V1.0/11-10-2023
## Authos: A.R. de Boer
## R-version: 4.2.3

#####################
### Load packages ###
#####################
library(data.table)
library(lubridate)
library(haven)

############################
### Define set variables ###
############################

# Define the baseline date of the cohort.
indexdate <- as.IDate("2020-01-01")

########################
### Load data cohort ###
########################

hosp1 <- fread("H:/Data/MedicalHistory2020.csv")
setkey(hosp1, RINPERSOON)

age <- fread("H:/Data/Bevolking2020.csv")
setkey(age, RINPERSOON)

#############################################################################
### Define categories in Risk classification: NL Medische Hoogrisicogroep ###
#############################################################################
# Add age to hosp.
age[, c("RINPERSOONS", "datedeath", "datebirth", "agecat") := NULL]
hosp <- age[hosp1]

rm(hosp1, age)

# We don't include years before 5 years of the indexdate, remove these.
# hosp1 <- hosp[!opndat < as.IDate(as.Date(indexdate) - years(5)),]
# Also we don't need admissions after the indexdate, remove these.
hosp2 <- hosp[!opndat >= indexdate,]

rm(hosp)

# How many individuals are in the hosp dataset?
length(unique(hosp2$RINPERSOON)) 

# Netherlands high risk group
hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% "C92|C93|C94|C95|C96|C81|C82|C83|C84|C85|C86|C88|C91|C90", NLHoog_hemmal := 1]
# table(hosp2$NLHoog_hemmal) 
# length(unique(hosp2[NLHoog_hemmal == 1, RINPERSOON])) 

hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% "D570|D571|D572", NLHoog_sikkel := 1]
# table(hosp2$NLHoog_sikkel) 
# length(unique(hosp2[NLHoog_sikkel == 1, RINPERSOON])) 

hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% "N185", NLHoog_dial := 1]
# table(hosp2$NLHoog_dial)      
# length(unique(hosp2[NLHoog_dial == 1, RINPERSOON])) 

hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% "Z940|Z941|Z942|Z943|Z944|Z948", NLHoog_trans := 1]
# table(hosp2$NLHoog_trans) 
# length(unique(hosp2[NLHoog_trans == 1, RINPERSOON])) 

hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% "D830|D831|D832|D838|D839|D81|D800", NLHoog_aanaf := 1]
# table(hosp2$NLHoog_aanaf)    
# length(unique(hosp2[NLHoog_aanaf == 1, RINPERSOON])) 

hosp2[opndat >= as.IDate(as.Date(indexdate) - months(6)) & LBZIcd10diag %ilike% paste0("C", formatC(0:75, width=2, flag="0"), sep = "", collapse = "|"), NLHoog_kank := 1]
# table(hosp2$NLHoog_kank)  
# length(unique(hosp2[NLHoog_kank == 1, RINPERSOON])) 

hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% "Q90", NLHoog_down := 1]
# table(hosp2$NLHoog_down)  
# length(unique(hosp2[NLHoog_down == 1, RINPERSOON])) 

# N0 ICD-codes for this category.
hosp2[, NLHoog_immunosup := 0]

hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% "E66", NLHoog_obes := 1]
# table(hosp2$NLHoog_obes)  
# length(unique(hosp2[NLHoog_obes == 1, RINPERSOON])) 

# Netherlands medium risk group
hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% "Z950|I00|I01|I02|I05|I06|I07|I09|C38|C49.3|D511|D213|Q20|Q21|Q22|Q23|Q24|Q25|Q26|Q27|Q28|P293|I20|I21|I22|I23|I24|I25|I50|I48|I47|I495|I08|I34|I35|I36|I37|I456|I458|I42|I498", NLMed_cvd := 1]
hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% paste0("I", 30:52,  sep = "", collapse = "|"), NLMed_cvd := 1]
# table(hosp2$NLMed_cvd)  
# length(unique(hosp2[NLMed_cvd == 1, RINPERSOON])) 

hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% paste0("E", 10:14,  sep = "", collapse = "|"), NLMed_DM := 1]
# table(hosp2$NLMed_DM)  
# length(unique(hosp2[NLMed_DM == 1, RINPERSOON])) 

hosp2[opndat >= as.IDate(as.Date(indexdate) - years(1)) & LBZIcd10diag %ilike% "Z948", NLMed_beentrans := 1]
# table(hosp2$NLMed_beentrans)  
# length(unique(hosp2[NLMed_beentrans == 1, RINPERSOON])) 

hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% paste0("B", 20:24,  sep = "", collapse = "|"), NLMed_HIV := 1]
# table(hosp2$NLMed_HIV)  
# length(unique(hosp2[NLMed_HIV == 1, RINPERSOON])) 

hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% "Z940|Z941|Z942|Z943|Z944|Z948|B170|B180|B181|K51|K50|M315|M316|M353|E24|E26|E27|E25", NLMed_verwee := 1]
hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% paste0("C", 81:96,  sep = "", collapse = "|"), NLMed_verwee := 1]
hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% paste0("D", 50:89,  sep = "", collapse = "|"), NLMed_verwee := 1]
hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% paste0("K", 70:77,  sep = "", collapse = "|"), NLMed_verwee := 1]
hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% paste0("M", formatC(0:14, width=2, flag="0"), sep = "", collapse = "|"), NLMed_verwee := 1]
hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% paste0("N", formatC(0:39, width=2, flag="0"), sep = "", collapse = "|"), NLMed_verwee := 1]
# table(hosp2$NLMed_verwee)  
# length(unique(hosp2[NLMed_verwee == 1, RINPERSOON])) 

hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% paste0("N", formatC(0:39, width=2, flag="0"), sep = "", collapse = "|"), NLMed_nier := 1]
# table(hosp2$NLMed_nier)  
# length(unique(hosp2[NLMed_nier == 1, RINPERSOON])) 

hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% "A15|A16|A17|A18|A19|D86|C34|C30|C31|C32|C33|C39|Q30|Q31|Q32|Q33|Q34|E84", NLMed_lucht := 1]
hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% paste0("J", formatC(0:99, width=2, flag="0"), sep = "", collapse = "|"), NLMed_lucht := 1]
# table(hosp2$NLMed_lucht)  
# length(unique(hosp2[NLMed_lucht == 1, RINPERSOON])) 

hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% paste0("G", formatC(0:99, width=2, flag="0"), sep = "", collapse = "|"), NLMed_neur := 1]
hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% paste0("I", 60:69,  sep = "", collapse = "|"), NLMed_neur := 1]
# table(hosp2$NLMed_neur)  
# length(unique(hosp2[NLMed_neur == 1, RINPERSOON])) 

hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% "E66", NLMed_obes := 1]
# table(hosp2$NLMed_obes)  
# length(unique(hosp2[NLMed_obes == 1, RINPERSOON])) 

hosp2[age < 60 & opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% "F106|F03|F00|F01", NLMed_demen := 1]
# table(hosp2$NLMed_demen)  
# length(unique(hosp2[NLMed_demen == 1, RINPERSOON])) 

hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% "Z962", NLMed_coch := 1]
# table(hosp2$NLMed_coch)  
# length(unique(hosp2[NLMed_coch == 1, RINPERSOON])) 

# ECDC high risk group
hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% "J45", ECDC_asthma := 1]
# table(hosp2$ECDC_asthma)      
# length(unique(hosp2[ECDC_asthma == 1, RINPERSOON]))

hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% "B20|D80|D81|D82|D83|D84|D898|D899|Z21|Z94", ECDC_immu := 1]
# table(hosp2$ECDC_immu)     
# length(unique(hosp2[ECDC_immu == 1, RINPERSOON]))

hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% paste0("C", formatC(0:96, width=2, flag="0"), sep = "", collapse = "|"), ECDC_kank := 1]
# table(hosp2$ECDC_kank)     
# length(unique(hosp2[ECDC_kank == 1, RINPERSOON]))

hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% "E10|E11", ECDC_DM := 1]
# table(hosp2$ECDC_DM)      
# length(unique(hosp2[ECDC_DM == 1, RINPERSOON]))

hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% "A5201|B376|B5881|I05|I06|I07|I08|I09|I11|I13|I20|I21|I22|I23|I24|I25|I2609|I269|I27|I970|I971|R001|Q20|Q21|Q22|Q23|Q24|Q251|Q252|Q260|Q261|Q268|Q874|R011|R012", ECDC_cvd := 1]
hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% paste0("I", 30:51,  sep = "", collapse = "|"), ECDC_cvd := 1]
# table(hosp2$ECDC_cvd)     
# length(unique(hosp2[ECDC_cvd == 1, RINPERSOON]))

hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% "I10|I15|I973|I270", ECDC_hyp := 1]
# table(hosp2$ECDC_hyp)      
# length(unique(hosp2[ECDC_hyp == 1, RINPERSOON]))

hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% "A15|J96|J99|J182|M3481|M0510", ECDC_lung := 1]
hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% paste0("J", 40:47,  sep = "", collapse = "|"), ECDC_lung := 1]
hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% paste0("J", 60:94,  sep = "", collapse = "|"), ECDC_lung := 1]
# table(hosp2$ECDC_lung)      
# length(unique(hosp2[ECDC_lung == 1, RINPERSOON]))

hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% "E6601|E662|E669", ECDC_obe := 1]
# table(hosp2$ECDC_obe)      
# length(unique(hosp2[ECDC_obe == 1, RINPERSOON])) 

# ECDC medium risk group
hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% paste0("D", 50:64,  sep = "", collapse = "|"), ECDC_anaemia := 1]
# table(hosp2$ECDC_anaemia)     
# length(unique(hosp2[ECDC_anaemia == 1, RINPERSOON])) 

hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% "Q8901|Q206|Z9081", ECDC_asplenia := 1]
# table(hosp2$ECDC_asplenia)        
# length(unique(hosp2[ECDC_asplenia == 1, RINPERSOON])) 

hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% "K70|K72|K73|K74|K754|K769", ECDC_liver := 1]
# table(hosp2$ECDC_liver)       
# length(unique(hosp2[ECDC_liver == 1, RINPERSOON]))

hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% "F01|F03|F05|G30|G31|G91|G94", ECDC_dem := 1]
# table(hosp2$ECDC_dem)        
# length(unique(hosp2[ECDC_dem == 1, RINPERSOON])) 

hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% "G7000|F7001|G702|G7080|G7081|G709|G7089|G737", ECDC_neur := 1]
# table(hosp2$ECDC_neur)        
# length(unique(hosp2[ECDC_neur == 1, RINPERSOON])) 

hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% "M1030|N200|N289", ECDC_ren := 1]
hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% paste0("N", formatC(0:19, width=2, flag="0"), sep = "", collapse = "|"), ECDC_ren := 1]
# table(hosp2$ECDC_ren)      
# length(unique(hosp2[ECDC_ren == 1, RINPERSOON]))

hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% "M30|M31|M32|M33|M34|M350|M355|M358|M359|M05|M06|M08|M1200", ECDC_reu := 1]
# table(hosp2$ECDC_reu)     
# length(unique(hosp2[ECDC_reu == 1, RINPERSOON])) 

hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% "G93|I6783|I69", ECDC_cva := 1]
# table(hosp2$ECDC_cva)     
# length(unique(hosp2[ECDC_cva == 1, RINPERSOON]))

hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & LBZIcd10diag %ilike% paste0("A", 15:19,  sep = "", collapse = "|"), ECDC_tbc := 1]
# table(hosp2$ECDC_tbc)      
# length(unique(hosp2[ECDC_tbc == 1, RINPERSOON]))

# Collaps hosp per RINPERSOON.
hospcol <- hosp2[, !c("opndat", "LBZIdopname", "LBZIcd10diag", "age")]
# Change NA's to zero's.
setnafill(hospcol, fill = 0)
hospcol <- hospcol[, lapply(.SD, max, na.rm = T), by = RINPERSOON]
# hospcol <- aggregate(. ~ RINPERSOON, hospcol, max)
setkey(hospcol, RINPERSOON)
length(unique(hospcol$RINPERSOON))

# Transform hosp from long to wide to define NLHoog_neur
hospneur <- hosp2[opndat >= as.IDate(as.Date(indexdate) - years(5)) & (LBZIcd10diag %ilike% paste0("G", formatC(0:99, width=2, flag="0"), sep = "", collapse = "|") | LBZIcd10diag %ilike% "R06"), c("RINPERSOON", "opndat", "LBZIcd10diag")]

rm(hosp2)

hospneur[, number := seq_len(.N), by = .(RINPERSOON, opndat)]
hospneur[, number2 := paste0("var", number)]
hospwide <- dcast(hospneur, RINPERSOON + opndat ~ number2, value.var =  c("LBZIcd10diag"))

rm(hospneur)

# Define on wide format.
hospwide[var1 %ilike% paste0("G", formatC(0:99, width=2, flag="0"), sep = "", collapse = "|"), G_cat := 1]
hospwide[var2 %ilike% paste0("G", formatC(0:99, width=2, flag="0"), sep = "", collapse = "|"), G_cat := 1]
hospwide[var3 %ilike% paste0("G", formatC(0:99, width=2, flag="0"), sep = "", collapse = "|"), G_cat := 1]
hospwide[var4 %ilike% paste0("G", formatC(0:99, width=2, flag="0"), sep = "", collapse = "|"), G_cat := 1]
hospwide[var5 %ilike% paste0("G", formatC(0:99, width=2, flag="0"), sep = "", collapse = "|"), G_cat := 1]
hospwide[var6 %ilike% paste0("G", formatC(0:99, width=2, flag="0"), sep = "", collapse = "|"), G_cat := 1]
hospwide[var7 %ilike% paste0("G", formatC(0:99, width=2, flag="0"), sep = "", collapse = "|"), G_cat := 1]
hospwide[var8 %ilike% paste0("G", formatC(0:99, width=2, flag="0"), sep = "", collapse = "|"), G_cat := 1]
hospwide[var9 %ilike% paste0("G", formatC(0:99, width=2, flag="0"), sep = "", collapse = "|"), G_cat := 1]
hospwide[var10 %ilike% paste0("G", formatC(0:99, width=2, flag="0"), sep = "", collapse = "|"), G_cat := 1]
hospwide[var11 %ilike% paste0("G", formatC(0:99, width=2, flag="0"), sep = "", collapse = "|"), G_cat := 1]
hospwide[var12 %ilike% paste0("G", formatC(0:99, width=2, flag="0"), sep = "", collapse = "|"), G_cat := 1]
hospwide[var13 %ilike% paste0("G", formatC(0:99, width=2, flag="0"), sep = "", collapse = "|"), G_cat := 1]
hospwide[var14 %ilike% paste0("G", formatC(0:99, width=2, flag="0"), sep = "", collapse = "|"), G_cat := 1]
hospwide[var15 %ilike% paste0("G", formatC(0:99, width=2, flag="0"), sep = "", collapse = "|"), G_cat := 1]
hospwide[var16 %ilike% paste0("G", formatC(0:99, width=2, flag="0"), sep = "", collapse = "|"), G_cat := 1]
hospwide[var17 %ilike% paste0("G", formatC(0:99, width=2, flag="0"), sep = "", collapse = "|"), G_cat := 1]
hospwide[var18 %ilike% paste0("G", formatC(0:99, width=2, flag="0"), sep = "", collapse = "|"), G_cat := 1]
hospwide[var19 %ilike% paste0("G", formatC(0:99, width=2, flag="0"), sep = "", collapse = "|"), G_cat := 1]
hospwide[var20 %ilike% paste0("G", formatC(0:99, width=2, flag="0"), sep = "", collapse = "|"), G_cat := 1]
hospwide[var21 %ilike% paste0("G", formatC(0:99, width=2, flag="0"), sep = "", collapse = "|"), G_cat := 1]
hospwide[var22 %ilike% paste0("G", formatC(0:99, width=2, flag="0"), sep = "", collapse = "|"), G_cat := 1]
hospwide[var23 %ilike% paste0("G", formatC(0:99, width=2, flag="0"), sep = "", collapse = "|"), G_cat := 1]

hospwide[var1 %ilike% "R06", R_cat := 1]
hospwide[var2 %ilike% "R06", R_cat := 1]
hospwide[var3 %ilike% "R06", R_cat := 1]
hospwide[var4 %ilike% "R06", R_cat := 1]
hospwide[var5 %ilike% "R06", R_cat := 1]
hospwide[var6 %ilike% "R06", R_cat := 1]
hospwide[var7 %ilike% "R06", R_cat := 1]
hospwide[var8 %ilike% "R06", R_cat := 1]
hospwide[var9 %ilike% "R06", R_cat := 1]
hospwide[var10 %ilike% "R06", R_cat := 1]
hospwide[var11 %ilike% "R06", R_cat := 1]
hospwide[var12 %ilike% "R06", R_cat := 1]
hospwide[var13 %ilike% "R06", R_cat := 1]
hospwide[var14 %ilike% "R06", R_cat := 1]
hospwide[var15 %ilike% "R06", R_cat := 1]
hospwide[var16 %ilike% "R06", R_cat := 1]
hospwide[var17 %ilike% "R06", R_cat := 1]
hospwide[var18 %ilike% "R06", R_cat := 1]
hospwide[var19 %ilike% "R06", R_cat := 1]
hospwide[var20 %ilike% "R06", R_cat := 1]
hospwide[var21 %ilike% "R06", R_cat := 1]
hospwide[var22 %ilike% "R06", R_cat := 1]
hospwide[var23 %ilike% "R06", R_cat := 1]

hospwide[G_cat == 1 & R_cat == 1, NLHoog_neur := 1]
length(unique(hospwide[NLHoog_neur == 1, RINPERSOON])) 
hospmerge <- hospwide[, c("RINPERSOON", "NLHoog_neur")]

rm(hospwide)

setnafill(hospmerge, fill = 0)
hospmerge <- hospmerge[, lapply(.SD, max, na.rm = T), by = RINPERSOON]
# table(hospmerge$NLHoog_neur) 
setkey(hospmerge, RINPERSOON)

# Merge hospcol en hospwide
hosp3 <- hospmerge[hospcol]
setnafill(hosp3, fill = 0)
setkey(hosp3, RINPERSOON)

rm(hospcol, hospmerge)

# Add medication data.
############################
### Load medication data ###
############################
med2019 <- read_sav("G:/GezondheidWelzijn/MEDICIJNTAB/2019/MEDICIJN2019TABV1.sav")
med2019 <- zap_formats(zap_labels(zap_label(zap_widths(med2019))))
setDT(med2019)
med2019[, RINPERSOON := as.integer(RINPERSOON)]
setkey(med2019, RINPERSOON)

# Load RIN-numbers that we need to keep.
RIN <- fread("H:/Data/CoviCron/Bevolking/RINBevolking2020.csv")
setkey(RIN, RINPERSOON)

# Select only individuals within cohort.
popmed <- med2019[RIN]

# Check how many unique individuals.
length(unique(popmed$RINPERSOON)) 

rm(RIN, med2019)

########################
### Load data cohort ###
########################
age <- fread("H:/Data/CoviCron/Bevolking/Bevolking2020.csv")
setkey(age, RINPERSOON)

#############################################################################
### Define categories in Risk classification: NL Medische Hoogrisicogroep ###
#############################################################################
# Add age to hosp.
age[, c("RINPERSOONS", "datedeath", "datebirth", "agecat") := NULL]
popmedage <- age[popmed]

# Check how many unique individuals.
length(unique(popmedage$RINPERSOON)) # 17402279

rm(popmed, age)

# Netherlands high risk group based on Medicationdata. If := 0 means no ATC-codes defined.
popmedage[, NLHoogMED_hemmal := 0]

popmedage[, NLHoogMED_sikkel := 0]

popmedage[ATC4 %ilike% "B05D|B05Z", NLHoogMED_dial := 1]
# table(popmedage$NLHoogMED_dial) 
# length(unique(popmedage[NLHoogMED_dial == 1, RINPERSOON])) 

popmedage[, NLHoogMED_trans := 0]

popmedage[, NLHoogMED_aanaf := 0]

popmedage[, NLHoogMED_neur := 0]

popmedage[, NLHoogMED_kank := 0]

popmedage[, NLHoogMED_down := 0]

popmedage[ATC4 %ilike% "L04A", NLHoogMED_immunosup := 1]
# table(popmedage$NLHoogMED_immunosup) 
# length(unique(popmedage[NLHoogMED_immunosup == 1, RINPERSOON]))

popmedage[ATC4 %ilike% "V06A", NLHoogMED_obes := 1]
# table(popmedage$NLHoogMED_obes)
# length(unique(popmedage[NLHoogMED_obes == 1, RINPERSOON])) 

# Netherlands medium risk group
popmedage[ATC4 %ilike% "C01A|C01B|C01C|C01D|C01D|C02|C03|C07|C08|C09|C10", NLMedMED_cvd := 1]
# table(popmedage$NLMedMED_cvd) 
# length(unique(popmedage[NLMedMED_cvd == 1, RINPERSOON])) 

popmedage[ATC4 %ilike% "A10A|A10B|A10X", NLMedMED_DM := 1]
# table(popmedage$NLMedMED_DM)  
# length(unique(popmedage[NLMedMED_DM == 1, RINPERSOON])) 

popmedage[, NLMedMED_beentrans := 0]

popmedage[, NLMedMED_HIV := 0]

popmedage[ATC4 %ilike% "L04A", NLMedMED_verwee := 1]
# table(popmedage$NLMedMED_verwee) 
# length(unique(popmedage[NLMedMED_verwee == 1, RINPERSOON])) 

popmedage[ATC4 %ilike% "B05D|B05Z", NLMedMED_nier := 1]
# table(popmedage$NLMedMED_nier) 
# length(unique(popmedage[NLMedMED_nier == 1, RINPERSOON])) 

popmedage[ATC4 %ilike% "R03A|R03B|R03C|R03D|J04A", NLMedMED_lucht := 1]
# table(popmedage$NLMedMED_lucht) 
# length(unique(popmedage[NLMedMED_lucht == 1, RINPERSOON]))

popmedage[, NLMedMED_neur := 0]

popmedage[ATC4 %ilike% "V06A", NLMedMED_obes := 1]
# table(popmedage$NLMedMED_obes) 
# length(unique(popmedage[NLMedMED_obes == 1, RINPERSOON])) 

popmedage[age < 60 & ATC4 %ilike% "N06D", NLMedMED_demen := 1]
# table(popmedage$NLMedMED_demen)
# length(unique(popmedage[NLMedMED_demen == 1, RINPERSOON]))  

popmedage[, NLMedMED_coch := 0]

# Collaps hosp per RINPERSOON.
popmedcol <- popmedage[, !c("ATC4", "age", "RINPERSOONS")]

rm(popmedage)

# Change NA's to zero's.
setnafill(popmedcol, fill = 0)
popmedcolmerge <- popmedcol[, lapply(.SD, max, na.rm = T), by = RINPERSOON]
setkey(popmedcolmerge, RINPERSOON)
length(unique(popmedcolmerge$RINPERSOON)) 

rm(popmedcol)

# Merge hospital information to the base dataset.
pop2020 <- fread("H:/Data/CoviCron/Bevolking/Bevolking2020.csv")
setkey(pop2020, RINPERSOON)

step1 <- hosp3[pop2020]

rm(hosp3, pop2020)

pop_final <- popmedcolmerge[step1]

rm(popmedcolmerge, step1)

# Create variables for risk categories.
# First, without using medication information.
pop_final[, NLHoog := 0][NLHoog_hemmal == 1 | NLHoog_sikkel == 1 | NLHoog_dial == 1 | NLHoog_trans == 1 |
                      NLHoog_aanaf == 1 | NLHoog_kank == 1 | NLHoog_down == 1 | NLHoog_immunosup == 1 | NLHoog_obes == 1 | NLHoog_neur == 1 , NLHoog := 1]
pop_final[NLMed_cvd == 1 | NLMed_DM == 1 | NLMed_beentrans == 1 | NLMed_HIV == 1 |
                     NLMed_verwee == 1 | NLMed_nier == 1 | NLMed_lucht == 1 | NLMed_neur == 1 | NLMed_obes == 1 | NLMed_demen == 1 | NLMed_coch == 1, NLMed := 1][
                       is.na(NLMed) == T, NLMed := 0]
pop_final[, NLLaag := 1][NLHoog == 1 | NLMed == 1, NLLaag := 0]
pop_final[NLLaag == 1, NLriskgroup := "laag"][NLMed == 1, NLriskgroup := "medium"][NLHoog == 1, NLriskgroup := "hoog"]

# Second, including medication information.
pop_final[, NLHoog_med := 0][NLHoog_hemmal == 1 | NLHoogMED_hemmal == 1 | NLHoog_sikkel == 1 | NLHoogMED_sikkel == 1 | NLHoog_dial == 1 | NLHoogMED_dial == 1 | NLHoog_trans == 1 | NLHoogMED_trans == 1 |
                           NLHoog_aanaf == 1 | NLHoogMED_aanaf == 1 | NLHoog_kank == 1 | NLHoogMED_kank == 1 | NLHoog_down == 1 | NLHoogMED_down == 1 | NLHoog_immunosup == 1 | NLHoogMED_immunosup == 1 | NLHoog_obes == 1 | NLHoogMED_obes == 1 | NLHoog_neur == 1 | NLHoogMED_neur == 1, NLHoog_med := 1]
pop_final[NLMed_cvd == 1 | NLMedMED_cvd == 1 | NLMed_DM == 1 | NLMedMED_DM == 1 | NLMed_beentrans == 1 | NLMedMED_beentrans == 1 | NLMed_HIV == 1 | NLMedMED_HIV == 1 |
            NLMed_verwee == 1 | NLMedMED_verwee == 1 | NLMed_nier == 1 | NLMedMED_nier == 1 | NLMed_lucht == 1 | NLMedMED_lucht == 1 | NLMed_neur == 1 | NLMedMED_neur == 1 | NLMed_obes == 1 | NLMedMED_obes == 1 | NLMed_demen == 1 | NLMedMED_demen == 1 | NLMed_coch == 1, NLMed_med := 1][
              is.na(NLMed_med) == T, NLMed_med := 0]
pop_final[, NLLaag_med := 1][NLHoog_med == 1 | NLMed_med == 1, NLLaag_med := 0]
pop_final[NLLaag_med == 1, NLriskgroup_med := "laag"][NLMed_med == 1, NLriskgroup_med := "medium"][NLHoog_med == 1, NLriskgroup_med := "hoog"]

# Third, using the ECDC classification.
pop_final[, ECDCHoog := 0][ECDC_asthma == 1 | ECDC_immu == 1 | ECDC_kank == 1 | ECDC_DM == 1 |
                         ECDC_cvd == 1 | ECDC_hyp == 1 | ECDC_lung == 1 | ECDC_obe == 1, ECDCHoog := 1]
pop_final[ECDC_anaemia == 1 | ECDC_asplenia == 1 | ECDC_liver == 1 | ECDC_dem == 1 |
        ECDC_neur == 1 | ECDC_ren == 1 | ECDC_reu == 1 | ECDC_cva == 1 | ECDC_tbc == 1, ECDCMed := 1][
          is.na(ECDCMed) == T, ECDCMed := 0]
pop_final[, ECDCLaag := 1][ECDCHoog == 1 | ECDCMed == 1, ECDCLaag := 0]
pop_final[ECDCLaag == 1, ECDCriskgroup := "laag"][ECDCMed == 1, ECDCriskgroup := "medium"][ECDCHoog == 1, ECDCriskgroup := "hoog"]

table(pop_final$ECDCriskgroup)
table(pop_final$NLriskgroup)
table(pop_final$NLriskgroup_med)

# Save datafile as .csv.
fwrite(pop_final, file = "H:/Data/RiskGroups2020MED.csv")
