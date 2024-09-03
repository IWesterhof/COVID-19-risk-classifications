## Goal: Output stratification 2020 Riskgroups based on chronic conditions and medication use. CoviChron study. 
## Version/date: V1.0/01-08-2024
## Authos: A.R. de Boer/ I. Westerhof
## R-version: 4.2.3

#####################
### Load packages ###
#####################
library(data.table)
library(ggplot2)
library(openxlsx)
library(tidyverse)
library(grid)

########################
### Load data cohort ###
########################

risk2020 <- fread("H:/RiskGroups2020MED.csv")
setkey(risk2020, RINPERSOON)

###############################################################################################################
### Create output for Table/Figure with number of individuals stratified for risk category and age category ###
###############################################################################################################
## Number of unique individuals in dataset.
Totalpop <- length(unique(risk2020$RINPERSOON))

# Second, create age categories.
breaks <- c(0,10,20,30,40,50,60,70,80,Inf)
risk2020[, agecat2 := cut(age, breaks=breaks, right = F)]

## Information about mean/sd and median/IQR age in population.
Age_info_mean <- mean(risk2020$age)
Age_info_sd <- sd(risk2020$age)
Age_info_median <- median(risk2020$age)
Age_info_iqr1 <- quantile(risk2020$age, 1/4)
Age_info_iqr3 <- quantile(risk2020$age, 3/4)

## 2020

# Make agecat a factor and order the levels.
risk2020[, fac_agecat := factor(agecat2, levels = c("[0,10)", "[10,20)", "[20,30)", "[30,40)", "[40,50)", "[50,60)", "[60,70)", "[70,80)", "[80,Inf)"))]
risk2020[, fac_NLriskgroup := factor(NLriskgroup, levels = c("hoog", "medium", "laag"))]
risk2020[, fac_NLriskgroup_med := factor(NLriskgroup_med, levels = c("hoog", "medium", "laag"))]
risk2020[, fac_ECDCriskgroup := factor(ECDCriskgroup, levels = c("hoog", "medium", "laag"))]

# Create tables underlying of figures for Stratification population for age and risk group.
NLriskgroups2020 <- risk2020 %>% 
  group_by(fac_NLriskgroup, fac_agecat) %>% 
  summarise(n=n(), .groups = "drop") %>% 
  complete(fac_NLriskgroup, fac_agecat, fill = list(n = 0))

NLriskgroups2020_1 <- NLriskgroups2020 %>% 
  pivot_wider(id_cols = fac_agecat, names_from = fac_NLriskgroup, values_from = n)

NLriskgroups2020_med <- risk2020 %>% 
  group_by(fac_NLriskgroup_med, fac_agecat) %>% 
  summarise(n=n(), .groups = "drop") %>% 
  complete(fac_NLriskgroup_med, fac_agecat, fill = list(n = 0))

NLriskgroups2020_med_1 <- NLriskgroups2020_med %>% 
  pivot_wider(id_cols = fac_agecat, names_from = fac_NLriskgroup_med, values_from = n)

ECDCriskgroups2020 <- risk2020 %>% 
  group_by(fac_ECDCriskgroup, fac_agecat) %>% 
  summarise(n=n(), .groups = "drop") %>% 
  complete(fac_ECDCriskgroup, fac_agecat, fill = list(n = 0))

ECDCriskgroups2020_1 <- ECDCriskgroups2020 %>% 
  pivot_wider(id_cols = fac_agecat, names_from = fac_ECDCriskgroup, values_from = n)

# Create output underlying heatmaps.
Heatmap2020 <- risk2020 %>% 
  group_by(fac_NLriskgroup, fac_ECDCriskgroup) %>% 
  summarise(n=n(), .groups = "drop") %>% 
  complete(fac_NLriskgroup, fac_ECDCriskgroup, fill = list(n = 0))

#################################
### Save output in excel file ###
#################################
# Create Workbook (= excel file)
wb <- createWorkbook()
# Add worksheet (=tabblad)
addWorksheet(wb = wb, sheetName = "PopulationStrat")
# Write commentary about the output.
text <- "Stratification population for COVID-19 risk based on chronic conditions and medication use"
writeData(wb = wb, sheet = "PopulationStrat", x = text, startCol = 1, startRow = 1)
writeData(wb = wb, sheet = "PopulationStrat", x = "Total n population = ", startCol = 1, startRow = 2)
writeData(wb = wb, sheet = "PopulationStrat", x = Totalpop, startCol = 2, startRow = 2)
writeData(wb = wb, sheet = "PopulationStrat", x = "Mean age", startCol = 3, startRow = 2)
writeData(wb = wb, sheet = "PopulationStrat", x = Age_info_mean, startCol = 4, startRow = 2)
writeData(wb = wb, sheet = "PopulationStrat", x = "Standard Deviation age", startCol = 5, startRow = 2)
writeData(wb = wb, sheet = "PopulationStrat", x = Age_info_sd, startCol = 6, startRow = 2)
writeData(wb = wb, sheet = "PopulationStrat", x = "Median age", startCol = 7, startRow = 2)
writeData(wb = wb, sheet = "PopulationStrat", x = Age_info_median, startCol = 8, startRow = 2)
writeData(wb = wb, sheet = "PopulationStrat", x = "IQR1 age", startCol = 9, startRow = 2)
writeData(wb = wb, sheet = "PopulationStrat", x = Age_info_iqr1, startCol = 10, startRow = 2)
writeData(wb = wb, sheet = "PopulationStrat", x = "IQR3 age", startCol = 11, startRow = 2)
writeData(wb = wb, sheet = "PopulationStrat", x = Age_info_iqr3, startCol = 12, startRow = 2)
writeData(wb = wb, sheet = "PopulationStrat", x = "2020", startCol = 1, startRow = 3)
writeData(wb = wb, sheet = "PopulationStrat", x = "Dutch risk classification", startCol = 1, startRow = 4)
writeData(wb = wb, sheet = "PopulationStrat", x = "Dutch risk classification including medication data", startCol = 6, startRow = 4)
writeData(wb = wb, sheet = "PopulationStrat", x = "ECDC risk classification", startCol = 11, startRow = 4)
writeData(wb = wb, sheet = "PopulationStrat", x = "Heatmap Dutch/ECDC classification", startCol = 16, startRow = 4)
# Add stratification for 2020 to workbook.
writeData(wb = wb, sheet = "PopulationStrat", x = NLriskgroups2020_1, startCol = 1, startRow = 5, colNames = T)
writeData(wb = wb, sheet = "PopulationStrat", x = NLriskgroups2020_med_1, startCol = 6, startRow = 5, colNames = T)
writeData(wb = wb, sheet = "PopulationStrat", x = ECDCriskgroups2020_1, startCol = 11, startRow = 5, colNames = T)
writeData(wb = wb, sheet = "PopulationStrat", x = Heatmap2020, startCol = 16, startRow = 5, colNames = T)#http://127.0.0.1:21017/graphics/75df5938-9945-4099-bee7-3cfcd5e36007.png
# Save the workbook.
#saveWorkbook(wb, "H:/Results/PopulationStrat2020.xlsx", overwrite = T)


# Create output underlying heatmaps.
Heatmap2020_60 <- risk2020 %>% filter(age<60) %>%
  group_by(fac_NLriskgroup, fac_ECDCriskgroup) %>% 
  summarise(n=n(), .groups = "drop") %>% 
  complete(fac_NLriskgroup, fac_ECDCriskgroup, fill = list(n = 0))

# Create output underlying heatmaps.
Heatmap2020_65 <- risk2020 %>% filter(age<65) %>%
  group_by(fac_NLriskgroup, fac_ECDCriskgroup) %>% 
  summarise(n=n(), .groups = "drop") %>% 
  complete(fac_NLriskgroup, fac_ECDCriskgroup, fill = list(n = 0))


#################################
### Save output in excel file ###
#################################
# Create Workbook (= excel file)
wb <- createWorkbook()
# Add worksheet (=tabblad)
addWorksheet(wb = wb, sheetName = "Discordance")
# Write commentary about the output.
text <- "Stratification population for COVID-19 risk based on chronic conditions and medication use"
writeData(wb = wb, sheet = "Discordance", x = text, startCol = 1, startRow = 1)
writeData(wb = wb, sheet = "Discordance", x = "Heatmap Dutch/ECDC classification aged <60", startCol = 1, startRow = 3)
writeData(wb = wb, sheet = "Discordance", x = "Heatmap Dutch/ECDC classification aged <65", startCol = 6, startRow = 3)
# Add stratification for 2020 to workbook.
writeData(wb = wb, sheet = "Discordance", x = Heatmap2020_60, startCol = 1, startRow = 4, colNames = T)
writeData(wb = wb, sheet = "Discordance", x = Heatmap2020_65, startCol = 6, startRow = 4, colNames = T)#http://127.0.0.1:21017/graphics/75df5938-9945-4099-bee7-3cfcd5e36007.png
# Save the workbook.
#saveWorkbook(wb, "H:/Results/Discordance aged below 60 and 65.xlsx", overwrite = T)


### Overview comorbidities
overviewdat = risk2020 %>% select(contains(c("ECDC_", "NLHoog_", "NLMed_"))) %>% 
  filter(!is.na(ECDC_asthma)) %>% 
  pivot_longer(cols = ECDC_asthma:NLMed_coch, 
               names_to = "Comorbidity",
               values_to = "value") %>% 
  filter(value==1) %>% 
  group_by(Comorbidity) %>% 
  summarise(n = n(), prop = paste0(round(n/Totalpop*100, digits = 2), "%")) 

info_ECDC = risk2020 %>% select(contains(c("ECDC_"))) %>%   filter(!is.na(ECDC_asthma)) 
info_ECDC$sum = rowSums(info_ECDC)
ECDC_info_mean <- round(mean(info_ECDC$sum), digits = 2)
ECDC_info_sd <- round(sd(info_ECDC$sum), digits = 1)
ECDC_info_median <- median(info_ECDC$sum)
ECDC_info_iqr1 <- quantile(info_ECDC$sum, 1/4)
ECDC_info_iqr3 <- quantile(info_ECDC$sum, 3/4)

info_NL = risk2020 %>% select(contains(c("NLHoog_", "NLMed_"))) %>%   filter(!is.na(NLHoog_aanaf)) 
info_NL$sum = rowSums(info_NL)
NL_info_mean <- round(mean(info_NL$sum), digits = 2)
NL_info_sd <- round(sd(info_NL$sum), digits = 1)
NL_info_median <- median(info_NL$sum)
NL_info_iqr1 <- quantile(info_NL$sum, 1/4)
NL_info_iqr3 <- quantile(info_NL$sum, 3/4)


#################################
### Save output in excel file ###
#################################
# Create Workbook (= excel file)
wb <- createWorkbook()
# Add worksheet (=tabblad)
addWorksheet(wb = wb, sheetName = "PopulationStrat2020")
# Write commentary about the output.
text <- "Stratification population for COVID-19 risk by comorbidity based on chronic conditions"
writeData(wb = wb, sheet = "PopulationStrat2020", x = text, startCol = 1, startRow = 1)
writeData(wb = wb, sheet = "PopulationStrat2020", x = "Total n population = ", startCol = 1, startRow = 2)
writeData(wb = wb, sheet = "PopulationStrat2020", x = Totalpop, startCol = 2, startRow = 2)
writeData(wb = wb, sheet = "PopulationStrat2020", x = "Mean ECDC", startCol = 3, startRow = 2)
writeData(wb = wb, sheet = "PopulationStrat2020", x = ECDC_info_mean, startCol = 4, startRow = 2)
writeData(wb = wb, sheet = "PopulationStrat2020", x = "Standard Deviation ECDC", startCol = 5, startRow = 2)
writeData(wb = wb, sheet = "PopulationStrat2020", x = ECDC_info_sd, startCol = 6, startRow = 2)
writeData(wb = wb, sheet = "PopulationStrat2020", x = "Median ECDC", startCol = 7, startRow = 2)
writeData(wb = wb, sheet = "PopulationStrat2020", x = ECDC_info_median, startCol = 8, startRow = 2)
writeData(wb = wb, sheet = "PopulationStrat2020", x = "IQR1 ECDC", startCol = 9, startRow = 2)
writeData(wb = wb, sheet = "PopulationStrat2020", x = ECDC_info_iqr1, startCol = 10, startRow = 2)
writeData(wb = wb, sheet = "PopulationStrat2020", x = "IQR3 ECDC", startCol = 11, startRow = 2)
writeData(wb = wb, sheet = "PopulationStrat2020", x = ECDC_info_iqr3, startCol = 12, startRow = 2)

writeData(wb = wb, sheet = "PopulationStrat2020", x = "Mean NL", startCol = 3, startRow = 3)
writeData(wb = wb, sheet = "PopulationStrat2020", x = NL_info_mean, startCol = 4, startRow = 3)
writeData(wb = wb, sheet = "PopulationStrat2020", x = "Standard Deviation NL", startCol = 5, startRow = 3)
writeData(wb = wb, sheet = "PopulationStrat2020", x = NL_info_sd, startCol = 6, startRow = 3)
writeData(wb = wb, sheet = "PopulationStrat2020", x = "Median NL", startCol = 7, startRow = 3)
writeData(wb = wb, sheet = "PopulationStrat2020", x = NL_info_median, startCol = 8, startRow = 3)
writeData(wb = wb, sheet = "PopulationStrat2020", x = "IQR1 NL", startCol = 9, startRow = 3)
writeData(wb = wb, sheet = "PopulationStrat2020", x = NL_info_iqr1, startCol = 10, startRow = 3)
writeData(wb = wb, sheet = "PopulationStrat2020", x = "IQR3 NL", startCol = 11, startRow = 3)
writeData(wb = wb, sheet = "PopulationStrat2020", x = NL_info_iqr3, startCol = 12, startRow = 3)

writeData(wb = wb, sheet = "PopulationStrat2020", x = overviewdat, startCol = 1, startRow = 5, colNames = T)# Save the workbook.
saveWorkbook(wb, "H:/Results/PopulationStrat_bycomorbidity.xlsx", overwrite = T)


### overview diseases
Agesize = risk2020 %>% group_by(agecat2) %>%   summarise(Totalpop=n())
overviewdat = risk2020 %>% select(agecat2, contains(c("ECDC_", "NLHoog_", "NLMed_"))) %>% 
  group_by(agecat2) %>% 
  pivot_longer(cols = ECDC_asthma:NLMed_coch, 
               names_to = "Comorbidity",
               values_to = "value") %>% 
  filter(value==1) %>% 
  group_by(agecat2, Comorbidity) %>% 
  summarise(n = n()) %>% distinct()
overviewdat = merge(overviewdat, Agesize, by="agecat2", all=T) %>% 
  mutate(prop = round(n/Totalpop*100, digits=2))

#################################
### Save output in excel file ###
#################################
# Create Workbook (= excel file)
wb <- createWorkbook()
# Add worksheet (=tabblad)
addWorksheet(wb = wb, sheetName = "PopulationStrat2020")
# Write commentary about the output.
text <- "Stratification population for COVID-19 risk by age and comorbidity based on chronic conditions"
writeData(wb = wb, sheet = "PopulationStrat2020", x = overviewdat, startCol = 1, startRow = 1, colNames = T)# Save the workbook.
saveWorkbook(wb, "H:/Results/PopulationStrat_byageandcomorbidity.xlsx", overwrite = T)


### Disease names
disease_names <- function(dataset, mutation) {dataset} %>% 
  mutate(name = ifelse(name=="NLHoog_neur", "Neurological disorder (HR)", 
                       ifelse(name=="NLHoog_hemmal" | name=="NLHoog_kank", "Cancer (HR)", 
                              ifelse(name=="NLHoog_sikkel", "Sickle cell disease (HR)", 
                                     ifelse(name=="NLHoog_dial", "Kidney disease (HR)", 
                                            ifelse(name=="NLHoog_trans", "Organ transplant (HR)", 
                                                   ifelse(name=="NLHoog_aanaf", "Immunodeficiency (HR)", 
                                                          ifelse(name=="NLHoog_down", "Down syndrome (HR)", 
                                                                 ifelse(name=="NLMed_obes" | name=="NLHoog_obes", "Morbide obesity (HR)", 
                                                                        ifelse(name=="NLMed_cvd", "Cardiovascular disease (MR)", 
                                                                               ifelse(name=="NLMed_DM", "Diabetes mellitus (MR)", 
                                                                                      ifelse(name=="NLMed_HIV", "HIV (MR)", 
                                                                                             ifelse(name=="NLMed_verwee", "Reduced infection resistance (MR)", 
                                                                                                    ifelse(name=="NLMed_nier", "Kindey disease (MR)", 
                                                                                                           ifelse(name=="NLMed_lucht", "Lung Disease (MR)", 
                                                                                                                  ifelse(name=="NLMed_neur", "Neurological/-muscular disorder (MR)", 
                                                                                                                         ifelse(name=="NLMed_demen", "Dementia (MR)", 
                                                                                                                                ifelse(name=="NLMed_coch", "Cochlear implants (MR)", 
                                                                                                                                       ifelse(name=="ECDC_asthma" | name=="ECDC_lung", "Lung disease (HR)", 
                                                                                                                                              ifelse(name=="ECDC_immu", "Immunodeficiency (HR)", 
                                                                                                                                                     ifelse(name=="ECDC_anaemia", "Anaemia (MR)", 
                                                                                                                                                            ifelse(name=="ECDC_kank", "Cancer (HR)", 
                                                                                                                                                                   ifelse(name=="ECDC_DM", "Diabetes mellitus  (HR)", 
                                                                                                                                                                          ifelse(name=="ECDC_cvd", "  Cardiovascular disease (HR)  ", 
                                                                                                                                                                                 ifelse(name=="ECDC_hyp", "Hypertension (HR)", 
                                                                                                                                                                                        ifelse(name=="ECDC_obe", "Morbide obesity (HR)", 
                                                                                                                                                                                               ifelse(name=="ECDC_asplenia", "Asplenia (MR)", 
                                                                                                                                                                                                      ifelse(name=="ECDC_liver", "Liver disease (MR)", 
                                                                                                                                                                                                             ifelse(name=="ECDC_dem", "Dementia (MR)", 
                                                                                                                                                                                                                    ifelse(name=="ECDC_neur", "Neurological disorder (MR)", 
                                                                                                                                                                                                                           ifelse(name=="ECDC_ren", "Renal disease (MR)", 
                                                                                                                                                                                                                                  ifelse(name=="ECDC_reu", "Rheumatologic disease (MR)", 
                                                                                                                                                                                                                                         ifelse(name=="ECDC_cva", "Neurological disorder (MR)", 
                                                                                                                                                                                                                                                ifelse(name=="ECDC_tbc", "Tuberculosis (MR)", name))))))))))))))))))))))))))))))))))



#### Figure 1 Upsetplots General population ####
library(UpSetR); library(grid)

### Discordance between classifications - NL
upset_data = risk2020  %>% filter(!NLriskgroup=="laag") %>%
  select(RINPERSOON, ECDCriskgroup, NLriskgroup, contains(c("NLHoog_", "NLMed_")))  %>% 
  pivot_longer(!c(RINPERSOON, ECDCriskgroup, NLriskgroup), names_to="name", values_to = "value") %>% 
  filter(value==1)%>%   select(-ECDCriskgroup, -NLriskgroup) %>%
  disease_names() %>% distinct() %>% 
  filter(!(grepl("NLMed_beentrans|NLHoog_med|NLMed_med", name))) %>% 
  group_by(RINPERSOON) %>%   pivot_wider(names_from = name, values_from = value, values_fill = list(value=0)) %>% 
  ungroup() %>%  select(-RINPERSOON) 
upset_data = as.data.frame(upset_data)
UpSetR::upset(upset_data, sets = colnames(upset_data),
              keep.order=F, order.by = "freq", set_size.show = T, #show.numbers = "no",
              mainbar.y.label = "Combinations of\nchronic conditions", sets.x.label = "",
              sets.bar.color = c("darkblue","darkblue","darkblue","darkblue","darkblue",
                                 "darkblue","darkorange2","darkorange2","darkorange2","darkorange2",
                                 "darkorange2","darkorange2","darkblue","darkorange2","darkblue","darkorange2","darkblue"),
              nintersects = 17, mb.ratio = c(0.3, 0.7), text.scale = c(1,1,1,1,1,0.85))  
plot + grid.text(label = "The Netherlands, National classification", x = 0.06, y = .965, just = "left", gp = gpar(fontsize = 8))
grid.edit('arrange', name = 'arrange2'); UpsetNL_NL = grid.grab()

### Discordance between classifications - ECDC
upset_data = risk2020  %>% filter(!ECDCriskgroup=="laag") %>%
  select(RINPERSOON, ECDCriskgroup, NLriskgroup, contains(c("ECDC_")))  %>% 
  mutate(ECDC_neur = ifelse(ECDC_cva==1, 1, ECDC_neur),
         ECDC_lung  = ifelse(ECDC_asthma==1, 1, ECDC_lung)) %>% select(-ECDC_cva, -ECDC_asthma) %>% 
  pivot_longer(!c(RINPERSOON, ECDCriskgroup, NLriskgroup), names_to="name", values_to = "value") %>% 
  filter(value==1)%>%   select(-ECDCriskgroup, -NLriskgroup) %>%
  disease_names() %>% distinct() %>% 
  group_by(RINPERSOON) %>%   pivot_wider(names_from = name, values_from = value, values_fill = list(value=0)) %>% 
  ungroup() %>%  select(-RINPERSOON) 
upset_data = as.data.frame(upset_data)
UpSetR::upset(upset_data, sets = colnames(upset_data),
              keep.order=F, order.by = "freq", set_size.show = T,
              mainbar.y.label = "Combinations of\nchronic conditions", sets.x.label = "", #show.numbers = "no",
              sets.bar.color = c("darkorange2","darkorange2","darkorange2","darkorange2","darkorange2",
                                 "darkblue","darkblue","darkblue","darkorange2","darkblue",
                                 "darkblue","darkorange2","darkblue","darkblue","darkblue"),
              nintersects = 13, mb.ratio = c(0.3, 0.7)) 
plot + grid.text(label = "The Netherlands, European classification", x = 0.06, y = .965, just = "left", gp = gpar(fontsize = 8))
grid.edit('arrange', name = 'arrange2'); UpsetNL_EU = grid.grab()

UPSETNL = cowplot::plot_grid(UpsetNL_NL, UpsetNL_EU, ncol=1, align = 'v', labels=c("A", "C")); UPSETNL
ggsave("H:/Results/Fig 1. General population.pdf", UPSETNL, width = 18, height = 20, units = "cm", dpi = 320, limitsize = FALSE)

### Save related numbers in excel file 
Populationsize = risk2020  %>% summarise(n())
ECDC_overview = risk2020  %>% filter(!ECDCriskgroup=="laag") %>%
  select(RINPERSOON, ECDCriskgroup, NLriskgroup, contains(c("ECDC_")))  %>% 
  mutate(ECDC_neur = ifelse(ECDC_cva==1, 1, ECDC_neur),
         ECDC_lung  = ifelse(ECDC_asthma==1, 1, ECDC_lung)) %>% select(-ECDC_cva, -ECDC_asthma) %>% 
  pivot_longer(!c(RINPERSOON, ECDCriskgroup, NLriskgroup), names_to="name", values_to = "value") %>% 
  filter(value==1)%>%   select(-ECDCriskgroup, -NLriskgroup) %>%  disease_names()  %>% 
  group_by(name) %>% summarise(n=n(), Prop =  as.numeric(n()/Populationsize*100)) %>% as.data.frame()
NL_overview = risk2020  %>% filter(!NLriskgroup=="laag") %>%
  select(RINPERSOON, ECDCriskgroup, NLriskgroup, contains(c("NLHoog_", "NLMed_")))  %>% 
  pivot_longer(!c(RINPERSOON, ECDCriskgroup, NLriskgroup), names_to="name", values_to = "value") %>% 
  filter(value==1)%>%   select(-ECDCriskgroup, -NLriskgroup) %>%
  disease_names() %>% distinct() %>% 
  filter(!(grepl("NLMed_beentrans|NLHoog_med|NLMed_med", name))) %>% 
  group_by(name) %>% summarise(n=n(), Prop =  as.numeric(n()/Populationsize*100)) %>% as.data.frame()

# Create Workbook (= excel file)
wb <- createWorkbook()
# Add worksheet (=tabblad)
addWorksheet(wb = wb, sheetName = "General population")
# Write commentary about the output.
text <- "Overview of chronic conditions in population with discordance between Dutch and European classification"
writeData(wb = wb, sheet = "General population", x = text, startCol = 1, startRow = 1)
writeData(wb = wb, sheet = "General population", x = "Total n population = ", startCol = 1, startRow = 2)
writeData(wb = wb, sheet = "General population", x = Populationsize$`n()`, startCol = 2, startRow = 2)
writeData(wb = wb, sheet = "General population", x = "Dutch risk classification", startCol = 1, startRow = 4)
writeData(wb = wb, sheet = "General population", x = NL_overview, startCol = 1, startRow = 5, colNames = T)
writeData(wb = wb, sheet = "General population", x = "ECDC risk classification", startCol = 1, startRow = 22)
writeData(wb = wb, sheet = "General population", x = ECDC_overview, startCol = 1, startRow = 23, colNames = T)# Save the workbook.
#saveWorkbook(wb, "H:/Results/Fig 1. General population Chronic conditions.xlsx", overwrite = T)


#### Figure 4 Upsetplots discordant ####
library(UpSetR); library(grid)

### Discordance between classifications - NL
upset_data = risk2020  %>% filter(ECDCriskgroup != NLriskgroup) %>%
  select(RINPERSOON, ECDCriskgroup, NLriskgroup, contains(c("NLHoog_", "NLMed_")))  %>% 
  pivot_longer(!c(RINPERSOON, ECDCriskgroup, NLriskgroup), names_to="name", values_to = "value") %>% 
  filter(value==1)%>%   select(-ECDCriskgroup, -NLriskgroup) %>%
  disease_names() %>% distinct() %>% 
  filter(!(grepl("NLMed_beentrans|NLHoog_med|NLMed_med", name))) %>% 
  group_by(RINPERSOON) %>%   pivot_wider(names_from = name, values_from = value, values_fill = list(value=0)) %>% 
  ungroup() %>%  select(-RINPERSOON) 
upset_data = as.data.frame(upset_data)
UpSetR::upset(upset_data, sets = colnames(upset_data),
              keep.order=F, order.by = "freq", set_size.show = T, #show.numbers = "no",
              mainbar.y.label = "Combinations of\nchronic conditions", sets.x.label = "",
              sets.bar.color = c("darkblue","darkblue","darkblue","darkblue","darkblue",
                                 "darkblue","darkorange2","darkorange2","darkblue","darkorange2",
                                 "darkorange2","darkorange2","darkblue","darkblue"),
              nintersects = 17, mb.ratio = c(0.3, 0.7), text.scale = c(1,1,1,1,1,0.85))   
plot + grid.text(label = "The Netherlands, National classification", x = 0.06, y = .965, just = "left", gp = gpar(fontsize = 8))
grid.edit('arrange', name = 'arrange2'); UpsetNL_NL = grid.grab()

### Discordance between classifications - ECDC
upset_data = risk2020  %>% filter(ECDCriskgroup != NLriskgroup) %>%
  select(RINPERSOON, ECDCriskgroup, NLriskgroup, contains(c("ECDC_")))  %>% 
  mutate(ECDC_neur = ifelse(ECDC_cva==1, 1, ECDC_neur),
         ECDC_lung  = ifelse(ECDC_asthma==1, 1, ECDC_lung)) %>% select(-ECDC_cva, -ECDC_asthma) %>% 
  pivot_longer(!c(RINPERSOON, ECDCriskgroup, NLriskgroup), names_to="name", values_to = "value") %>% 
  filter(value==1)%>%   select(-ECDCriskgroup, -NLriskgroup) %>%
  disease_names() %>% distinct() %>% 
  group_by(RINPERSOON) %>%   pivot_wider(names_from = name, values_from = value, values_fill = list(value=0)) %>% 
  ungroup() %>%  select(-RINPERSOON) 
upset_data = as.data.frame(upset_data)
UpSetR::upset(upset_data, sets = colnames(upset_data),
              keep.order=F, order.by = "freq", set_size.show = T,
              mainbar.y.label = "Combinations of\nchronic conditions", sets.x.label = "", #show.numbers = "no",
              sets.bar.color = c("darkorange2","darkorange2","darkorange2","darkorange2","darkorange2",
                                 "darkblue","darkblue","darkblue","darkblue","darkblue",
                                 "darkblue","darkorange2","darkblue","darkblue"),
              nintersects = 13, mb.ratio = c(0.3, 0.7)) 
plot + grid.text(label = "The Netherlands, European classification", x = 0.06, y = .965, just = "left", gp = gpar(fontsize = 8))
grid.edit('arrange', name = 'arrange2'); UpsetNL_EU = grid.grab()

UPSETNL = cowplot::plot_grid(UpsetNL_NL, UpsetNL_EU, ncol=1, align = 'v', labels=c("A", "C")); UPSETNL
ggsave("H:/Results/Fig 4. Discordant population.pdf", UPSETNL, width = 18, height = 20, units = "cm", dpi = 320, limitsize = FALSE)

### Save related numbers in excel file 
Populationsize = risk2020  %>% filter(ECDCriskgroup != NLriskgroup) %>% summarise(n())
ECDC_overview = risk2020  %>% filter(ECDCriskgroup != NLriskgroup) %>%
  select(RINPERSOON, ECDCriskgroup, NLriskgroup, contains(c("ECDC_")))  %>% 
  mutate(ECDC_neur = ifelse(ECDC_cva==1, 1, ECDC_neur),
         ECDC_lung  = ifelse(ECDC_asthma==1, 1, ECDC_lung)) %>% select(-ECDC_cva, -ECDC_asthma) %>% 
  pivot_longer(!c(RINPERSOON, ECDCriskgroup, NLriskgroup), names_to="name", values_to = "value") %>% 
  filter(value==1)%>%   select(-ECDCriskgroup, -NLriskgroup) %>%  disease_names()  %>% 
  group_by(name) %>% summarise(n=n(), Prop =  as.numeric(n()/Populationsize*100)) %>% as.data.frame()
NL_overview = risk2020  %>% filter(ECDCriskgroup != NLriskgroup) %>%
  select(RINPERSOON, ECDCriskgroup, NLriskgroup, contains(c("NLHoog_", "NLMed_")))  %>% 
  pivot_longer(!c(RINPERSOON, ECDCriskgroup, NLriskgroup), names_to="name", values_to = "value") %>% 
  filter(value==1)%>%   select(-ECDCriskgroup, -NLriskgroup) %>%
  disease_names() %>% distinct() %>% 
  filter(!(grepl("NLMed_beentrans|NLHoog_med|NLMed_med", name))) %>% 
  group_by(name) %>% summarise(n=n(), Prop =  as.numeric(n()/Populationsize*100)) %>% as.data.frame()

# Create Workbook (= excel file)
wb <- createWorkbook()
# Add worksheet (=tabblad)
addWorksheet(wb = wb, sheetName = "Prop with disease")
# Write commentary about the output.
text <- "Overview of chronic conditions in population with discordance between Dutch and European classification"
writeData(wb = wb, sheet = "Prop with disease", x = text, startCol = 1, startRow = 1)
writeData(wb = wb, sheet = "Prop with disease", x = "Total n population = ", startCol = 1, startRow = 2)
writeData(wb = wb, sheet = "Prop with disease", x = Populationsize$`n()`, startCol = 2, startRow = 2)
writeData(wb = wb, sheet = "Prop with disease", x = "Dutch risk classification", startCol = 1, startRow = 4)
writeData(wb = wb, sheet = "Prop with disease", x = NL_overview, startCol = 1, startRow = 5, colNames = T)
writeData(wb = wb, sheet = "Prop with disease", x = "ECDC risk classification", startCol = 1, startRow = 22)
writeData(wb = wb, sheet = "Prop with disease", x = ECDC_overview, startCol = 1, startRow = 23, colNames = T)# Save the workbook.
#saveWorkbook(wb, "H:/Results/Fig 4. Population_Discordant Chronic conditions.xlsx", overwrite = T)


#### Figure 5 Upsetplots discordant aged <60 ####
library(UpSetR); library(grid)

### Discordance between classifications - NL
upset_data = risk2020  %>% filter(ECDCriskgroup != NLriskgroup & age<60) %>%
  select(RINPERSOON, ECDCriskgroup, NLriskgroup, contains(c("NLHoog_", "NLMed_")))  %>% 
  pivot_longer(!c(RINPERSOON, ECDCriskgroup, NLriskgroup), names_to="name", values_to = "value") %>% 
  filter(value==1)%>%   select(-ECDCriskgroup, -NLriskgroup) %>%
  disease_names() %>% distinct() %>% 
  filter(!(grepl("NLMed_beentrans|NLHoog_med|NLMed_med", name))) %>% 
  group_by(RINPERSOON) %>%   pivot_wider(names_from = name, values_from = value, values_fill = list(value=0)) %>% 
  ungroup() %>%  select(-RINPERSOON) 
upset_data = as.data.frame(upset_data)
UpSetR::upset(upset_data, sets = colnames(upset_data),
              keep.order=F, order.by = "freq", set_size.show = T, #show.numbers = "no",
              mainbar.y.label = "Combinations of\nchronic conditions", sets.x.label = "",
              sets.bar.color = c("darkblue","darkblue","darkblue","darkblue","darkblue",
                                 "darkblue","darkorange2","darkorange2","darkorange2","darkblue",
                                 "darkorange2","darkblue","darkorange2","darkblue"),
              nintersects = 10, mb.ratio = c(0.3, 0.7))   
plot + grid.text(label = "The Netherlands, National classification", x = 0.06, y = .965, just = "left", gp = gpar(fontsize = 8))
grid.edit('arrange', name = 'arrange2'); UpsetNL_NL = grid.grab()

### Discordance between classifications - ECDC
upset_data = risk2020  %>% filter(ECDCriskgroup != NLriskgroup & age<60) %>%
  select(RINPERSOON, ECDCriskgroup, NLriskgroup, contains(c("ECDC_")))  %>% 
  mutate(ECDC_neur = ifelse(ECDC_cva==1, 1, ECDC_neur),
         ECDC_lung  = ifelse(ECDC_asthma==1, 1, ECDC_lung)) %>% select(-ECDC_cva, -ECDC_asthma) %>% 
  pivot_longer(!c(RINPERSOON, ECDCriskgroup, NLriskgroup), names_to="name", values_to = "value") %>% 
  filter(value==1)%>%   select(-ECDCriskgroup, -NLriskgroup) %>%
  disease_names() %>% distinct() %>% 
  group_by(RINPERSOON) %>%   pivot_wider(names_from = name, values_from = value, values_fill = list(value=0)) %>% 
  ungroup() %>%  select(-RINPERSOON) 
upset_data = as.data.frame(upset_data)
UpSetR::upset(upset_data, sets = colnames(upset_data),
              keep.order=F, order.by = "freq", set_size.show = T,
              mainbar.y.label = "Combinations of\nchronic conditions", sets.x.label = "", #show.numbers = "no",
              sets.bar.color = c("darkorange2","darkorange2","darkorange2","darkorange2","darkorange2",
                                 "darkblue","darkblue","darkblue","darkblue","darkblue",
                                 "darkblue","darkorange2","darkblue","darkblue"),
              nintersects = 8, mb.ratio = c(0.3, 0.7)) 
plot + grid.text(label = "The Netherlands, European classification", x = 0.06, y = .965, just = "left", gp = gpar(fontsize = 8))
grid.edit('arrange', name = 'arrange2'); UpsetNL_EU = grid.grab()

UPSETNL = cowplot::plot_grid(UpsetNL_NL, UpsetNL_EU, ncol=1, align = 'v', labels=c("A", "C")); UPSETNL
ggsave("H:/Results/Fig 5. Discordant population_60.pdf", UPSETNL, width = 18, height = 20, units = "cm", dpi = 320, limitsize = FALSE)

### Save related numbers in excel file 
Populationsize = risk2020  %>% filter(ECDCriskgroup != NLriskgroup & age<60) %>% summarise(n());Populationsize
ECDC_overview = risk2020  %>% filter(ECDCriskgroup != NLriskgroup & age<60) %>%
  select(RINPERSOON, ECDCriskgroup, NLriskgroup, contains(c("ECDC_")))  %>% 
  mutate(ECDC_neur = ifelse(ECDC_cva==1, 1, ECDC_neur),
         ECDC_lung  = ifelse(ECDC_asthma==1, 1, ECDC_lung)) %>% select(-ECDC_cva, -ECDC_asthma) %>% 
  pivot_longer(!c(RINPERSOON, ECDCriskgroup, NLriskgroup), names_to="name", values_to = "value") %>% 
  filter(value==1)%>%   select(-ECDCriskgroup, -NLriskgroup) %>%  disease_names()  %>% 
  group_by(name) %>% summarise(n=n(), Prop =  as.numeric(n()/Populationsize*100)) %>% as.data.frame()
NL_overview = risk2020  %>% filter(ECDCriskgroup != NLriskgroup & age<60) %>%
  select(RINPERSOON, ECDCriskgroup, NLriskgroup, contains(c("NLHoog_", "NLMed_")))  %>% 
  pivot_longer(!c(RINPERSOON, ECDCriskgroup, NLriskgroup), names_to="name", values_to = "value") %>% 
  filter(value==1)%>%   select(-ECDCriskgroup, -NLriskgroup) %>%
  disease_names() %>% distinct() %>% 
  filter(!(grepl("NLMed_beentrans|NLHoog_med|NLMed_med", name))) %>% 
  group_by(name) %>% summarise(n=n(), Prop =  as.numeric(n()/Populationsize*100)) %>% as.data.frame()

# Create Workbook (= excel file)
wb <- createWorkbook()
# Add worksheet (=tabblad)
addWorksheet(wb = wb, sheetName = "Prop with disease")
# Write commentary about the output.
text <- "Overview of chronic conditions in population with discordance between Dutch and European classification"
writeData(wb = wb, sheet = "Prop with disease", x = text, startCol = 1, startRow = 1)
writeData(wb = wb, sheet = "Prop with disease", x = "Total n population = ", startCol = 1, startRow = 2)
writeData(wb = wb, sheet = "Prop with disease", x = Populationsize$`n()`, startCol = 2, startRow = 2)
writeData(wb = wb, sheet = "Prop with disease", x = "Dutch risk classification", startCol = 1, startRow = 4)
writeData(wb = wb, sheet = "Prop with disease", x = NL_overview, startCol = 1, startRow = 5, colNames = T)
writeData(wb = wb, sheet = "Prop with disease", x = "ECDC risk classification", startCol = 1, startRow = 22)
writeData(wb = wb, sheet = "Prop with disease", x = ECDC_overview, startCol = 1, startRow = 23, colNames = T)# Save the workbook.
#saveWorkbook(wb, "H:/Results/Fig 5. Population_Discordant Chronic conditions_Age below 60.xlsx", overwrite = T)



#### Figure 6 Upsetplots discordant aged <65 ####
library(UpSetR); library(grid)

### Discordance between classifications - NL
upset_data = risk2020  %>% filter(ECDCriskgroup != NLriskgroup & age<65) %>%
  select(RINPERSOON, ECDCriskgroup, NLriskgroup, contains(c("NLHoog_", "NLMed_")))  %>% 
  pivot_longer(!c(RINPERSOON, ECDCriskgroup, NLriskgroup), names_to="name", values_to = "value") %>% 
  filter(value==1)%>%   select(-ECDCriskgroup, -NLriskgroup) %>%
  disease_names() %>% distinct() %>% 
  filter(!(grepl("NLMed_beentrans|NLHoog_med|NLMed_med", name))) %>% 
  group_by(RINPERSOON) %>%   pivot_wider(names_from = name, values_from = value, values_fill = list(value=0)) %>% 
  ungroup() %>%  select(-RINPERSOON) 
upset_data = as.data.frame(upset_data)
UpSetR::upset(upset_data, sets = colnames(upset_data),
              keep.order=F, order.by = "freq", set_size.show = T, #show.numbers = "no",
              mainbar.y.label = "Combinations of\nchronic conditions", sets.x.label = "",
              sets.bar.color = c("darkblue","darkblue","darkblue","darkblue","darkblue",
                                 "darkblue","darkorange2","darkorange2","darkorange2","darkblue",
                                 "darkorange2","darkblue","darkorange2","darkblue"),
              nintersects = 10, mb.ratio = c(0.3, 0.7))   
plot + grid.text(label = "The Netherlands, National classification", x = 0.06, y = .965, just = "left", gp = gpar(fontsize = 8))
grid.edit('arrange', name = 'arrange2'); UpsetNL_NL = grid.grab()

### Discordance between classifications - ECDC
upset_data = risk2020  %>% filter(ECDCriskgroup != NLriskgroup & age<65) %>%
  select(RINPERSOON, ECDCriskgroup, NLriskgroup, contains(c("ECDC_")))  %>% 
  mutate(ECDC_neur = ifelse(ECDC_cva==1, 1, ECDC_neur),
         ECDC_lung  = ifelse(ECDC_asthma==1, 1, ECDC_lung)) %>% select(-ECDC_cva, -ECDC_asthma) %>% 
  pivot_longer(!c(RINPERSOON, ECDCriskgroup, NLriskgroup), names_to="name", values_to = "value") %>% 
  filter(value==1)%>%   select(-ECDCriskgroup, -NLriskgroup) %>%
  disease_names() %>% distinct() %>% 
  group_by(RINPERSOON) %>%   pivot_wider(names_from = name, values_from = value, values_fill = list(value=0)) %>% 
  ungroup() %>%  select(-RINPERSOON) 
upset_data = as.data.frame(upset_data)
UpSetR::upset(upset_data, sets = colnames(upset_data),
              keep.order=F, order.by = "freq", set_size.show = T,
              mainbar.y.label = "Combinations of\nchronic conditions", sets.x.label = "", #show.numbers = "no",
              sets.bar.color = c("darkorange2","darkorange2","darkorange2","darkorange2","darkorange2",
                                 "darkblue","darkblue","darkblue","darkblue","darkblue",
                                 "darkorange2","darkblue","darkblue","darkblue"),
              nintersects = 8, mb.ratio = c(0.3, 0.7)) 
plot + grid.text(label = "The Netherlands, European classification", x = 0.06, y = .965, just = "left", gp = gpar(fontsize = 8))
grid.edit('arrange', name = 'arrange2'); UpsetNL_EU = grid.grab()

UPSETNL = cowplot::plot_grid(UpsetNL_NL, UpsetNL_EU, ncol=1, align = 'v', labels=c("A", "C")); UPSETNL
ggsave("H:/Results/Fig 6. Discordant population_65.pdf", UPSETNL, width = 18, height = 20, units = "cm", dpi = 320, limitsize = FALSE)

### Save related numbers in excel file 
Populationsize = risk2020  %>% filter(ECDCriskgroup != NLriskgroup & age<65) %>% summarise(n());Populationsize
ECDC_overview = risk2020  %>% filter(ECDCriskgroup != NLriskgroup & age<65) %>%
  select(RINPERSOON, ECDCriskgroup, NLriskgroup, contains(c("ECDC_")))  %>% 
  mutate(ECDC_neur = ifelse(ECDC_cva==1, 1, ECDC_neur),
         ECDC_lung  = ifelse(ECDC_asthma==1, 1, ECDC_lung)) %>% select(-ECDC_cva, -ECDC_asthma) %>% 
  pivot_longer(!c(RINPERSOON, ECDCriskgroup, NLriskgroup), names_to="name", values_to = "value") %>% 
  filter(value==1)%>%   select(-ECDCriskgroup, -NLriskgroup) %>%  disease_names()  %>% 
  group_by(name) %>% summarise(n=n(), Prop =  as.numeric(n()/Populationsize*100)) %>% as.data.frame()
NL_overview = risk2020  %>% filter(ECDCriskgroup != NLriskgroup & age<65) %>%
  select(RINPERSOON, ECDCriskgroup, NLriskgroup, contains(c("NLHoog_", "NLMed_")))  %>% 
  pivot_longer(!c(RINPERSOON, ECDCriskgroup, NLriskgroup), names_to="name", values_to = "value") %>% 
  filter(value==1)%>%   select(-ECDCriskgroup, -NLriskgroup) %>%
  disease_names() %>% distinct() %>% 
  filter(!(grepl("NLMed_beentrans|NLHoog_med|NLMed_med", name))) %>% 
  group_by(name) %>% summarise(n=n(), Prop =  as.numeric(n()/Populationsize*100)) %>% as.data.frame()

# Create Workbook (= excel file)
wb <- createWorkbook()
# Add worksheet (=tabblad)
addWorksheet(wb = wb, sheetName = "Prop with disease")
# Write commentary about the output.
text <- "Overview of chronic conditions in population with discordance between Dutch and European classification"
writeData(wb = wb, sheet = "Prop with disease", x = text, startCol = 1, startRow = 1)
writeData(wb = wb, sheet = "Prop with disease", x = "Total n population = ", startCol = 1, startRow = 2)
writeData(wb = wb, sheet = "Prop with disease", x = Populationsize$`n()`, startCol = 2, startRow = 2)
writeData(wb = wb, sheet = "Prop with disease", x = "Dutch risk classification", startCol = 1, startRow = 4)
writeData(wb = wb, sheet = "Prop with disease", x = NL_overview, startCol = 1, startRow = 5, colNames = T)
writeData(wb = wb, sheet = "Prop with disease", x = "ECDC risk classification", startCol = 1, startRow = 22)
writeData(wb = wb, sheet = "Prop with disease", x = ECDC_overview, startCol = 1, startRow = 23, colNames = T)# Save the workbook.
#saveWorkbook(wb, "H:/Results/Fig 6. Population_Discordant Chronic conditions_Age below 65.xlsx", overwrite = T)




#### Suppl Fig 1:  ####
library(UpSetR); library(grid)

risk2020med = risk2020 %>% 
  mutate(NLHoog_immunosup = ifelse(NLHoogMED_immunosup == 1, 1, NLHoog_immunosup),
         NLMed_cvd = ifelse(NLMedMED_cvd == 1, 1, NLMed_cvd),
         NLMed_demen = ifelse(NLMedMED_demen == 1, 1, NLMed_demen),
         NLMed_lucht = ifelse(NLMedMED_lucht == 1, 1, NLMed_lucht),
         NLMed_verwee = ifelse(NLMedMED_verwee == 1, 1, NLMed_verwee))  %>% 
  select(-NLHoog_immunosup, -NLMed_cvd, -NLMed_demen, -NLMed_lucht, -NLMed_verwee)

### Discordance between classifications - NL
upset_data = risk2020med  %>% filter(!NLriskgroup_med=="laag") %>%
  select(RINPERSOON, ECDCriskgroup, NLriskgroup_med, contains(c("NLHoog_", "NLMed_")))  %>% 
  pivot_longer(!c(RINPERSOON, ECDCriskgroup, NLriskgroup_med), names_to="name", values_to = "value") %>% 
  filter(value==1)%>%   select(-ECDCriskgroup, -NLriskgroup_med) %>%  disease_names() %>% distinct() %>% 
  filter(!grepl("NLMed_beentrans|NLMed_med|NLHoog_med", name)) %>% 
   group_by(RINPERSOON) %>%   pivot_wider(names_from = name, values_from = value, values_fill = list(value=0)) %>% 
  ungroup() %>%  select(-RINPERSOON) 
upset_data = as.data.frame(upset_data)
UpSetR::upset(upset_data, sets = colnames(upset_data),
              keep.order=F, order.by = "freq", set_size.show = T, #show.numbers = "no",
              mainbar.y.label = "Combinations of\nchronic conditions", sets.x.label = "",
              sets.bar.color = c("darkblue","darkblue","darkblue","darkorange2","darkorange2",
                                 "darkorange2","darkorange2","darkorange2","darkorange2","darkblue",
                                 "darkorange2","darkblue","darkorange2"),
              nintersects = 5, mb.ratio = c(0.3, 0.7), text.scale = c(1,1,1,1,1,0.85))  
plot + grid.text(label = "The Netherlands, National classification", x = 0.06, y = .965, just = "left", gp = gpar(fontsize = 8))
grid.edit('arrange', name = 'arrange2'); UpsetNL_NL = grid.grab()


### Save related numbers in excel file 
Populationsize = risk2020med  %>% summarise(n())
NL_overview = risk2020med  %>% filter(!NLriskgroup=="laag") %>%
  select(RINPERSOON, ECDCriskgroup, NLriskgroup, contains(c("NLHoog_", "NLMed_")))  %>% 
  pivot_longer(!c(RINPERSOON, ECDCriskgroup, NLriskgroup), names_to="name", values_to = "value") %>% 
  filter(value==1)%>%   select(-ECDCriskgroup, -NLriskgroup) %>%
  disease_names() %>% distinct() %>% 
  filter(!(grepl("NLMed_beentrans|NLHoog_med|NLMed_med", name))) %>% 
  group_by(name) %>% summarise(n=n(), Prop =  as.numeric(n()/Populationsize*100)) %>% as.data.frame()

# Create Workbook (= excel file)
wb <- createWorkbook()
# Add worksheet (=tabblad)
addWorksheet(wb = wb, sheetName = "General population")
# Write commentary about the output.
text <- "Overview of chronic conditions in population with discordance between Dutch and European classification"
writeData(wb = wb, sheet = "General population", x = text, startCol = 1, startRow = 1)
writeData(wb = wb, sheet = "General population", x = "Total n population = ", startCol = 1, startRow = 2)
writeData(wb = wb, sheet = "General population", x = Populationsize$`n()`, startCol = 2, startRow = 2)
writeData(wb = wb, sheet = "General population", x = "Dutch risk classification", startCol = 1, startRow = 4)
writeData(wb = wb, sheet = "General population", x = NL_overview, startCol = 1, startRow = 5, colNames = T)# Save the workbook.
#saveWorkbook(wb, "H:/Results/Suppl Fig 1. General population w prescription data.xlsx", overwrite = T)


group.colors = c('hoog' = "#EA6A47", 'medium' = "#194180", "laag" = "#D6E6D6")
NLriskgroups2020_med <-  risk2020 %>% 
  group_by(fac_NLriskgroup_med, fac_agecat) %>% 
  summarise(n=n(), .groups = "drop") %>% 
  complete(fac_NLriskgroup_med, fac_agecat, fill = list(n = 0)) %>%
  mutate(agecat = ifelse(fac_agecat=="[0,10)", "0-9", 
                         ifelse(fac_agecat=="[10,20)", "10-19",
                                ifelse(fac_agecat=="[20,30)", "20-29",
                                       ifelse(fac_agecat=="[30,40)", "30-39",
                                              ifelse(fac_agecat=="[40,50)", "40-49",
                                                     ifelse(fac_agecat=="[50,60)", "50-59",
                                                            ifelse(fac_agecat=="[60,70)", "60-69",
                                                                   ifelse(fac_agecat=="[70,80)", "70-79",
                                                                          ifelse(fac_agecat=="[80,Inf)", "80+", fac_agecat))))))))))
baseline_NL1 = NLriskgroups2020_med %>% ggplot(aes(fac_agecat, fill = fac_NLriskgroup_med)) + geom_col(aes(y=n)) + 
    scale_y_continuous(breaks = seq(600000, to = 2500000, by= 600000), labels = scales::comma, limits = c(0, 2600000), expand = c(0,0)) +
    theme_bw() + scale_fill_manual(values = group.colors) +
    theme(legend.position = "none", axis.title.x = element_blank(), axis.line.x = element_blank(), axis.ticks.x = element_blank(),
          plot.margin = unit(c(0.3, 0.2, 0, 0.2), "cm")) + labs(y="\nPopulation\n", title = "The Netherlands, National classification")
baseline_NL2 = NLriskgroups2020_med %>% ggplot(aes(agecat, fill = fac_NLriskgroup_med)) + geom_col(aes(y=n), position = "fill") + 
  guides(fill=guide_legend(nrow=3)) +
  theme_bw() + scale_fill_manual(values = group.colors) +
  theme(plot.margin = unit(c(0, 0.2, 0.2, 0.2), "cm"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_y_continuous(labels = function(x) paste(x*100, "%"), expand = c(0.005,0)) +
  labs(y="\n", fill = "Risk", x="Age\n")
BARPLOTS = cowplot::plot_grid(baseline_NL1 + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank()),
                              baseline_NL2 + theme(legend.position = "none"),
                              align = "v", ncol = 1, rel_heights = c(1, 1.3)) ; BARPLOTS
 
legend <- cowplot::get_legend(baseline_NL1 + theme(legend.position = "bottom", legend.title = element_text(size = 14), legend.text = element_text(size = 14)) + 
                                scale_fill_manual(values=group.colors, labels = c("High", "Moderate", "Low")) + labs(fill = "Risk due to chronic medical conditions"))

UPSETNL = cowplot::plot_grid(UpsetNL_NL, BARPLOTS, ncol=1, align = 'v', labels=c("A", "B")); UPSETNL
UPSETNL = cowplot::plot_grid(legend, UPSETNL, ncol = 1, nrow = 2, rel_heights = c(0.045, 1))
ggsave("H:/Results/Suppl Fig 1. General population w prescription data.pdf", UPSETNL, width = 20, height = 20, units = "cm", dpi = 320, limitsize = FALSE)

####### End ####### 