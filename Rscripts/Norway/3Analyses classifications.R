################################################
## Goal: Describe COVID-19 disease burden in Norway
## Version/date: V1.0/01-08-2024
## Authos: I. Westerhof
## R-version: 4.4.1
################################################

### Clean enviroment and memory
rm(list=ls())

### 1. Load packages
library(haven); library(dplyr); library(readxl); library("RColorBrewer"); library(readr); library(stringr); library(tidyr); library(data.table); library(ggplot2); library(caret); library("gridExtra"); library(UpSetR); library(grid)

### 2. Set work directory
setwd("N:/durable/Ilse/2023")

### 3. load data
load("1. Data_Classifications/BaselineOverview2020.RData")  

### 4. prep data
age_group_levels <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
age_groups <- c(-1, 10, 20,30,40,50,60,70,80, Inf)
assign_age_group <- function(age) {
  group_labels <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
  group <- findInterval(age, age_groups);  group_labels[group]}
desired_order_Agegroups <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"); desired_order_Risk <- c("LR", "MR", "HR"); desired_order_Immunity <- c("NoImmunity", "WanedImmunity", "ModerateImmunity", "HighImmunity")

BaselineOverview <- BaselineOverviewDiseases  %>% 
  mutate(AgeGroup = assign_age_group(Age)) %>% 
  mutate(AgeGroup = factor(AgeGroup, levels = desired_order_Agegroups)) %>% arrange(AgeGroup) %>%
  select(id, Age, AgeGroup, Gender, contains("Morbidity"))

group.colors <- c("High risk" = "#EA6A47", "Moderate risk" = "#194180", "Low risk" = "#D6E6D6")
disease_names <- function(upset_data, mutation) { upset_data %>%
    mutate(name = ifelse(name == "Morbidity_FHI_ChronicPulmonaryDiseases_MR", "Lung disease (MR)", 
                         ifelse(name == "Morbidity_ECDC_LungDisease_Mandatory", "Lung disease (HR)", 
                                ifelse(name == "Morbidity_ECDC_Dementia_Optional", "Dementia (MR)",  
                                       ifelse(name == "Morbidity_FHI_NeurologicalMusculoskeletalDisorders_HR", "Neurological/-muscular disorder (HR)",   
                                              ifelse(name == "Morbidity_FHI_Organtranspl_HR", "Organ transplant (HR)",
                                                     ifelse(name == "Morbidity_ECDC_Immunodeficiency_Mandatory", "Immunodeficiency (HR)",
                                                            ifelse(name == "Morbidity_FHI_Diabetes_MR", "Diabetes mellitus (MR)",
                                                                   ifelse(name == "Morbidity_ECDC_Diabetes_Mandatory", "Diabetes mellitus (HR)",
                                                                          ifelse(name == "Morbidity_FHI_CardiovascularDiseases_MR", "Cardiovascular disease (MR)",
                                                                                 ifelse(name == "Morbidity_FHI_Stroke_MR", "Stroke (MR)",
                                                                                        ifelse(name == "Morbidity_ECDC_CardiovascularDiseases_Mandatory", "Cardiovascular disease (HR)",
                                                                                               ifelse(name == "Morbidity_FHI_Immunodeficiency_HR", "Immunodeficiency (HR)",
                                                                                                      ifelse(name == "Morbidity_FHI_OtherCancer" | name == "Morbidity_FHI_HeamatologicalCancer", "Cancer (HR)",
                                                                                                             ifelse(name == "Morbidity_ECDC_Cancer_Mandatory", "Cancer (HR)", 
                                                                                                                    ifelse(name == "Morbidity_ECDC_Renal_Optional", "Renal disease (MR)",
                                                                                                                           ifelse(name == "Morbidity_FHI_ChronicLiverDisease_HR", "Liver disease (HR)",
                                                                                                                                  ifelse(name == "Morbidity_FHI_ChronicKidneyLiverDisease_HR", "Kidney disease (HR)",
                                                                                                                                  ifelse(name == "Morbidity_ECDC_ChronicLiver_Optional", "Liver disease (MR)",
                                                                                                                                         ifelse(name == "Morbidity_ECDC_Hypertension_Mandatory", "Hypertension (HR)",
                                                                                                                                                ifelse(name == "Morbidity_FHI_Obesity_MR", "Morbide obesity (MR)", 
                                                                                                                                                       ifelse(name == "Morbidity_ECDC_Anaemia_Optional", "Anemia (MR)",       
                                                                                                                                                              ifelse(name == "Morbidity_FHI_CompromisedImmuneFunction_MR", "Compromised immune system (MR)",  
                                                                                                                                                                     ifelse(name == "Morbidity_ECDC_Rheumatologic_Optional", "Rheumatologic disease (MR)",  
                                                                                                                                                                            ifelse(name == "Morbidity_ECDC_Stroke_Optional", "Stroke (MR)", 
                                                                                                                                                                                   ifelse(name == "Morbidity_FHI_Dementia_MR", "Dementia (MR)",  
                                                                                                                                                                                          ifelse(name == "Morbidity_ECDC_Obesity_Mandatory", "Morbide obesity (HR)", 
                                                                                                                                                                                                 ifelse(name == "Morbidity_ECDC_Neuromuscular_Mandatory", "Neurological disorder (MR)", 
                                                                                                                                                                                                        ifelse(name == "Morbidity_ECDC_Tuberculosis_Optional", "Tuberculosis (MR)", 
                                                                                                                                                                                                        ifelse(name == "Morbidity_FHI_ICD10_DownSyndrome_HR", "Down syndrome (HR)", 
                                                                                                                                                                                                        name))))))))))))))))))))))))))))))}

######### Analyses ######### 
#### Fig 1. Upset plots general population ####
popsize = BaselineOverview %>% summarise(n=n())
upset_data = BaselineOverviewDiseases %>% filter(!(Morbidity_FHI_COVIDcat =="Low risk" & Morbidity_ECDC_COVIDcat=="Low risk")) %>%
  select(contains(c("id", "Morbidity_"))) %>% select(-contains("cat"))%>%  pivot_longer(-id) %>% filter(value==1)
upset_data = upset_data %>% filter(grepl("FHI", name)) %>%  disease_names();gc()
upset_data %>% group_by(name) %>% summarise(n(), n()/popsize$n*100)
upset_data = upset_data %>% pivot_wider(names_from = name, values_from = value, values_fill = 0, values_fn = sum) %>%    select(-id); gc()
upset_data = as.data.frame(upset_data)
UpSetR::upset(upset_data, sets = colnames(upset_data), 
              keep.order = F, order.by = "freq",set_size.show = T,
              mainbar.y.label = "Combinations of\nchronic conditions", sets.x.label = "",
              sets.bar.color = c("darkblue", "darkblue", "darkblue", "darkorange2", "darkblue", "darkblue", 
                                 "darkblue", "darkorange2", "darkblue", "darkorange2", "darkorange2", "darkorange2", "darkorange2", "darkorange2"),
              nintersects = 10, mb.ratio = c(0.3, 0.7))
plot +  grid.text(label = "Norway, National classification", x = 0.06, y = .965, just = "left", gp = gpar(fontsize = 8))
grid.edit('arrange', name = 'arrange2'); UpsetNO_FHI = grid.grab()

upset_data = BaselineOverviewDiseases %>% filter(!(Morbidity_FHI_COVIDcat =="Low risk" & Morbidity_ECDC_COVIDcat=="Low risk")) %>%
  select(contains(c("id", "Morbidity_"))) %>% select(-contains("cat"))%>%  pivot_longer(-id) %>% filter(value==1)
upset_data = upset_data %>% filter(grepl("ECDC", name)) %>%  disease_names();gc()
upset_data %>% group_by(name) %>% summarise(n(), n()/popsize$n*100)
upset_data = upset_data %>% pivot_wider(names_from = name, values_from = value, values_fill = 0, values_fn = sum) %>%    select(-id); gc()
upset_data = as.data.frame(upset_data)
UpSetR::upset(upset_data, sets = colnames(upset_data), 
              keep.order = F, order.by = "freq", set_size.show = T,
              mainbar.y.label = "Combinations of\nchronic conditions", sets.x.label = "",
              sets.bar.color = c("darkorange2", "darkorange2", "darkorange2", "darkorange2", "darkorange2", "darkblue", "darkblue", 
                                 "darkblue", "darkblue", "darkblue", "darkorange2", "darkorange2", "darkblue", "darkblue", "darkblue"),
              nintersects = 13, mb.ratio = c(0.3, 0.7))
plot +  grid.text(label = "Norway, European classification", x = 0.06, y = .965, just = "left", gp = gpar(fontsize = 8))
grid.edit('arrange', name = 'arrange2'); UpsetNO_ECDC = grid.grab()

UPSETNO = cowplot::plot_grid(UpsetNO_FHI, UpsetNO_ECDC, ncol=1, align = "v", labels=c("B", "D"));UPSETNO
ggsave("3. Output/Fig 1. Upsetplots.pdf", UPSETNO, width = 18, height = 20, units = "cm", dpi=320, limitsize = FALSE)

#### Fig 2. Histograms ####
# A) Histograms Population the Netherlands by national classification stratified by age
# B) Histograms Population Norway by national classification stratified by age
# C) Histograms Population the Netherlands by European classification stratified by age
# D) Histograms Population Norway by European classification stratified by age

# A) Histograms Population the Netherlands by national classification stratified by age
data <- read.table(text = "High Moderate Low
                           3640	188863	1563929
                           6068	70189	1922505
                           11390	105070	2114908
                           21422	121515	2005673
                           30490	155190	2016989
                           50194	266511	2217840
                           55560	349318	1712165
                           53538	400220	1128015
                           25335	316546	489196", header = TRUE, stringsAsFactors = FALSE)
data$AgeGroup = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
data_long <- data %>%  gather(key = "Morbidity_GR_COVIDcat", value = "Aantal", -AgeGroup) %>%
  mutate(Morbidity_GR_COVIDcat = paste(Morbidity_GR_COVIDcat, "risk", sep=" "))%>%
  mutate(Morbidity_GR_COVIDcat = factor(Morbidity_GR_COVIDcat, levels = c("High risk", 'Moderate risk', 'Low risk'))) %>% arrange(Morbidity_GR_COVIDcat) 

BaselineGRNL1 <- data_long %>%ggplot(aes(AgeGroup, fill=Morbidity_GR_COVIDcat)) + geom_col(aes(y=Aantal)) + 
  scale_y_continuous(breaks = seq(from = 600000, to = 3900000, by = 600000), labels = scales::comma, limits=c(0,2600000), expand= c(0,0)) +
  theme_bw() + scale_fill_manual(values=group.colors) +  
  theme(legend.position="none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        plot.margin = unit(c(0.3, 0.2, 0, 0.2), "cm")) + labs(y="\nPopulation\n", title = "The Netherlands, National classification")
BaselineGRNL2 <- data_long  %>%  ggplot(aes(AgeGroup, fill=Morbidity_GR_COVIDcat)) + geom_col(aes(y=Aantal),position = "fill")  +
  guides(fill=guide_legend(nrow=3))+ 
  theme_bw() + scale_fill_manual(values=group.colors) +  theme(plot.margin = unit(c(0, 0.2, 0.2, 0.2), "cm"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  + 
  scale_y_continuous(labels = function(x) paste0(x*100, "%"), expand= c(0.005,0)) + labs(y="\n", fill="Risk", x="Age\n")

# B) Histograms Population Norway by national classification stratified by age
BaselineFHI1 <- BaselineOverview %>%  filter(!is.na(AgeGroup)) %>% 
  ggplot(aes(AgeGroup, fill=Morbidity_FHI_COVIDcat)) + geom_bar() + 
  scale_y_continuous(breaks = seq(from = 200000, to = 3900000, by = 200000), labels = scales::comma, limits=c(0,850000), expand= c(0,0)) +
  theme_bw() + scale_fill_manual(values=group.colors) +  
  theme(legend.position="none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        plot.margin = unit(c(0.3, 0.2, 0, 0.2), "cm")) + labs(y="\nPopulation\n", title = "Norway, National classification")
BaselineFHI2 <- BaselineOverview  %>% filter(!is.na(AgeGroup)) %>% #filter(Year=="2020-01-01") %>% 
  ggplot(aes(AgeGroup, fill=Morbidity_FHI_COVIDcat)) + geom_bar(stat="count", position = "fill")  +
  guides(fill=guide_legend(nrow=3))+ 
  theme_bw() + scale_fill_manual(values=group.colors) +  theme(plot.margin = unit(c(0, 0.2, 0.2, 0.2), "cm"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  + 
  scale_y_continuous(labels = function(x) paste0(x*100, "%"), expand= c(0.005,0)) + labs(y="\n", fill="Risk", x="Age\n")

# C) Histograms Population the Netherlands by European classification stratified by age
data <- read.table(text = "High Moderate Low
                           23617	8227	1724588
                           19873	5246	1973643
                           30553	12915	2187900
                           49175	19360	2080075
                           94484	19750	2088435
                           231073	24340	2279132
                           359469	23274	1734300
                           437509	24818	1119446
                           319909	27395	483773", header = TRUE, stringsAsFactors = FALSE)
data$AgeGroup = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
data_long <- data %>%  gather(key = "Morbidity_ECDC_COVIDcat", value = "Aantal", -AgeGroup) %>%
  mutate(Morbidity_ECDC_COVIDcat = paste(Morbidity_ECDC_COVIDcat, "risk", sep=" "))%>%
  mutate(Morbidity_ECDC_COVIDcat = factor(Morbidity_ECDC_COVIDcat, levels = c("High risk", 'Moderate risk', 'Low risk'))) %>% arrange(Morbidity_ECDC_COVIDcat) 
BaselineECDCNL1 <- data_long %>%   ggplot(aes(AgeGroup, fill=Morbidity_ECDC_COVIDcat)) + geom_col(aes(y=Aantal)) + 
  scale_y_continuous(breaks = seq(from = 600000, to = 3900000, by = 600000), labels = scales::comma, limits=c(0,2600000), expand= c(0,0)) +
  theme_bw() + scale_fill_manual(values=group.colors) +  
  theme(legend.position="none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        plot.margin = unit(c(0.3, 0.2, 0, 0.2), "cm")) + labs(y="\nPopulation\n", title = "The Netherlands, European classification")
BaselineECDCNL2 <- data_long  %>%  ggplot(aes(AgeGroup, fill=Morbidity_ECDC_COVIDcat)) + geom_col(aes(y=Aantal),position = "fill")  +
  guides(fill=guide_legend(nrow=3))+ 
  theme_bw() + scale_fill_manual(values=group.colors) +  theme(plot.margin = unit(c(0, 0.2, 0.2, 0.2), "cm"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  + 
  scale_y_continuous(labels = function(x) paste0(x*100, "%"), expand= c(0.005,0)) + labs(y="\n", fill="Risk", x="Age\n")

# D) Histograms Population Norway by European classification stratified by age
BaselineECDC1 <- BaselineOverview %>%    filter(!is.na(AgeGroup)) %>% 
  ggplot(aes(AgeGroup, fill=Morbidity_ECDC_COVIDcat)) + geom_bar() + 
  scale_y_continuous(breaks = seq(from = 200000, to = 3900000, by = 200000), labels = scales::comma, limits=c(0,850000), expand= c(0,0)) +
  theme_bw() + scale_fill_manual(values=group.colors) +  
  theme(legend.position="none", axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        plot.margin = unit(c(0.3, 0.2, 0, 0.2), "cm")) + labs(y="\nPopulation", title = "Norway, European classification")
BaselineECDC2 <- BaselineOverview %>%  filter(!is.na(AgeGroup)) %>% 
  ggplot(aes(AgeGroup, fill=Morbidity_ECDC_COVIDcat)) + geom_bar(stat="count", position = "fill")  +
  guides(fill=guide_legend(nrow=3))+ 
  theme_bw() + scale_fill_manual(values=group.colors, labels=c('High', 'Moderate', 'Low')) +  theme(plot.margin = unit(c(0, 0.2, 0.2, 0.2), "cm"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  + 
  scale_y_continuous(labels = function(x) paste0(x*100, "%"), expand= c(0.005,0)) + labs(y="\n", fill="Risk", x="Age\n")+ guides(fill = guide_legend(nrow = 1, byrow = TRUE))

BARPLOTS  = cowplot::plot_grid(BaselineGRNL1 + theme(axis.text.x = element_blank(),axis.ticks.x = element_blank(),axis.title.x = element_blank()), 
                               BaselineFHI1 + theme(axis.text.x = element_blank(),axis.ticks.x = element_blank(),axis.title.x = element_blank()), 
                               BaselineGRNL2 + theme(legend.position="none"), 
                               BaselineFHI2 + theme(legend.position="none"), 
                               BaselineECDCNL1 + theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()),
                               BaselineECDC1 + theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()),
                               BaselineECDCNL2 + theme(legend.position="none"), 
                               BaselineECDC2 + theme(legend.position="none"),
                               align = "v",                                ncol=2,rel_heights = c(1,1.3, 1,1.3),
                               labels=c("A", "B", "", "", "C", "D", "", "")) 
legend <- cowplot::get_legend(BaselineGRNL1 + theme(legend.position="bottom", legend.title = element_text(size = 14), legend.text = element_text(size = 14))+ 
                                scale_fill_manual(values=group.colors, labels=c('High', 'Moderate', 'Low'))  + labs(fill="Risk due to chronic medical conditions"))
cowplot::plot_grid(legend, BARPLOTS, ncol=1, nrow = 2, rel_heights = c(0.045, 1))    
ggsave("3. Output/Fig 2. Barplots baseline by age and national and ECDC classification for NL and Norway.pdf", width = 24.5, height = 20, units = "cm", dpi=320, limitsize = FALSE)


#### Fig 3. Discordance ####
# A) Discordance Population the Netherlands
# B) Discordance Population Norway
# C) Discordance Population the Netherlands aged <60
# D) Discordance Population the Norway aged <60
# E) Discordance Population the Netherlands aged <65
# F) Discordance Population the Norway aged <65
# G) Discordance Population the Netherlands aged <70
# H) Discordance Population the Norway aged <70

# A) Discordance Population the Netherlands
data <- read.table(text = "RIVM ECDC n
                           hoog	hoog	219646
                           hoog	medium	3932
                           hoog	laag	34059
                           medium	hoog	1112505
                           medium	medium	154776
                           medium	laag	706141
                           laag	hoog	233511
                           laag	medium	6617
                           laag	laag	14931092", header = TRUE, stringsAsFactors = FALSE)
long_format <- data.frame(ECDC = rep(data$ECDC, data$n), RIVM = rep(data$RIVM, data$n))
long_format$ECDC <- factor(long_format$ECDC, levels = c("hoog", "medium", "laag"))
long_format$RIVM <- factor(long_format$RIVM, levels = c("hoog", "medium", "laag"))
summary = caret::confusionMatrix(data = long_format$ECDC, reference = long_format$RIVM)
Total = sum(data$n)
NL_EU_Baseline_RIVM = data  %>%  group_by(RIVM) %>% summarise(n = sum(n), ECDC = "Total")
NL_EU_Baseline_ECDC = data  %>%  group_by(ECDC) %>% summarise(n = sum(n), RIVM = "Total")
NL_EU_Baseline = bind_rows(data, NL_EU_Baseline_RIVM, NL_EU_Baseline_ECDC) %>% add_row(ECDC = "Total", RIVM = "Total", n = Total)
NL_EU_Baseline = NL_EU_Baseline %>% 
  mutate(prop=n/Total, RIVM = factor(RIVM, levels = c("hoog", 'medium', 'laag', 'Total')), ECDC = factor(ECDC, levels = c("Total", 'laag', 'medium', "hoog"))) %>%
  ggplot(aes(x=RIVM, y=ECDC, fill=prop, label=paste0(round(prop*100, digits=1), "%"))) +  geom_tile() +
  scale_fill_gradient(low = "white", high = "white") +  
  geom_text(aes(label = ifelse(ECDC == RIVM, paste0(round(prop*100, digits=1), "%"), ""))) +  # Make diagonal elements bold
  geom_text(aes(label = ifelse(ECDC != RIVM, paste0(round(prop*100, digits=1), "%"), "")), fontface = "bold") +  # Make diagonal elements bold
  theme_bw() + theme(legend.position="none") +
  scale_x_discrete(labels=c("High", "Moderate", "Low", "Total")) +  scale_y_discrete(labels=c("Total", "Low", "Moderate", "High")) +
  labs(x="National classification", y="European\nclassification", title="The Netherlands")+
  theme(plot.caption = element_text(hjust = 0)) +
  labs(caption = "\nDiscordance = 12.0% (95%CI 12.0-12.1%)
Sensitivity:  High = 85.3%, Moderate = 7.8%, Low= 98.4%
Specificity:  High = 92.1%, Moderate = 99.9%, Low = 66.8%
PPV: High = 14.0%, Moderate = 93.6%, Low = 95.3%
NPV: High = 99.8%, Moderate = 89.4%, Low = 86.1%") +
  geom_hline(yintercept = 1.5, color = "black") +  geom_vline(xintercept = 3.5, color = "black") 

# B) Discordance Population Norway
NO_EU_Baseline=BaselineOverview 
NO_EU_Baseline$Morbidity_ECDC_COVIDcat <- factor(NO_EU_Baseline$Morbidity_ECDC_COVIDcat, levels = c("High risk", "Moderate risk", "Low risk"))
NO_EU_Baseline$Morbidity_FHI_COVIDcat <- factor(NO_EU_Baseline$Morbidity_FHI_COVIDcat, levels = c("High risk", "Moderate risk", "Low risk"))
summary = caret::confusionMatrix(data = NO_EU_Baseline$Morbidity_ECDC_COVIDcat, reference = NO_EU_Baseline$Morbidity_FHI_COVIDcat)
Total = NO_EU_Baseline %>% summarise(n=n())
NO_EU_Baseline = NO_EU_Baseline %>%   mutate(N=n()) %>% group_by(Morbidity_FHI_COVIDcat, Morbidity_ECDC_COVIDcat) %>% summarise(n=n())
NO_EU_Baseline_FHI = NO_EU_Baseline  %>%  group_by(Morbidity_FHI_COVIDcat) %>% summarise(n = sum(n), Morbidity_ECDC_COVIDcat = "Total")
NO_EU_Baseline_ECDC = NO_EU_Baseline  %>%  group_by(Morbidity_ECDC_COVIDcat) %>% summarise(n = sum(n), Morbidity_FHI_COVIDcat = "Total")
NO_EU_Baseline = bind_rows(NO_EU_Baseline, NO_EU_Baseline_FHI, NO_EU_Baseline_ECDC)%>% ungroup() %>% add_row(Morbidity_ECDC_COVIDcat = "Total", Morbidity_FHI_COVIDcat = "Total", n = as.numeric(Total$n)) %>% ungroup()
NO_EU_Baseline = NO_EU_Baseline %>%   mutate(prop = as.numeric(n/Total$n))  %>% 
  mutate(Morbidity_FHI_COVIDcat = factor(Morbidity_FHI_COVIDcat, levels = c("High risk", "Moderate risk", "Low risk", 'Total')), Morbidity_ECDC_COVIDcat = factor(Morbidity_ECDC_COVIDcat, levels = c("Total", "Low risk", "Moderate risk", "High risk"))) %>%
  ggplot(aes(x=Morbidity_FHI_COVIDcat, y=Morbidity_ECDC_COVIDcat, fill=prop, label=paste0(round(prop*100, digits=1), "%"))) +  geom_tile() +
  scale_fill_gradient(low = "white", high = "white") +  
  geom_text(aes(label = ifelse(Morbidity_ECDC_COVIDcat == Morbidity_FHI_COVIDcat, paste0(round(prop*100, digits=1), "%"), ""))) +  # Make diagonal elements bold
  geom_text(aes(label = ifelse(Morbidity_ECDC_COVIDcat != Morbidity_FHI_COVIDcat, paste0(round(prop*100, digits=1), "%"), "")), fontface = "bold") +  # Make diagonal elements bold
  theme_bw() + theme(legend.position="none") +
  scale_x_discrete(labels=c("High", "Moderate", "Low", "Total")) +  scale_y_discrete(labels=c("Total", "Low", "Moderate", "High")) +
  labs(x="National classification", y="European\nclassification", title="Norway")+
  theme(plot.caption = element_text(hjust = 0)) +
  labs(caption = "\nDiscordance = 13.8% (95%CI 13.8-13.9%)
Sensitivity: High = 83.8%, Moderate = 4,6%, Low = 98.5%
Specificity: High = 93.1%, Moderate = 99.2%, Low = 59.0%
PPV: High = 27.0%, Moderate = 44.6%, Low = 92.9%
NPV: High = 99.5%, Moderate = 87.8%, Low = 87.9%") +
  geom_hline(yintercept = 1.5, color = "black") +  geom_vline(xintercept = 3.5, color = "black") 


# C) Discordance Population the Netherlands aged <60
data <- read.table(text = "RIVM ECDC n
                           hoog	hoog	89888
                           hoog	medium	2837
                           hoog	laag	30479
                           medium	hoog	283631
                           medium	medium	85090
                           medium	laag	538617
                           laag	hoog	75256
                           laag	medium	1911
                           laag	laag	11764677", header = TRUE, stringsAsFactors = FALSE)
long_format <- data.frame(ECDC = rep(data$ECDC, data$n), RIVM = rep(data$RIVM, data$n))
long_format$ECDC <- factor(long_format$ECDC, levels = c("hoog", "medium", "laag"))
long_format$RIVM <- factor(long_format$RIVM, levels = c("hoog", "medium", "laag"))
summary = caret::confusionMatrix(data = long_format$ECDC, reference = long_format$RIVM)
Total = sum(data$n)
NL_EU_Baseline_RIVM = data  %>%  group_by(RIVM) %>% summarise(n = sum(n), ECDC = "Total")
NL_EU_Baseline_ECDC = data  %>%  group_by(ECDC) %>% summarise(n = sum(n), RIVM = "Total")
NL_EU_Baseline60 = bind_rows(data, NL_EU_Baseline_RIVM, NL_EU_Baseline_ECDC) %>% add_row(ECDC = "Total", RIVM = "Total", n = Total)
NL_EU_Baseline60 = NL_EU_Baseline60 %>% 
  mutate(prop=n/Total, RIVM = factor(RIVM, levels = c("hoog", 'medium', 'laag', 'Total')), ECDC = factor(ECDC, levels = c("Total", 'laag', 'medium', "hoog"))) %>%
  ggplot(aes(x=RIVM, y=ECDC, fill=prop, label=paste0(round(prop*100, digits=1), "%"))) +  geom_tile() +
  scale_fill_gradient(low = "white", high = "white") +  
  geom_text(aes(label = ifelse(ECDC == RIVM, paste0(round(prop*100, digits=1), "%"), ""))) +  # Make diagonal elements bold
  geom_text(aes(label = ifelse(ECDC != RIVM, paste0(round(prop*100, digits=1), "%"), "")), fontface = "bold") +  # Make diagonal elements bold
  theme_bw() + theme(legend.position="none") +
  scale_x_discrete(labels=c("High", "Moderate", "Low", "Total")) +  scale_y_discrete(labels=c("Total", "Low", "Moderate", "High")) +
  labs(x="National classification", y="European\nclassification", title="The Netherlands, aged <60")+
  theme(plot.caption = element_text(hjust = 0)) +
  labs(caption = "\nDiscordance = 7.3% (95%CI 7.2-7.3%)
Sensitivity:  High = 73.0%, Moderate = 9.4%, Low= 99.4%
Specificity:  High = 97.2%, Moderate = >99.9%, Low = 44.8%
PPV: High = 20.0%, Moderate = 94.7%, Low = 95.4%
NPV: High = 99.7%, Moderate = 93.6%, Low = 85.7%") +
  geom_hline(yintercept = 1.5, color = "black") +  geom_vline(xintercept = 3.5, color = "black") 


# D) Discordance Population the Norway aged <60
NO_EU_Baseline60=BaselineOverview %>% filter(Age<60)
NO_EU_Baseline60$Morbidity_ECDC_COVIDcat <- factor(NO_EU_Baseline60$Morbidity_ECDC_COVIDcat, levels = c("High risk", "Moderate risk", "Low risk"))
NO_EU_Baseline60$Morbidity_FHI_COVIDcat <- factor(NO_EU_Baseline60$Morbidity_FHI_COVIDcat, levels = c("High risk", "Moderate risk", "Low risk"))
summary = caret::confusionMatrix(data = NO_EU_Baseline60$Morbidity_ECDC_COVIDcat, reference = NO_EU_Baseline60$Morbidity_FHI_COVIDcat)
Total = NO_EU_Baseline60 %>% summarise(n=n())
NO_EU_Baseline60 = NO_EU_Baseline60 %>%   mutate(N=n()) %>% group_by(Morbidity_FHI_COVIDcat, Morbidity_ECDC_COVIDcat) %>% summarise(n=n())
NO_EU_Baseline60_FHI = NO_EU_Baseline60  %>%  group_by(Morbidity_FHI_COVIDcat) %>% summarise(n = sum(n), Morbidity_ECDC_COVIDcat = "Total")
NO_EU_Baseline60_ECDC = NO_EU_Baseline60  %>%  group_by(Morbidity_ECDC_COVIDcat) %>% summarise(n = sum(n), Morbidity_FHI_COVIDcat = "Total")
NO_EU_Baseline60 = bind_rows(NO_EU_Baseline60, NO_EU_Baseline60_FHI, NO_EU_Baseline60_ECDC)%>% ungroup() %>% add_row(Morbidity_ECDC_COVIDcat = "Total", Morbidity_FHI_COVIDcat = "Total", n = as.numeric(Total$n)) %>% ungroup()
NO_EU_Baseline60 = NO_EU_Baseline60 %>%   mutate(prop = as.numeric(n/Total$n))  %>% 
  mutate(Morbidity_FHI_COVIDcat = factor(Morbidity_FHI_COVIDcat, levels = c("High risk", "Moderate risk", "Low risk", 'Total')), Morbidity_ECDC_COVIDcat = factor(Morbidity_ECDC_COVIDcat, levels = c("Total", "Low risk", "Moderate risk", "High risk"))) %>%
  ggplot(aes(x=Morbidity_FHI_COVIDcat, y=Morbidity_ECDC_COVIDcat, fill=prop, label=paste0(round(prop*100, digits=1), "%"))) +  geom_tile() +
  scale_fill_gradient(low = "white",high = "white") +  
  geom_text(aes(label = ifelse(Morbidity_ECDC_COVIDcat == Morbidity_FHI_COVIDcat, paste0(round(prop*100, digits=1), "%"), ""))) +  # Make diagonal elements bold
  geom_text(aes(label = ifelse(Morbidity_ECDC_COVIDcat != Morbidity_FHI_COVIDcat, paste0(round(prop*100, digits=1), "%"), "")), fontface = "bold") +  # Make diagonal elements bold
  theme_bw() + theme(legend.position="none") +
  scale_x_discrete(labels=c("High", "Moderate", "Low", "Total")) +  scale_y_discrete(labels=c("Total", "Low", "Moderate", "High")) +
  labs(x="National classification", y="European\nclassification", title="Norway, aged <60")+
  theme(plot.caption = element_text(hjust = 0)) +
  labs(caption = "\nDiscordance = 8.4% (95%CI 8.4-8.5%)
Sensitivity: High = 75.0%, Moderate = 4.1%, Low = 98.9%
Specificity: High = 96.4%, Moderate = 99.4% Low = 50.8%
PPV: High = 19.9%, Moderate = 35.0%, Low = 95.5%
NPV: High = 99.7%, Moderate = 92.8%, Low = 81.9%") +
  geom_hline(yintercept = 1.5, color = "black") +  geom_vline(xintercept = 3.5, color = "black") 


# E) Discordance Population the Netherlands aged <65
data <- read.table(text = "RIVM ECDC n
                           hoog	hoog	115011
                           hoog	medium	3097
                           hoog	laag	32442
                           medium	hoog	394953
                           medium	medium	95927
                           medium	laag	582217
                           laag	hoog	103379
                           laag	medium	2201
                           laag	laag	12662679", header = TRUE, stringsAsFactors = FALSE)
long_format <- data.frame(ECDC = rep(data$ECDC, data$n), RIVM = rep(data$RIVM, data$n))
long_format$ECDC <- factor(long_format$ECDC, levels = c("hoog", "medium", "laag"))
long_format$RIVM <- factor(long_format$RIVM, levels = c("hoog", "medium", "laag"))
summary = caret::confusionMatrix(data = long_format$ECDC, reference = long_format$RIVM)
Total = sum(data$n)
NL_EU_Baseline_RIVM = data  %>%  group_by(RIVM) %>% summarise(n = sum(n), ECDC = "Total")
NL_EU_Baseline_ECDC = data  %>%  group_by(ECDC) %>% summarise(n = sum(n), RIVM = "Total")
NL_EU_Baseline65 = bind_rows(data, NL_EU_Baseline_RIVM, NL_EU_Baseline_ECDC) %>% add_row(ECDC = "Total", RIVM = "Total", n = Total)
NL_EU_Baseline65 = NL_EU_Baseline65 %>% 
  mutate(prop=n/Total, RIVM = factor(RIVM, levels = c("hoog", 'medium', 'laag', 'Total')), ECDC = factor(ECDC, levels = c("Total", 'laag', 'medium', "hoog"))) %>%
  ggplot(aes(x=RIVM, y=ECDC, fill=prop, label=paste0(round(prop*100, digits=1), "%"))) +  geom_tile() +
  scale_fill_gradient(low = "white", high = "white") +  
  geom_text(aes(label = ifelse(ECDC == RIVM, paste0(round(prop*100, digits=1), "%"), ""))) +  # Make diagonal elements bold
  geom_text(aes(label = ifelse(ECDC != RIVM, paste0(round(prop*100, digits=1), "%"), "")), fontface = "bold") +  # Make diagonal elements bold
  theme_bw() + theme(legend.position="none") +
  scale_x_discrete(labels=c("High", "Moderate", "Low", "Total")) +  scale_y_discrete(labels=c("Total", "Low", "Moderate", "High")) +
  labs(x="National classification", y="European\nclassification", title="The Netherlands, aged <65")+
  theme(plot.caption = element_text(hjust = 0)) +
  labs(caption = "\nDiscordance = 8.0% (95%CI 8.0-8.0%)
Sensitivity:  High = 76.4%, Moderate = 8.9%, Low= 99.2%
Specificity:  High = 96.4%, Moderate = >99.9%, Low = 50.0%
PPV: High = 18.8%, Moderate = 94.8%, Low = 95.4%
NPV: High = 99.7%, Moderate = 93.0%, Low = 85.2%") +
  geom_hline(yintercept = 1.5, color = "black") +  geom_vline(xintercept = 3.5, color = "black") 

# F) Discordance Population the Norway aged <65
NO_EU_Baseline65=BaselineOverview %>% filter(Age<65)
NO_EU_Baseline65$Morbidity_ECDC_COVIDcat <- factor(NO_EU_Baseline65$Morbidity_ECDC_COVIDcat, levels = c("High risk", "Moderate risk", "Low risk"))
NO_EU_Baseline65$Morbidity_FHI_COVIDcat <- factor(NO_EU_Baseline65$Morbidity_FHI_COVIDcat, levels = c("High risk", "Moderate risk", "Low risk"))
summary = caret::confusionMatrix(data = NO_EU_Baseline65$Morbidity_ECDC_COVIDcat, reference = NO_EU_Baseline65$Morbidity_FHI_COVIDcat)
Total = NO_EU_Baseline65 %>% summarise(n=n())
NO_EU_Baseline65 = NO_EU_Baseline65 %>%   mutate(N=n()) %>% group_by(Morbidity_FHI_COVIDcat, Morbidity_ECDC_COVIDcat) %>% summarise(n=n())
NO_EU_Baseline65_FHI = NO_EU_Baseline65  %>%  group_by(Morbidity_FHI_COVIDcat) %>% summarise(n = sum(n), Morbidity_ECDC_COVIDcat = "Total")
NO_EU_Baseline65_ECDC = NO_EU_Baseline65  %>%  group_by(Morbidity_ECDC_COVIDcat) %>% summarise(n = sum(n), Morbidity_FHI_COVIDcat = "Total")
NO_EU_Baseline65 = bind_rows(NO_EU_Baseline65, NO_EU_Baseline65_FHI, NO_EU_Baseline65_ECDC)%>% ungroup() %>% add_row(Morbidity_ECDC_COVIDcat = "Total", Morbidity_FHI_COVIDcat = "Total", n = as.numeric(Total$n)) %>% ungroup()
NO_EU_Baseline65 = NO_EU_Baseline65 %>%   mutate(prop = as.numeric(n/Total$n))  %>% 
  mutate(Morbidity_FHI_COVIDcat = factor(Morbidity_FHI_COVIDcat, levels = c("High risk", "Moderate risk", "Low risk", 'Total')), Morbidity_ECDC_COVIDcat = factor(Morbidity_ECDC_COVIDcat, levels = c("Total", "Low risk", "Moderate risk", "High risk"))) %>%
  ggplot(aes(x=Morbidity_FHI_COVIDcat, y=Morbidity_ECDC_COVIDcat, fill=prop, label=paste0(round(prop*100, digits=1), "%"))) +  geom_tile() +
  scale_fill_gradient(low = "white",high = "white") +  
  geom_text(aes(label = ifelse(Morbidity_ECDC_COVIDcat == Morbidity_FHI_COVIDcat, paste0(round(prop*100, digits=1), "%"), ""))) +  # Make diagonal elements bold
  geom_text(aes(label = ifelse(Morbidity_ECDC_COVIDcat != Morbidity_FHI_COVIDcat, paste0(round(prop*100, digits=1), "%"), "")), fontface = "bold") +  # Make diagonal elements bold
  theme_bw() + theme(legend.position="none") +
  scale_x_discrete(labels=c("High", "Moderate", "Low", "Total")) +  scale_y_discrete(labels=c("Total", "Low", "Moderate", "High")) +
  labs(x="National classification", y="European\nclassification", title="Norway, aged <65")+
  theme(plot.caption = element_text(hjust = 0)) +
  labs(caption = "\nDiscordance = 9.4% (95%CI 9.4-9.4%)
Sensitivity: High = 78.1%, Moderate = 4.2%, Low = 98.9%
Specificity: High = 96.8%, Moderate = 99.3% Low = 52.7%
PPV: High = 21.7%, Moderate = 37.3%, Low = 95.0%
NPV: High = 99.7%, Moderate = 91.9%, Low = 83.4%") +
  geom_hline(yintercept = 1.5, color = "black") +  geom_vline(xintercept = 3.5, color = "black") 

# Save as figure
cowplot::plot_grid(NL_EU_Baseline, NO_EU_Baseline, NL_EU_Baseline65, NO_EU_Baseline65, NL_EU_Baseline60, NO_EU_Baseline60, 
                               align = "v", ncol=2, labels=c("A", "B","C", "D", "E", "F")) 
ggsave("3. Output/Fig. 3 Discordance by age and national and ECDC classification for NL and Norway.pdf", width = 24.5, height = 24, units = "cm", dpi=320, limitsize = FALSE)



#### Fig 4. Upset plots discordant people ####
popsize =  BaselineOverviewDiseases %>% filter(Morbidity_FHI_COVIDcat != Morbidity_ECDC_COVIDcat) %>% summarise(n=n())
upset_data = BaselineOverviewDiseases %>% filter(Morbidity_FHI_COVIDcat != Morbidity_ECDC_COVIDcat) %>%
  select(contains(c("id", "Morbidity_"))) %>% select(-contains("cat")) 
popsize = BaselineOverviewDiseases %>% filter(Morbidity_FHI_COVIDcat != Morbidity_ECDC_COVIDcat)  %>% summarise(n=n())
upset_data = upset_data %>%  pivot_longer(-id) %>% filter(value==1)%>% filter(grepl("FHI", name)) %>%  disease_names();gc()
upset_data %>% group_by(name) %>% summarise(n(), n()/popsize$n*100) %>% arrange(desc(`n()`))
upset_data = upset_data %>% pivot_wider(names_from = name, values_from = value, values_fill = 0, values_fn = sum) %>%    select(-id); gc()
upset_data = as.data.frame(upset_data)
UpSetR::upset(upset_data, sets = colnames(upset_data), 
              keep.order = F, order.by = "freq",set_size.show = T,
              mainbar.y.label = "Combinations of\nchronic conditions", sets.x.label = "",
              sets.bar.color = c("darkblue", "darkblue", "darkblue", "darkblue", "darkblue", "darkblue", 
                                 "darkorange2", "darkblue", "darkorange2", "darkorange2", "darkorange2", "darkorange2"),
              nintersects = 10, mb.ratio = c(0.3, 0.7))
plot +  grid.text(label = "Norway, National classification", x = 0.06, y = .965, just = "left", gp = gpar(fontsize = 8))
grid.edit('arrange', name = 'arrange2'); UpsetNO_FHI = grid.grab()

upset_data = BaselineOverviewDiseases %>% filter(Morbidity_FHI_COVIDcat != Morbidity_ECDC_COVIDcat) %>%
  select(contains(c("id", "Morbidity_"))) %>% select(-contains("cat"))%>%  pivot_longer(-id) %>% filter(value==1)
upset_data = upset_data %>% filter(grepl("ECDC", name)) %>%  disease_names();gc()
upset_data %>% group_by(name) %>% summarise(n(), n()/popsize$n*100) %>% arrange(desc(`n()`))
upset_data = upset_data %>% pivot_wider(names_from = name, values_from = value, values_fill = 0, values_fn = sum) %>%    select(-id); gc()
upset_data = as.data.frame(upset_data)
UpSetR::upset(upset_data, sets = colnames(upset_data), 
              keep.order = F, order.by = "freq", set_size.show = T,
              mainbar.y.label = "Combinations of\nchronic conditions", sets.x.label = "",
              sets.bar.color = c("darkorange2", "darkorange2", "darkorange2", "darkorange2", "darkblue", "darkblue", "darkblue", 
                                 "darkorange2", "darkblue", "darkblue", "darkblue", "darkorange2", "darkblue", "darkblue", "darkorange2"),
              nintersects = 12, mb.ratio = c(0.3, 0.7))
plot +  grid.text(label = "Norway, European classification", x = 0.06, y = .965, just = "left", gp = gpar(fontsize = 8))
grid.edit('arrange', name = 'arrange2'); UpsetNO_ECDC = grid.grab()

UPSETNO = cowplot::plot_grid(UpsetNO_FHI, UpsetNO_ECDC, ncol=1, align = "v", labels=c("B", "D"))
ggsave("3. Output/Fig 4. Upsetplots Discordant.pdf", UPSETNO, width = 18, height = 20, units = "cm", dpi=320, limitsize = FALSE)

#### Supplementary Figure 1. Upset plots discordant people aged <60 ####
popsize =  BaselineOverviewDiseases %>% filter(Morbidity_FHI_COVIDcat != Morbidity_ECDC_COVIDcat & Age<60) %>% summarise(n=n())
upset_data = BaselineOverviewDiseases %>% filter(Morbidity_FHI_COVIDcat != Morbidity_ECDC_COVIDcat & Age<60) %>%
  select(contains(c("id", "Morbidity_"))) %>% select(-contains("cat"))%>%  pivot_longer(-id) %>% filter(value==1)
upset_data = upset_data %>% filter(grepl("FHI", name)) %>%  disease_names();gc()
upset_data %>% group_by(name) %>% summarise(n(), n()/popsize$n*100) %>% arrange(desc(`n()`))
upset_data = upset_data %>% pivot_wider(names_from = name, values_from = value, values_fill = 0, values_fn = sum) %>%    select(-id); gc()
upset_data = as.data.frame(upset_data)
UpSetR::upset(upset_data, sets = colnames(upset_data), 
              keep.order = F, order.by = "freq",set_size.show = T,
              mainbar.y.label = "Combinations of\nchronic conditions", sets.x.label = "",
              sets.bar.color = c("darkblue", "darkblue", "darkblue", "darkblue", "darkblue", "darkorange2", 
                                 "darkblue", "darkblue", "darkorange2", "darkorange2", "darkorange2", "darkorange2"),
              nintersects = 7, mb.ratio = c(0.3, 0.7))
plot +  grid.text(label = "Norway, National classification", x = 0.06, y = .965, just = "left", gp = gpar(fontsize = 8))
grid.edit('arrange', name = 'arrange2'); UpsetNO_FHI = grid.grab()

upset_data = BaselineOverviewDiseases %>% filter(Morbidity_FHI_COVIDcat != Morbidity_ECDC_COVIDcat & Age<60) %>%
  select(contains(c("id", "Morbidity_"))) %>% select(-contains("cat"))%>%  pivot_longer(-id) %>% filter(value==1)
upset_data = upset_data %>% filter(grepl("ECDC", name)) %>%  disease_names();gc()
upset_data %>% group_by(name) %>% summarise(n(), n()/popsize$n*100) %>% arrange(desc(`n()`))
upset_data = upset_data %>% filter(!grepl("Cancer", name)) %>%
  pivot_wider(names_from = name, values_from = value, values_fill = 0, values_fn = sum) %>%    select(-id); gc()
upset_data = as.data.frame(upset_data)
UpSetR::upset(upset_data, sets = colnames(upset_data), 
              keep.order = F, order.by = "freq", set_size.show = T,
              mainbar.y.label = "Combinations of\nchronic conditions", sets.x.label = "",
              sets.bar.color = c("darkorange2", "darkorange2", "darkorange2", "darkorange2", "darkblue", "darkblue", "darkorange2", 
                                 "darkblue", "darkblue", "darkblue", "darkblue", "darkorange2", "darkblue", "darkblue"),
              nintersects = 11, mb.ratio = c(0.3, 0.7))
plot +  grid.text(label = "Norway, European classification", x = 0.06, y = .965, just = "left", gp = gpar(fontsize = 8))
grid.edit('arrange', name = 'arrange2'); UpsetNO_ECDC = grid.grab()

UPSETNO = cowplot::plot_grid(UpsetNO_FHI, UpsetNO_ECDC, ncol=1, align = "v", labels=c("B", "D"))
ggsave("3. Output/Fig 5. Upsetplots Discordant_60.pdf", UPSETNO, width = 18, height = 20, units = "cm", dpi=320, limitsize = FALSE)


#### Supplementary Figure 2. Upset plots discordant people aged <65 ####
popsize =  BaselineOverviewDiseases %>% filter(Morbidity_FHI_COVIDcat != Morbidity_ECDC_COVIDcat & Age<65) %>% summarise(n=n())
upset_data = BaselineOverviewDiseases %>% filter(Morbidity_FHI_COVIDcat != Morbidity_ECDC_COVIDcat & Age<65) %>%
  select(contains(c("id", "Morbidity_"))) %>% select(-contains("cat"))%>%  pivot_longer(-id) %>% filter(value==1)
upset_data = upset_data %>% filter(grepl("FHI", name)) %>%  disease_names();gc()
upset_data %>% group_by(name) %>% summarise(n(), n()/popsize$n*100) %>% arrange(desc(`n()`))
upset_data = upset_data %>% pivot_wider(names_from = name, values_from = value, values_fill = 0, values_fn = sum) %>%    select(-id); gc()
upset_data = as.data.frame(upset_data)
UpSetR::upset(upset_data, sets = colnames(upset_data), 
              keep.order = F, order.by = "freq",set_size.show = T,
              mainbar.y.label = "Combinations of\nchronic conditions", sets.x.label = "",
              sets.bar.color = c("darkblue", "darkblue", "darkblue", "darkblue", "darkblue", "darkorange2", 
                                 "darkblue", "darkblue", "darkorange2", "darkorange2", "darkorange2", "darkorange2"),
              nintersects = 9, mb.ratio = c(0.3, 0.7))
plot +  grid.text(label = "Norway, National classification", x = 0.06, y = .965, just = "left", gp = gpar(fontsize = 8))
grid.edit('arrange', name = 'arrange2'); UpsetNO_FHI = grid.grab()

upset_data = BaselineOverviewDiseases %>% filter(Morbidity_FHI_COVIDcat != Morbidity_ECDC_COVIDcat & Age<65) %>%
  select(contains(c("id", "Morbidity_"))) %>% select(-contains("cat"))%>%  pivot_longer(-id) %>% filter(value==1)
upset_data = upset_data %>% filter(grepl("ECDC", name)) %>%  disease_names();gc()
upset_data %>% group_by(name) %>% summarise(n(), n()/popsize$n*100) %>% arrange(desc(`n()`))
upset_data = upset_data %>% filter(!grepl("Cancer", name)) %>%
  pivot_wider(names_from = name, values_from = value, values_fill = 0, values_fn = sum) %>%    select(-id); gc()
upset_data = as.data.frame(upset_data)
UpSetR::upset(upset_data, sets = colnames(upset_data), 
              keep.order = F, order.by = "freq", set_size.show = T,
              mainbar.y.label = "Combinations of\nchronic conditions", sets.x.label = "",
              sets.bar.color = c("darkorange2", "darkorange2", "darkorange2", "darkorange2", "darkblue", "darkblue", "darkorange2", 
                                 "darkblue", "darkblue", "darkblue", "darkblue", "darkorange2", "darkblue", "darkblue"),
              nintersects = 12, mb.ratio = c(0.3, 0.7))
plot +  grid.text(label = "Norway, European classification", x = 0.06, y = .965, just = "left", gp = gpar(fontsize = 8))
grid.edit('arrange', name = 'arrange2'); UpsetNO_ECDC = grid.grab()

UPSETNO = cowplot::plot_grid(UpsetNO_FHI, UpsetNO_ECDC, ncol=1, align = "v", labels=c("B", "D"));UPSETNO
ggsave("3. Output/Fig 6. Upsetplots Discordant_65.pdf", UPSETNO, width = 18, height = 20, units = "cm", dpi=320, limitsize = FALSE)






########  END  ########