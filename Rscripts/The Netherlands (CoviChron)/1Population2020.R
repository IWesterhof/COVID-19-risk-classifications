## Goal: Select individuals living in the Netherlands on 01-01-2020.
## Version/date: V1.0/13-11-2023
## Authos: A.R. de Boer
## R-version: 4.2.3

#####################
### Load packages ###
#####################
library(data.table)
library(haven)
library(eeptools)

############################
### Define set variables ###
############################

# Select only individuals that are living in the Netherlands and registered in the GBA/BRP on 01-01-2020.
indexdate <- as.IDate("2020-01-01")

######################################
### Dutch population on 2020-01-01 ###
######################################
# Load GBAadresobjectbus (to start with the number of people living in the Netherlands at a certain moment)
adres2020 <- read_sav("G:/Bevolking/GBAADRESOBJECTBUS/GBAADRESOBJECT2020BUSV1.sav")
adres2020 <- zap_formats(zap_labels(zap_label(zap_widths(adres2020))))
setDT(adres2020)
adres2020[, startadres := as.IDate(GBADATUMAANVANGADRESHOUDING, format = "%Y%m%d" )]
adres2020[, eindadres := as.IDate(GBADATUMEINDEADRESHOUDING, format = "%Y%m%d" )]
# Keep only those living in the Netherlands during the indexdate.
adresindex <- adres2020[startadres <= indexdate & eindadres >= indexdate,]
adresindex[, c("GBADATUMAANVANGADRESHOUDING", 
               "GBADATUMEINDEADRESHOUDING", "SOORTOBJECTNUMMER", "RINOBJECTNUMMER", 
               "startadres", "eindadres") := NULL]
rm(adres2020)
setkey(adresindex, "RINPERSOONS", "RINPERSOON")

# Check how many unique individuals.
length(unique(adresindex$RINPERSOON))

# Load GBA (date of birth)
gba2020 <- read_sav("G:/Bevolking/GBAPERSOONKTAB/GBAPERSOONKTAB2020V1.sav")
gba2020 <- zap_formats(zap_labels(zap_label(zap_widths(gba2020))))
setDT(gba2020)
gba2020[, datebirth1 := paste0(GBAGEBOORTEJAAR, GBAGEBOORTEMAAND, GBAGEBOORTEDAG)]
gba2020[, datebirth := as.IDate(datebirth1, format = "%Y%m%d" )]
gba2020[, c("GBAGESLACHT", "GBAGEBOORTEJAAR", "GBAGEBOORTEMAAND", "GBAGEBOORTEDAG", "datebirth1") := NULL]
setkey(gba2020, "RINPERSOONS", "RINPERSOON")

# Link data to adresindex.
pop2020 <- gba2020[adresindex]

rm(gba2020, adresindex)

# Remove all individuals that were born after the indexdate.
pop2020_clean <- pop2020[datebirth < indexdate,]
setkey(pop2020_clean, "RINPERSOONS", "RINPERSOON")

# Check how many unique individuals.
length(unique(pop2020_clean$RINPERSOON)) 

# Remove unnecessary data.
rm(pop2020)

# Load date of death (load most recent year, due to a possible delay in death registration)
death2022 <- read_sav("G:/Bevolking/GBAOVERLIJDENTAB/2022/GBAOVERLIJDEN2022TABV1.sav")
death2022 <- zap_formats(zap_labels(zap_label(zap_widths(death2022))))
setDT(death2022)
death2022[, datedeath := as.IDate(GBADatumOverlijden, format = "%Y%m%d" )]
death2022[, c("GBADatumOverlijden") := NULL]
setkey(death2022, "RINPERSOONS", "RINPERSOON")

# Merge gba and death.
pop2020_death <- death2022[pop2020_clean]

# Remove unnecessary data.
rm(death2022, pop2020_clean)

# Remove all individuals that dead before the indexdate.
pop2020_death_clean <- pop2020_death[is.na(datedeath) == T | datedeath > indexdate,]
setkey(pop2020_death_clean, "RINPERSOONS", "RINPERSOON")

# Check how many unique individuals.
length(unique(pop2020_death_clean$RINPERSOON))

rm(pop2020_death)

# Create age-categories.
# First, calculate age.
pop2020_death_clean[, age := floor(age_calc(datebirth, indexdate, units = "years"))]
# Second, create age categories.
breaks <- c(0,10,20,30,40,50,60,70,80,Inf)
pop2020_death_clean[, agecat := cut(age, breaks=breaks, right = F)]

# Save datafile as .csv.
fwrite(pop2020_death_clean, file = "H:/Data/Bevolking2020.csv")

# SAve only Rin-numbers as .csv.
rins <- pop2020_death_clean[, c("RINPERSOON")]
fwrite(rins, file = "H:/Data/RINBevolking2020.csv")
