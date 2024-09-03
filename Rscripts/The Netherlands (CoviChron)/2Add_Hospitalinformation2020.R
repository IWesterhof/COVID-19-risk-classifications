## Goal: Add hospitalization data to NL population at 01-01-2020
## Version/date: V1.0/13-11-2023
## Authos: A.R. de Boer
## R-version: 4.2.3

#####################
### Load packages ###
#####################
library(data.table)
library(haven)

########################
### Load base cohort ###
########################

RIN <- fread("H:/Data/RINBevolking2020.csv")
setkey(RIN, RINPERSOON)

###########################################################
### Read hospitalinformation from people in base cohort ###
###########################################################

files <- seq(from = 2013, to = 2021)

# Start with basis files.

for(i in 1:length(files)){
  if (files[i] == "1995") {
    setwd(paste0("G:/Externesurvey/LMR_BASIS/1995/geconverteerde data"))
    assign(paste0("lmrbasis_1995"), fread(paste0("070928 REOS-bestand LMRbasis 1995V2_csv.csv"),
                                                 select = c("rin", "deregjr", "deinstn", "deopnnr", "opndat")))
    setkey(lmrbasis_1995, rin)
    lmrbasis_1995 <- lmrbasis_1995[RIN]
  } else if (files[i] == "1996") {
    setwd(paste0("G:/Externesurvey/LMR_BASIS/1996/geconverteerde data"))
    assign(paste0("lmrbasis_1996"), fread(paste0("070928 REOS-bestand LMRbasis 1996V2_csv.csv"),
                                                 select = c("rin", "deregjr", "deinstn", "deopnnr", "opndat")))
    setkey(lmrbasis_1996, rin)
    lmrbasis_1996 <- lmrbasis_1996[RIN]
  } else if (files[i] == "1997") {
    setwd(paste0("G:/Externesurvey/LMR_BASIS/1997/geconverteerde data"))
    assign(paste0("lmrbasis_1997"), fread(paste0("070928 REOS-bestand LMRbasis 1997v2_csv.csv"),
                                                 select = c("rin", "deregjr", "deinstn", "deopnnr", "opndat")))
    setkey(lmrbasis_1997, rin)
    lmrbasis_1997 <- lmrbasis_1997[RIN]
  } else if (files[i] == "1998") {
    setwd(paste0("G:/Externesurvey/LMR_BASIS/1998/geconverteerde data"))
    assign(paste0("lmrbasis_1998"), fread(paste0("070928 REOS-bestand LMRbasis 1998v2_csv.csv"),
                                                 select = c("rin", "deregjr", "deinstn", "deopnnr", "opndat")))
    setkey(lmrbasis_1998, rin)
    lmrbasis_1998 <- lmrbasis_1998[RIN]
  } else if (files[i] == "1999") {
    setwd(paste0("G:/Externesurvey/LMR_BASIS/1999/geconverteerde data"))
    assign(paste0("lmrbasis_1999"), fread(paste0("070928 REOS-bestand LMRbasis 1999v2_csv.csv"),
                                                 select = c("rin", "deregjr", "deinstn", "deopnnr", "opndat")))
    setkey(lmrbasis_1999, rin)
    lmrbasis_1999 <- lmrbasis_1999[RIN]
  } else if (files[i] == "2000") {
    setwd(paste0("G:/Externesurvey/LMR_BASIS/2000/geconverteerde data"))
    assign(paste0("lmrbasis_2000"), fread(paste0("070928 REOS-bestand LMRbasis 2000v2_csv.csv"),
                                                 select = c("rin", "deregjr", "deinstn", "deopnnr", "opndat")))
    setkey(lmrbasis_2000, rin)
    lmrbasis_2000 <- lmrbasis_2000[RIN]
  } else if (files[i] == "2001") {
    setwd(paste0("G:/Externesurvey/LMR_BASIS/2001/geconverteerde data"))
    assign(paste0("lmrbasis_2001"), fread(paste0("070928 REOS-bestand LMRbasis 2001v2_csv.csv"),
                                                 select = c("rin", "deregjr", "deinstn", "deopnnr", "opndat")))
    setkey(lmrbasis_2001, rin)
    lmrbasis_2001 <- lmrbasis_2001[RIN]
  } else if (files[i] == "2002") {
    setwd(paste0("G:/Externesurvey/LMR_BASIS/2002/geconverteerde data"))
    assign(paste0("lmrbasis_2002"), fread(paste0("070928 REOS-bestand LMRbasis 2002v2_csv.csv"),
                                                 select = c("rin", "deregjr", "deinstn", "deopnnr", "opndat")))
    setkey(lmrbasis_2002, rin)
    lmrbasis_2002 <- lmrbasis_2002[RIN]
  } else if (files[i] == "2003") {
    setwd(paste0("G:/Externesurvey/LMR_BASIS/2003/geconverteerde data"))
    assign(paste0("lmrbasis_2003"), fread(paste0("070928 REOS-bestand LMRbasis 2003v2_csv.csv"),
                                                 select = c("rin", "deregjr", "deinstn", "deopnnr", "opndat")))
    setkey(lmrbasis_2003, rin)
    lmrbasis_2003 <- lmrbasis_2003[RIN]
  } else if (files[i] == "2004") {
    setwd(paste0("G:/Externesurvey/LMR_BASIS/2004/geconverteerde data"))
    assign(paste0("lmrbasis_2004"), fread(paste0("070928 REOS-bestand LMRbasis 2004v2_csv.csv"),
                                                 select = c("rin", "deregjr", "deinstn", "deopnnr", "opndat")))
    setkey(lmrbasis_2004, rin)
    lmrbasis_2004 <- lmrbasis_2004[RIN]
  } else if (files[i] == "2005") {
    setwd(paste0("G:/Externesurvey/LMR_BASIS/2005/geconverteerde data"))
    assign(paste0("lmrbasis_2005"), fread(paste0("071024 REOS-bestand LMRbasis 2005v3_csv.csv"),
                                                 select = c("rin", "deregjr", "deinstn", "deopnnr", "opndat")))
    setkey(lmrbasis_2005, rin)
    lmrbasis_2005 <- lmrbasis_2005[RIN]
  } else if (files[i] == "2006") {
    setwd(paste0("G:/Externesurvey/LMR_BASIS/2006/geconverteerde data"))
    assign(paste0("lmrbasis_2006"), fread(paste0("090617 LMRbasis 2006V1_csv.csv"),
                                                 select = c("rin", "deregjr", "deinstn", "deopnnr", "opndat")))
    setkey(lmrbasis_2006, rin)
    lmrbasis_2006 <- lmrbasis_2006[RIN]
  } else if (files[i] == "2007") {
    setwd(paste0("G:/Externesurvey/LMR_BASIS/2007/geconverteerde data"))
    assign(paste0("lmrbasis_2007"), fread(paste0("090617 LMRbasis 2007V1_csv.csv"),
                                                 select = c("rin", "deregjr", "deinstn", "deopnnr", "opndat")))
    setkey(lmrbasis_2007, rin)
    lmrbasis_2007 <- lmrbasis_2007[RIN]
  } else if (files[i] == "2008") {
    setwd(paste0("G:/Externesurvey/LMR_BASIS/2008/geconverteerde data"))
    assign(paste0("lmrbasis_2008"), fread(paste0("LMRbasis 2008V2_csv.csv"),
                                                 select = c("rin", "deregjr", "deinstn", "deopnnr", "opndat")))
    setkey(lmrbasis_2008, rinpersoon)
    lmrbasis_2008 <- lmrbasis_2008[RIN]
  } else if (files[i] == "2009") {
    setwd(paste0("G:/Externesurvey/LMR_BASIS/2009/geconverteerde data"))
    assign(paste0("lmrbasis_2009"), fread(paste0("LMRbasis 2009V2_csv.csv"),
                                                 select = c("rin", "deregjr", "deinstn", "deopnnr", "opndat")))
    setkey(lmrbasis_2009, rinpersoon)
    lmrbasis_2009 <- lmrbasis_2009[RIN]
  } else if (files[i] == "2010") {
    setwd(paste0("G:/Externesurvey/LMR_BASIS/2010/geconverteerde data"))
    assign(paste0("lmrbasis_2010"), fread(paste0("LMRbasis 2010V2_csv.csv"),
                                                 select = c("rin", "deregjr", "deinstn", "deopnnr", "opndat")))
    setkey(lmrbasis_2010, rinpersoon)
    lmrbasis_2010 <- lmrbasis_2010[RIN]
  } else if (files[i] == "2011") {
    setwd(paste0("G:/Externesurvey/LMR_BASIS/2011/geconverteerde data"))
    assign(paste0("lmrbasis_2011"), fread(paste0("LMRbasis 2011V3_csv.csv"),
                                                 select = c("rin", "deregjr", "deinstn", "deopnnr", "opndat")))
    setkey(lmrbasis_2011, rinpersoon)
    lmrbasis_2011 <- lmrbasis_2011[RIN]
  } else if (files[i] == "2012") {
    setwd(paste0("G:/Externesurvey/LMR_BASIS/2012/geconverteerde data"))
    assign(paste0("lmrbasis_2012"), fread(paste0("LMRbasis 2012V2_csv.csv"),
                                                 select = c("rin", "deregjr", "deinstn", "deopnnr", "opndat")))
    setkey(lmrbasis_2012, rinpersoon)
    lmrbasis_2012 <- lmrbasis_2012[RIN]
  } else if (files[i] == "2013") {
    setwd(paste0("G:/GezondheidWelzijn/LBZBASISTAB/2013/geconverteerde data"))
    assign(paste0("lbzbasis_2013"), fread(paste0("LBZbasis2013TABV2_csv.csv"),
                                                 select = c("RINPERSOON", "LBZIdopname", "LBZOpnamedatum")))
    setkey(lbzbasis_2013, RINPERSOON)
    lbzbasis_2013 <- lbzbasis_2013[RIN]
  } else if (files[i] == "2014") {
    setwd(paste0("G:/GezondheidWelzijn/LBZBASISTAB/2014/geconverteerde data"))
    assign(paste0("lbzbasis_2014"), fread(paste0("LBZbasis2014TABV2_csv.csv"),
                                                 select = c("RINPERSOON", "LBZIdopname", "LBZOpnamedatum")))
    setkey(lbzbasis_2014, RINPERSOON)
    lbzbasis_2014 <- lbzbasis_2014[RIN]
  } else if (files[i] == "2015") {
    setwd(paste0("G:/GezondheidWelzijn/LBZBASISTAB/2015/geconverteerde data"))
    assign(paste0("lbzbasis_2015"), fread(paste0("LBZbasis2015TABV2_csv.csv"),
                                                 select = c("RINPERSOON", "LBZIdopname", "LBZOpnamedatum")))
    setkey(lbzbasis_2015, RINPERSOON)
    lbzbasis_2015 <- lbzbasis_2015[RIN]
  } else if (files[i] == "2016") {
    setwd(paste0("G:/GezondheidWelzijn/LBZBASISTAB/2016/geconverteerde data"))
    assign(paste0("lbzbasis_2016"), fread(paste0("LBZbasis2016TABV1_csv.csv"),
                                                 select = c("RINPERSOON", "LBZIdopname", "LBZOpnamedatum")))
    setkey(lbzbasis_2016, RINPERSOON)
    lbzbasis_2016 <- lbzbasis_2016[RIN]
  } else if (files[i] == "2017") {
    setwd(paste0("G:/GezondheidWelzijn/LBZBASISTAB/2017/geconverteerde data"))
    assign(paste0("lbzbasis_2017"), fread(paste0("LBZbasis2017TABV1_csv.csv"),
                                                 select = c("RINPERSOON", "LBZIdopname", "LBZOpnamedatum")))
    setkey(lbzbasis_2017, RINPERSOON)
    lbzbasis_2017 <- lbzbasis_2017[RIN]
  } else if (files[i] == "2018") {
    setwd(paste0("G:/GezondheidWelzijn/LBZBASISTAB/2018/geconverteerde data"))
    assign(paste0("lbzbasis_2018"), fread(paste0("LBZBASIS2018TABV2_csv.csv"),
                                                 select = c("RINPERSOON", "LBZIdopname", "LBZOpnamedatum")))
    setkey(lbzbasis_2018, RINPERSOON)
    lbzbasis_2018 <- lbzbasis_2018[RIN]
  } else if (files[i] == "2019") {
    setwd(paste0("G:/GezondheidWelzijn/LBZBASISTAB/2019"))
    assign(paste0("lbzbasis_2019"), fread(paste0("LBZBASIS2019TABV1_csv.csv"),
                                                 select = c("RINPERSOON", "LBZIdopname", "LBZOpnamedatum")))
    setkey(lbzbasis_2019, RINPERSOON)
    lbzbasis_2019 <- lbzbasis_2019[RIN]
  } else if (files[i] == "2020") {
    setwd(paste0("G:/GezondheidWelzijn/LBZBASISTAB/2020/geconverteerde data"))
    assign(paste0("lbzbasis_2020"), fread(paste0("LBZBASIS2020TABV1_csv.csv"),
                                                 select = c("RINPERSOON", "LBZIdopname", "LBZOpnamedatum")))
    setkey(lbzbasis_2020, RINPERSOON)
    lbzbasis_2020 <- lbzbasis_2020[RIN]
  } else if (files[i] == "2021") {
    setwd(paste0("G:/GezondheidWelzijn/LBZBASISTAB/2021/geconverteerde data"))
    assign(paste0("lbzbasis_2021"), fread(paste0("LBZbasis2021TABV1_csv.csv"),
                                                 select = c("RINPERSOON", "LBZIdopname", "LBZOpnamedatum")))
    setkey(lbzbasis_2021, RINPERSOON)
    lbzbasis_2021 <- lbzbasis_2021[RIN]
  }
 }

# # Make column names similar
# lmrb_list <- mget(ls(pattern = "lmrbasis_"))
# lmrb_vars <- c("rin", "deregjr", "deinstn", "deopnnr", "opndat", "rinpersoon")
# lmrb_list <- lapply(lmrb_list, funciton(x) {colnames(x) <-
#   c("rinpersoon", "deregjr", "deinstn", "deopnnr", "opndat")})

# Bind all rows together.
# lmrbasis <- do.call(rbind, lmrb_list)

# Remove individual files.
# rm(list = apropos("lmrbasis_"))

# Repeat for lbzbasis_.
# Make column names similar
lbzb_list <- mget(ls(pattern = "lbzbasis_"))
# lbzb_vars <- c("RINPERSOON", "LBZIdopname", "LBZOpnamedatum")
# lmrb_list <- lapply(lmrb_list, funciton(x) {colnames(x) <-
#   c("rinpersoon", "deopnnr", "opndat")})

# Bind all rows together.
lbzbasis <- do.call(rbind, lbzb_list)

# Remove individual files.
rm(list = apropos("lbzbasis_"))
rm(lbzb_list)

# Remove empty rows (from people without any hospitalization in their history)
lbzbasis2 <- lbzbasis[is.na(LBZIdopname)==F,]

rm(lbzbasis)

# Second, diagnosis files.

for(i in 1:length(files)){
  if (files[i] == "1995") {
    setwd(paste0("G:/Externesurvey/LMR_DIAGN/1995/geconverteerde data"))
    assign(paste0("lmrdiagn_1995"), fread(paste0("060913 REOS-bestand LMRdiagn 1995V2_csv.csv"),
                                                 select = c("rin", "deregjr", "deinstn", "deopnnr", "opndat")))
  } else if (files[i] == "1996") {
    setwd(paste0("G:/Externesurvey/LMR_DIAGN/1996/geconverteerde data"))
    assign(paste0("lmrdiagn_1996"), fread(paste0("060913 REOS-bestand LMRdiagn 1996V2_csv.csv"),
                                                 select = c("rin", "deregjr", "deinstn", "deopnnr", "opndat")))
  } else if (files[i] == "1997") {
    setwd(paste0("G:/Externesurvey/LMR_DIAGN/1997/geconverteerde data"))
    assign(paste0("lmrdiagn_1997"), fread(paste0("060913 REOS-bestand LMRdiagn 1997V2_csv.csv"),
                                                 select = c("rin", "deregjr", "deinstn", "deopnnr", "opndat")))
  } else if (files[i] == "1998") {
    setwd(paste0("G:/Externesurvey/LMR_DIAGN/1998/geconverteerde data"))
    assign(paste0("lmrdiagn_1998"), fread(paste0("060913 REOS-bestand LMRdiagn 1998V2_csv.csv"),
                                                 select = c("rin", "deregjr", "deinstn", "deopnnr", "opndat")))
  } else if (files[i] == "1999") {
    setwd(paste0("G:/Externesurvey/LMR_DIAGN/1999/geconverteerde data"))
    assign(paste0("lmrdiagn_1999"), fread(paste0("060913 REOS-bestand LMRdiagn 1999V2_csv.csv"),
                                                 select = c("rin", "deregjr", "deinstn", "deopnnr", "opndat")))
  } else if (files[i] == "2000") {
    setwd(paste0("G:/Externesurvey/LMR_DIAGN/2000/geconverteerde data"))
    assign(paste0("lmrdiagn_2000"), fread(paste0("060913 REOS-bestand LMRdiagn 2000V2_csv.csv"),
                                                 select = c("rin", "deregjr", "deinstn", "deopnnr", "opndat")))
  } else if (files[i] == "2001") {
    setwd(paste0("G:/Externesurvey/LMR_DIAGN/2001/geconverteerde data"))
    assign(paste0("lmrdiagn_2001"), fread(paste0("060913 REOS-bestand LMRdiagn 2001V2_csv.csv"),
                                                 select = c("rin", "deregjr", "deinstn", "deopnnr", "opndat")))
  } else if (files[i] == "2002") {
    setwd(paste0("G:/Externesurvey/LMR_DIAGN/2002/geconverteerde data"))
    assign(paste0("lmrdiagn_2002"), fread(paste0("060912 REOS-bestand LMRdiagn 2002V2_csv.csv"),
                                                 select = c("rin", "deregjr", "deinstn", "deopnnr", "opndat")))
  } else if (files[i] == "2003") {
    setwd(paste0("G:/Externesurvey/LMR_DIAGN/2003/geconverteerde data"))
    assign(paste0("lmrdiagn_2003"), fread(paste0("060912 REOS-bestand LMRdiagn 2003V2_csv.csv"),
                                                 select = c("rin", "deregjr", "deinstn", "deopnnr", "opndat")))
  } else if (files[i] == "2004") {
    setwd(paste0("G:/Externesurvey/LMR_DIAGN/2004/geconverteerde data"))
    assign(paste0("lmrdiagn_2004"), fread(paste0("060911 REOS-bestand LMRdiagn 2004V2_csv.csv"),
                                                 select = c("rin", "deregjr", "deinstn", "deopnnr", "opndat")))
  } else if (files[i] == "2005") {
    setwd(paste0("G:/Externesurvey/LMR_DIAGN/2005/geconverteerde data"))
    assign(paste0("lmrdiagn_2005"), fread(paste0("061206 REOS-bestand LMRdiagn 2005V3_csv.csv"),
                                                 select = c("rin", "deregjr", "deinstn", "deopnnr", "opndat")))
  } else if (files[i] == "2006") {
    setwd(paste0("G:/Externesurvey/LMR_DIAGN/2006/geconverteerde data"))
    assign(paste0("lmrdiagn_2006"), fread(paste0("090617 LMRdiagn 2006V1_csv.csv"),
                                                 select = c("rin", "deregjr", "deinstn", "deopnnr", "opndat")))
  } else if (files[i] == "2007") {
    setwd(paste0("G:/Externesurvey/LMR_DIAGN/2007/geconverteerde data"))
    assign(paste0("lmrdiagn_2007"), fread(paste0("090617 LMRdiagn 2007V1_csv.csv"),
                                                 select = c("rin", "deregjr", "deinstn", "deopnnr", "opndat")))
  } else if (files[i] == "2008") {
    setwd(paste0("G:/Externesurvey/LMR_DIAGN/2008/geconverteerde data"))
    assign(paste0("lmrdiagn_2008"), fread(paste0("100504 LMRdiagn 2008V1_csv.csv"),
                                                 select = c("rin", "deregjr", "deinstn", "deopnnr", "opndat")))
  } else if (files[i] == "2009") {
    setwd(paste0("G:/Externesurvey/LMR_DIAGN/2009/geconverteerde data"))
    assign(paste0("lmrdiagn_2009"), fread(paste0("110711 LMRdiagn 2009V1_csv.csv"),
                                                 select = c("rin", "deregjr", "deinstn", "deopnnr", "opndat")))
  } else if (files[i] == "2010") {
    setwd(paste0("G:/Externesurvey/LMR_DIAGN/2010/geconverteerde data"))
    assign(paste0("lmrdiagn_2010"), fread(paste0("120620 LMRdiagn 2010V1_csv.csv"),
                                                 select = c("rin", "deregjr", "deinstn", "deopnnr", "opndat")))
  } else if (files[i] == "2011") {
    setwd(paste0("G:/Externesurvey/LMR_DIAGN/2011/geconverteerde data"))
    assign(paste0("lmrdiagn_2011"), fread(paste0("131211 LMRdiagn 2011V1_csv.csv"),
                                                 select = c("rin", "deregjr", "deinstn", "deopnnr", "opndat")))
  } else if (files[i] == "2012") {
    setwd(paste0("G:/Externesurvey/LMR_DIAGN/2012/geconverteerde data"))
    assign(paste0("lmrdiagn_2012"), fread(paste0("LMRdiagn 2012V1_csv.csv"),
                                                 select = c("rin", "deregjr", "deinstn", "deopnnr", "opndat")))
  } else if (files[i] == "2013") {
    setwd(paste0("G:/GezondheidWelzijn/LBZDIAGNOSENTAB/2013/geconverteerde data"))
    assign(paste0("lbzdiagn_2013"), fread(paste0("LBZDIAGNOSEN2013TABV2_csv.csv"),
                                                 select = c("RINPERSOON", "LBZIdopname", "LBZIcd10diag")))
  } else if (files[i] == "2014") {
    setwd(paste0("G:/GezondheidWelzijn/LBZDIAGNOSENTAB/2014/geconverteerde data"))
    assign(paste0("lbzdiagn_2014"), fread(paste0("LBZDIAGNOSEN2014TABV2_csv.csv"),
                                                 select = c("RINPERSOON", "LBZIdopname", "LBZIcd10diag")))
  } else if (files[i] == "2015") {
    setwd(paste0("G:/GezondheidWelzijn/LBZDIAGNOSENTAB/2015/geconverteerde data"))
    assign(paste0("lbzdiagn_2015"), fread(paste0("LBZDIAGNOSEN2015TABV2_csv.csv"),
                                                 select = c("RINPERSOON", "LBZIdopname", "LBZIcd10diag")))
  } else if (files[i] == "2016") {
    setwd(paste0("G:/GezondheidWelzijn/LBZDIAGNOSENTAB/2016/geconverteerde data"))
    assign(paste0("lbzdiagn_2016"), fread(paste0("LBZDIAGNOSEN2016TABV1_csv.csv"),
                                                 select = c("RINPERSOON", "LBZIdopname", "LBZIcd10diag")))
  } else if (files[i] == "2017") {
    setwd(paste0("G:/GezondheidWelzijn/LBZDIAGNOSENTAB/2017/geconverteerde data"))
    assign(paste0("lbzdiagn_2017"), fread(paste0("LBZdiagnosen2017TABV1_csv.csv"),
                                                 select = c("RINPERSOON", "LBZIdopname", "LBZIcd10diag")))
  } else if (files[i] == "2018") {
    setwd(paste0("G:/GezondheidWelzijn/LBZDIAGNOSENTAB/2018/geconverteerde data"))
    assign(paste0("lbzdiagn_2018"), fread(paste0("LBZDIAGNOSEN2018TABV1_csv.csv"),
                                                 select = c("RINPERSOON", "LBZIdopname", "LBZIcd10diag")))
  } else if (files[i] == "2019") {
    setwd(paste0("G:/GezondheidWelzijn/LBZDIAGNOSENTAB/2019/geconverteerde data"))
    assign(paste0("lbzdiagn_2019"), fread(paste0("LBZDIAGNOSEN2019TABV1_csv.csv"),
                                                 select = c("RINPERSOON", "LBZIdopname", "LBZIcd10diag")))
  } else if (files[i] == "2020") {
    setwd(paste0("G:/GezondheidWelzijn/LBZDIAGNOSENTAB/2020/geconverteerde data"))
    assign(paste0("lbzdiagn_2020"), fread(paste0("LBZDIAGNOSEN2020TABV1_csv.csv"),
                                                 select = c("RINPERSOON", "LBZIdopname", "LBZIcd10diag")))
  } else if (files[i] == "2021") {
    setwd(paste0("G:/GezondheidWelzijn/LBZDIAGNOSENTAB/2021/geconverteerde data"))
    assign(paste0("lbzdiagn_2021"), fread(paste0("LBZdiagnosen2021TABV1_csv.csv"),
                                                 select = c("RINPERSOON", "LBZIdopname", "LBZIcd10diag")))
  }
}

# Make column names similar
lbzd_list <- mget(ls(pattern = "lbzdiagn_"))
# lbzb_vars <- c("RINPERSOON", "LBZIdopname", "LBZOpnamedatum")
# lmrb_list <- lapply(lmrb_list, funciton(x) {colnames(x) <-
#   c("rinpersoon", "deopnnr", "opndat")})

# Bind all rows together.
lbzdiagn <- do.call(rbind, lbzd_list)

# Remove individual files.
rm(list = apropos("lbzdiagn_"))
rm(lbzd_list)
rm(RIN)

# Convert opnamedatum to IDate format
lbzbasis2[, opndat := as.IDate(as.character(LBZOpnamedatum), format = "%Y%m%d" )]
lbzbasis2[, LBZOpnamedatum := NULL]

# Prepare for merge.
setkey(lbzbasis2, RINPERSOON, LBZIdopname)
setkey(lbzdiagn, RINPERSOON, LBZIdopname)

# Merge.
hosp <- lbzdiagn[lbzbasis2]

# Save datafile as .csv.
fwrite(hosp, file = "H:/Data/MedicalHistory2020.csv")
