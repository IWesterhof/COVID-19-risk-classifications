# COVID-19-risk-classifications

## DOI for this GitHub repository
doi: http://doi.org/xxx/zenodo.xxx

## PROJECT OVERVIEW
CoviCron (Covid-19 and Chronic illness interactions in transmission dynamics) aims to project SARS-CoV-2 transmission dynamics and evaluate the impact of public health interventions in the population of the Netherlands stratified by age and risk due to chronic conditions. We are extending an existing age-stratified mathematical model previously validated against real COVID-19 data to include stratification by risk due to chronic conditions.

VERDI (SARS-coV2 variants Evaluation in pRegnancy and paeDIatrics cohorts) project aims to generate improved evidence on the epidemiology, outcomes, prevention and treatment of variants of SARS-CoV-2 amongst children and pregnant women as a global response to the pandemic, involving cohort studies from diverse geographic and economic settings. 

## FUNDING
GR, IW, AdB, AL, OB, JB, FP, MSvdL, MK, and JvdW were supported by ZonMw project (10430362220002). GR, IW, AL, and PB were supported by the VERDI project (101045989), funded by the European Union. Views and opinions expressed are however those of the author(s) only and do not necessarily reflect those of the European Union or the Health and Digital Executive Agency. Neither the European Union nor the granting authority can be held responsible for them. IW was supported by the scholarship program iAPOGEE  “International Alliance for PharmacoGenetic Epidemiology Excellence” at the University of Oslo, Norway. GR was supported by Fundação para a Ciência e a Tecnologia project 2022.01448.PTDC, DOI 10.54499/2022.01448.PTDC.

## REPOSITORY OVERVIEW

This repository provides
(i) Stratification of the population of the Netherlands by age and by risk due to chronic conditions (CoviCron)
(ii): Stratification of the population of the Netherlands by age and by risk due to chronic conditions (VERDI)

## DATA 

Data analyses for the Netherlands were carried out within a dedicated remote access research environment of Statistics Netherlands. Within the research environment, data were linked at individual level from the Population Registry (PR), Dutch Hospital Discharge Registry (DHDR), COVID-19 vaccination registry (CIMS), and COVID-19 infections registry (GGDCOVID19BM). The PR contains information from all inhabitants of the Netherlands. From the PR, age was extracted. From the DHDR, data were extracted on date of hospital admission and discharge, and diagnosis during admission. Diagnosis are registered according to the International Statistical Classifications of Diseases and Related Health Problems, 10th revision (ICD-10) of the WHO. CIMS contains information about COVID-19 vaccines administered in the Netherlands, for individuals that gave permission to be registered in the Dutch national vaccine registry. For the primary vaccine series: 93% gave permission, for the first booster round this was 95%, and the second booster round: 99%. From CIMS, date of COVID-19 vaccine administration, type of vaccination, and vaccine series (either primary series, or booster) was extracted. The GGDCOVID19BM registry contains information about reported COVID-19 infections to the Dutch public health services. COVID-19 is a so-called ‘notifiable disease’ which means that physicians and medical microbiology laboratories are obligated to report positive COVID-19 cases to the public health services. From the GGDCOVID19BM, date of reported COVID-19 infection was extracted.

Data analyses for Norway were stored and created within the service for sensitive data provided by the University of Oslo. Within this research environment, several population data sources were linked at the individual level using unique personal identification numbers, including the National Population Registry, the Norwegian Patient Registry (NPR), the Norwegian Immunisation Registry (SYSVAK), the Norway Control and Payment of Health Reimbursements (KUHR), and Statistics Norway (SSB). The National Population Registry and SSB were used to extract age, immigration, or emigration dates on all inhabitants of Norway. The SYSVAK is a national electronic immunisation register based on mandatory records of individual vaccination since birth, and includes information on the dose number, type, and date of COVID-19 vaccine administered. The KUHR and NPR provided information on clinical diagnosis registered in primary and secondary care (both outpatient and inpatient), including the date of diagnosis. Diagnoses are coded according to the ICD-10 and the International Classification of Primary Care, 2nd edition (ICPC-2). The NPR provided information about COVID-19 hospitalizations, including ICD-10 diagnosis codes, dates of admission, and discharge. 


### Definition chronic medical conditions
Within the Remote Access Research Environment of Statistics Netherlands and service for sensitive data Norway, chronic medical conditions were defined on the basis of previous hospital admission in the five years preceding the indexdate (either January 1st 2020, COVID-19 hospitalization, or positive COVID-19 reporting). An overview of specific ICD-10 codes used to define risk based on chronic medical conditions can be found in Supplement Table 1-3. R-codes available on Github. Risk classifications include: 

> **European risk classification** Obtained from ‘Core protocol for ECDC studies of COVID-19 vaccine effectiveness against hospitalization with Severe Acute Respiratory Infection, laboratory-confirmed with SARS-CoV-2 or with seasonal influenza – Version 3.0’ from February 20249. This classification contains a list of ICD-10 codes referring to chronic medical conditions. In this classification, we refer to mandatory and optional conditions as high- and moderate-risk, respectively.
> 
> **Dutch risk classification**	Obtained from the ‘Adviesnota Vaststelling volwassen medische risicogroepen COVID-19-vaccinatiecampagne’ (‘Advice note Establishing adult medical risk groups for COVID-19 vaccination campaign’ in English). This classification contains a list of chronic medical conditions defined as either high- or moderate-risk.7 Manuscript author and medical doctor (AdB) converted each medical condition to the relevant ICD-10 code(s). This was verified by IW, with no discrepancies found.
> 
> **Norwegian risk classification**	Obtained from the Norwegian Health Institute in July 20238. This classification contains a list of chronic medical conditions with corresponding ICPC-2 and ICD-10 codes as either high-risk or increased-risk, which we renamed as high- and moderate-risk, respectively for consistency with the other classifications. An individual was assigned to high- or moderate-risk if either the relevant ICPC-2 or ICD-10 diagnostic code was present. 

References: 
> Norwegian Institute of Public Health. Coronavirus vaccine. https://www.fhi.no/en/id/corona/coronavirus-immunisation-programme/coronavirus-vaccine/#about-risk-groups-and-childrenadolescents-with-underlying-conditions (2023).
> 
>European Centre for Disease Prevention and Control. Core Protocol for ECDC VEBIS Studies of COVID-19 Vaccine Effectiveness against Hospitalisation with Severe Acute Respiratory Infection, Laboratory-Confirmed with SARS-CoV-2 or with Seasonal Influenza - Version https://www.ecdc.europa.eu/en/publications-data/core-protocol-ecdc-studies-covid-19-vaccine-effectiveness-3 (2024).
> 
>Rijksinstituut voor Volksgezondheid en Milieu. The National Immunisation Programme in the Netherlands. Surveillance and Developments in 2021-2022. https://rivm.openrepository.com/handle/10029/625373 (2021).




### Definition COVID-19 hospitalization

COVID-19 Hospitalization was defined as hospital admission for at least 24 hours and i) a primary diagnosis with ICD-10 code U071, U072, or U109; or ii) a secondary diagnosis with ICD-10 code U071, U072, or U109 combined with a primary code of respiratory illness (defined as any of the ICD-10 J-codes). This definition was based on prior research in Norway14.

### Definition COVID-19 death
Death due to COVID-19 was defined as individuals with either i) COVID-19 listed as the primary cause of death (ICD-10 codes U071, U072, or U109 or ii) with a COVID-19 hospitalization in the 7 days preceding date of death. In the Dutch data registry, only the second definition was used, because cause of death was not available in death registry. 


### Overview Databases
The following databases were used (a link to the description of the data is provided):

## Population Registry (provided by Statistics Netherlands)

This dataset contains all individuals registered in the Netherlands. The following information was extracted: age.
For more detailed information: 

> https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/gbapersoontab-persoonskenmerken-van-personen-in-de-brp

### CIMS (provided by Statistics Netherlands) 

This dataset contains information about COVID-19 vaccines administered in the Netherlands for individuals who gave permission to be registered (basic series: 93%, booster (95-99%). The following information was extracted: date of vaccination and type of vaccination.
For more detailed information: 

> https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/cims-covid-vaccinatie-informatie-en-monitoringssysteem

### GGDCOVID19BM (provided by Statistics Netherlands)

This dataset contains information on the reported COVID-19 cases. The following information was extracted: date of positive COVID-19 reporting.
For more detailed information: 

> https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/ggd-covid19-besmettingsmeldingen

### Dutch Hospital Discharge Registry (provided by Dutch Hospital Data) 
This dataset contains the diagnosis and date of hospitalization from all hospitals in the Netherlands. The following information was extracted: date of hospital admission and discharge, and diagnosis codes registered during admission.

For more detailed information: 

> https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/lbzbasistab-ziekenhuisopnamen-lbz and > https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/microdatabestanden/lbzdiagnosentab-diagnosen-bij-ziekenhuisopnamen--lbz--

The aggregated data were added to the ``` data ``` folder for convenience.



### OS requirements

Minimal Necessary Requirements to access the Remote Access Research Environment of Statistics Netherlands:

- Microsoft Windows Minimum Version 10
- Apple macOS Minimum Version 13
- VMware Horizon Client Download the latest Version for Windows/macOS
- FortiClient Download the FortiVPN client setup for Windows/macOS

To access the Remote Access Research Environment of Statistics Netherlands, we used 

- Windows Version 10 Enterprise Version 22H2
- FortiClient 6.4.7.1713
- VMware Horizon Client Version 2203 Build 8.5.0.
- Statistics Netherlands provided Windows 11 Enterprise Version 23H2 for R.

### Hardware requirements

Statistics Netherlands: A standard computer to log onto the Remote Access Research Environment of Statistics Netherlands. 
Statistics Netherlands: A standard computer to log onto the Service for Sensitive Data Norway. 

## Installation guide

Statistics Netherlands provides the following R Version and packages that were used to aggregate data:

- R Version 4.2.3
  
> https://www.r-project.org/

- R Studio Version 2023.06.2 (Interface to R)

> https://rstudio.com/

- data.table R package Version 1.14.8

> https://cran.r-project.org/web/packages/data.table/index.html

- haven R package Version 2.5.2

> https://cran.r-project.org/web/packages/haven/index.html

- eeptools R package Version 1.2.5

> https://cran.r-project.org/package=eeptools

- lubridate R package Version 1.9.2

> https://cran.r-project.org/web/packages/lubridate/vignettes/lubridate.html

- ggplot2 R package Version 3.4.2

> https://cran.r-project.org/package=ggplot2

- openxlsx R package Version 4.2.5.2

> https://cran.r-project.org/package=openxlsx

- tidyverse R package Version 2.0.0

> https://cran.r-project.org/package=tidyverse

Access to the Remote Access Research Environment of Statistics Netherlands can be requested on 

> https://www.cbs.nl/nl-nl/onze-diensten/maatwerk-en-microdata/microdata-zelf-onderzoek-doen/aanvraag-toegang-microdata
