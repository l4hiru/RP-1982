#RP 1982 (INSEE)

#O) Packages 

library(arrow)
library(haven)        # For reading .dta files
library(dplyr)        # For data manipulation (mutate, case_when, group_by, etc.)
library(tidyverse)    # Includes ggplot2, dplyr, tidyr, etc.
library(janitor)      # For cleaning data, e.g., renaming variables
library(summarytools) # For frequency tables (freq)
library(reshape2)     # For reshaping data (melt, cast)
library(stargazer)    # For regression tables (if needed)
library(plm)          # For panel data models (if needed)

#I) Data --------------------------------------------------------------------------

#RP 1982

data <- read_sas("verdugo_rp82_fdq_10.sas7bdat", col_select = c("IN", "N", "R", "D", "S", "AD", "M", "TA", "AE100", "DIPC", "SOND", "IRA", "IRA75", "PRA", "IMA", "T")) 

data <- read_sas("verdugo_rp82_fdq_10.sas7bdat", col_select = c("AU", "CC", "CCR", "D", "R", "AD", "TA", "AE3", "AE100", "CS", "CS8", "DIPC", "DIP", "DEG", "AFE", "SOND")) #Socio-demographic controls

data_1982 <- read_sas("verdugo_rp82_fdq_10.sas7bdat", col_select = c("D", "IN", "N", "DIPC", "SOND"))  # Shift for the shift-share IV (Edo et al. 2019)

#II) Variables ------------------------------------------

#A) Nationality 

freq(data_1982$IN)

data_1982 <- data_1982 %>%
  mutate(Nationality = recode(IN,
         "11" = "Native",
         "12" = "Naturalized",
         "20" = "Immigrant"))

freq(data_1982$Nationality)

#B) Diploma 

data_1982 <- data_1982 %>%
  mutate(Diploma = case_when(
    DIPC == "*" ~ NA_character_,  # < 15 yo, N.A
    DIPC %in% c("0") ~ "Low",  # No diploma, CEP, DFEO, etc.
    DIPC %in% c("1", "2") ~ "Mid",  # BEPC, BEP, CAP, etc.
    DIPC %in% c("3", "4", "5") ~ "High",  # BAC or more
    TRUE ~ NA_character_
  )) %>%
  mutate(Diploma = factor(Diploma, levels = c("Low", "Mid", "High")))

#C) Origin 

freq(data_1982$Origin)

data_1982 <- data_1982 %>%
  mutate(Origin = recode(N,
    "01" = "Europe",
    "02" = "Europe",
    "03" = "Europe",
    "04" = "East_Europe",
    "05" = "Europe",
    "06" = "South_Europe",
    "07" = "Europe",
    "08" = "Europe",
    "09" = "East_Europe",
    "10" = "Europe",
    "11" = "South_Europe",
    "12" = "Europe",
    "13" = "Europe",
    "14" = "Europe",
    "15" = "East_Europe",
    "16" = "South_Europe",
    "17" = "East_Europe",
    "18" = "Europe",
    "19" = "Europe",
    "20" = "Europe",
    "21" = "East_Europe",
    "22" = "East_Europe",
    "23" = "Europe",
    "24" = "Europe",
    "25" = "East_Europe",
    "29" = "East_Europe",
    "31" = "Maghreb",
    "33" = "Africa",
    "34" = "Africa",
    "35" = "Africa",
    "36" = "Africa",
    "37" = "Africa",
    "39" = "Africa",
    "40" = "Africa",
    "42" = "Africa",
    "43" = "Africa",
    "44" = "Africa",
    "45" = "Maghreb",
    "46" = "Africa",
    "47" = "Africa",
    "48" = "Africa",
    "49" = "Africa",
    "50" = "Africa",
    "51" = "Africa",
    "52" = "Maghreb",
    "53" = "Africa",
    "54" = "Africa",
    "55" = "Africa",
    "56" = "Africa",
    "57" = "Africa",
    "59" = "Africa",
    "60" = "North_America",
    "61" = "North_America",
    "62" = "North_America",
    "63" = "South_America",
    "64" = "South_America",
    "65" = "South_America",
    "66" = "South_America",
    "67" = "South_America",
    "68" = "South_America",
    "69" = "South_America",
    "70" = "Asia",
    "71" = "Asia",
    "72" = "Asia",
    "73" = "Asia",
    "74" = "Asia",
    "75" = "Asia",
    "76" = "Asia",
    "77" = "Asia",
    "78" = "Asia",
    "79" = "Asia",
    "80" = "Asia",
    "81" = "Asia",
    "82" = "Asia",
    "83" = "Asia",
    "84" = "Asia",
    "85" = "Asia",
    "86" = "Asia",
    "89" = "Asia" ))

#III) Shifts in 1982

#A) Immigrant

immi_1982 <- data_1982 %>%
  filter(Nationality == "Immigrant") %>%
  group_by(Origin, Diploma) %>%
  summarise(
    total_imm_1982 = sum(SOND, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(
    Origin_group = Origin
  )

#B) Native

native_1982 <- data_1982 %>%
  filter(Nationality == "Native") %>%
  group_by(Diploma) %>%
  summarise(
    total_native_1982 = sum(SOND, na.rm = TRUE),
    .groups = "drop"
  )

#C) Naturalize

naturalized_1982 <- data_1982 %>%
  filter(Nationality == "Naturalized") %>%
  group_by(Diploma) %>%
  summarise(
    total_naturalized_1982 = sum(SOND, na.rm = TRUE),
    .groups = "drop"
  )

#D) Pooled shift

shift_1982 <- bind_rows(
  immi_1982 %>%
    mutate(
      Nationality = "Immigrant",
      Year = as.factor(1982),
      Count = total_imm_1982
    ) %>%
    select(Nationality, Origin_group, Diploma, Count, Year),

  native_1982 %>%
    mutate(
      Nationality = "Native",
      Origin_group = "French",
      Year = as.factor(1982),
      Count = total_native_1982
    ) %>%
    select(Nationality, Origin_group, Diploma, Count, Year),

  naturalized_1982 %>%
    mutate(
      Nationality = "Naturalized",
      Origin_group = "French",
      Year = as.factor(1982),
      Count = total_naturalized_1982
    ) %>%
    select(Nationality, Origin_group, Diploma, Count, Year)
)

sum(shift_1982$Count)

write_parquet(shift_1982, "shift_1982.parquet")






####### NEWS SHIFTS - FOR SS IV AFTER G. VERDUGO ####

data_1982 <- read_sas("verdugo_rp82_fdq_10.sas7bdat", col_select = c("D", "IN", "N", "SOND"))  # Shift for the shift-share IV (Edo et al. 2019)

#I) Variables ----------------------------------------

#A) Nationality 

freq(data_1982$IN)

data_1982 <- data_1982 %>%
  mutate(Nationality = recode(IN,
         "11" = "Native",
         "12" = "Naturalized",
         "20" = "Immigrant"))

freq(data_1982$Nationality)

#B) Origin 

freq(data_1982$N)

data_1982 <- data_1982 %>%
  mutate(Origin = recode(N,
    "01" = "Europe",
    "02" = "Europe",
    "03" = "Europe",
    "04" = "Europe",
    "05" = "Europe",
    "06" = "Spain",
    "07" = "Europe",
    "08" = "Europe",
    "09" = "Europe",
    "10" = "Europe",
    "11" = "Italy",
    "12" = "Europe",
    "13" = "Europe",
    "14" = "Europe",
    "15" = "Portugal",
    "16" = "Europe",
    "17" = "Europe",
    "18" = "Europe",
    "19" = "Europe",
    "20" = "Europe",
    "21" = "Europe",
    "22" = "Europe",
    "23" = "Europe",
    "24" = "Europe",
    "25" = "Europe",
    "29" = "Europe",
    "31" = "Algeria",
    "33" = "Africa",
    "34" = "Africa",
    "35" = "Africa",
    "36" = "Africa",
    "37" = "Africa",
    "39" = "Africa",
    "40" = "Africa",
    "42" = "Africa",
    "43" = "Africa",
    "44" = "Africa",
    "45" = "Morocco",
    "46" = "Africa",
    "47" = "Africa",
    "48" = "Africa",
    "49" = "Africa",
    "50" = "Africa",
    "51" = "Africa",
    "52" = "Tunisia",
    "53" = "Africa",
    "54" = "Africa",
    "55" = "Africa",
    "56" = "Africa",
    "57" = "Africa",
    "59" = "Africa",
    "60" = "America",
    "61" = "America",
    "62" = "America",
    "63" = "America",
    "64" = "America",
    "65" = "America",
    "66" = "America",
    "67" = "America",
    "68" = "America",
    "69" = "America",
    "70" = "Asia",
    "71" = "Asia",
    "72" = "Asia",
    "73" = "Asia",
    "74" = "Asia",
    "75" = "Asia",
    "76" = "Asia",
    "77" = "Asia",
    "78" = "Asia",
    "79" = "Asia",
    "80" = "Turkey",
    "81" = "Asia",
    "82" = "Asia",
    "83" = "Asia",
    "84" = "Asia",
    "85" = "Asia",
    "86" = "Asia",
    "89" = "Asia" ))

freq(data_1982$Origin)

#II) Shift 

immi_1982 <- data_1982 %>%
  filter(Nationality == "Immigrant") %>%
  group_by(Origin) %>%
  summarise(
    shift_immi_1982 = sum(SOND, na.rm = TRUE),
    .groups = "drop"
  )

#III) Final dataset

write_parquet(immi_1982, "immi_shift82_11nat.parquet")














#Pondération (SOND)

freq(data$SOND)

#Region (R)

freq(data$R)

data <- data %>%
  mutate(Region = recode(R,
    "11" = "Ile-de-France",
    "21" = "Champagne-Ardenne",
    "22" = "Picardie",
    "23" = "Haute-Normandie",
    "24" = "Centre",
    "25" = "Basse-Normandie",
    "26" = "Bourgogne",
    "31" = "Nord-Pas-de-Calais",
    "41" = "Lorraine",
    "42" = "Alsace",
    "43" = "Franche-Comté",
    "52" = "Pays de la Loire",
    "53" = "Bretagne",
    "54" = "Poitou-Charentes",
    "72" = "Aquitaine",
    "73" = "Midi-Pyrénées",
    "74" = "Limousin",
    "82" = "Rhône-Alpes",
    "83" = "Auvergne",
    "91" = "Languedoc-Roussillon",
    "93" = "Provence-Alpes-Côte d'Azur",
    "94" = "Corse"
  ))

freq(data$Region)

#Genre (S)

freq(data$S)

data <- data %>%
  mutate(Gender = recode(S,
         "1" = "Homme",
         "2" = "Femme"))

freq(data$Gender)

data <- data %>%
  mutate(Male = case_when(
    S %in% c("1") ~ 1,
    TRUE ~ 0
  ))

#Immigrant Status (IN)

freq(data$IN)

data <- data %>%
  mutate(Nationality = recode(IN,
         "11" = "French",
         "12" = "French",
         "20" = "Immigrant"))

data <- data %>%
  mutate(Immigrant = case_when(
    IN %in% c("20") ~ 1,
    TRUE ~ 0
  ))

sum(data$Immigrant * data$SOND, na.rm = TRUE) #Nbr d'immigrés avec pondération au 1/4 (3 714 200)
sum(data$Immigrant * data$SOND, na.rm = TRUE) / sum(data$SOND, na.rm = TRUE) #Part d'immigrés dans la pop totale (~ 7%)

#Nationality

freq(data$N)

# Load dplyr package
library(dplyr)

data <- data %>%
  mutate(Nationality = recode(N,
    "01" = "Europe",
    "02" = "Europe",
    "03" = "Europe",
    "04" = "Europe",
    "05" = "Europe",
    "06" = "Espagnols",
    "07" = "Europe",
    "08" = "Europe",
    "09" = "Europe",
    "10" = "Europe",
    "11" = "Europe",
    "12" = "Europe",
    "13" = "Europe",
    "14" = "Europe",
    "15" = "Europe",
    "16" = "Portugais",
    "17" = "Europe",
    "18" = "Europe",
    "19" = "Europe",
    "20" = "Europe",
    "21" = "Europe",
    "22" = "Europe",
    "23" = "Europe",
    "24" = "Europe",
    "25" = "Europe",
    "29" = "Europe",
    "31" = "Maghreb",
    "33" = "Africa",
    "34" = "Africa",
    "35" = "Africa",
    "36" = "Africa",
    "37" = "Africa",
    "39" = "Africa",
    "40" = "Africa",
    "42" = "Africa",
    "43" = "Africa",
    "44" = "Africa",
    "45" = "Maghreb",
    "46" = "Africa",
    "47" = "Africa",
    "48" = "Africa",
    "49" = "Africa",
    "50" = "Africa",
    "51" = "Africa",
    "52" = "Maghreb",
    "53" = "Africa",
    "54" = "Africa",
    "55" = "Africa",
    "56" = "Africa",
    "57" = "Africa",
    "59" = "Africa",
    "60" = "North_America",
    "61" = "North_America",
    "62" = "North_America",
    "63" = "South_America",
    "64" = "South_America",
    "65" = "South_America",
    "66" = "South_America",
    "67" = "South_America",
    "68" = "South_America",
    "69" = "South_America",
    "70" = "Asia",
    "71" = "Asia",
    "72" = "Asia",
    "73" = "Asia",
    "74" = "Asia",
    "75" = "Asia",
    "76" = "Asia",
    "77" = "Asia",
    "78" = "Asia",
    "79" = "Asia",
    "80" = "Turquie",
    "81" = "Asia",
    "82" = "Asia",
    "83" = "Asia",
    "84" = "Asia",
    "85" = "URSS",
    "86" = "Océanie",
    "89" = "Océanie" ))

freq(data$Nationality)

data <- data %>%
  mutate(NatRegularized = case_when(
    Nationality %in% c("Portugais", "Espagnols", "Maghreb", "Africa", "Turquie") ~ 1,
    TRUE ~ 0
  ))

freq(data$NatRegularized)

#Age (AD)

freq(data$AD)

data$Age <- data$AD

#Activity Status (TA)

freq(data$TA)

data <- data %>%
  mutate(Activity = recode(TA,
         "0" = "Inactif",
         "1" = "Actif occupé",
         "3" = "Chômage", 
         "4" = "Retraité",
         "5" = "Inactif",
         "6" = "Inactif"))

freq(data$Activity)

data <- data %>%
  mutate(Active = case_when(
    TA %in% c("1") ~ 1,
    TRUE ~ 0
  ))

#Matrimonial Status

data <- data %>%
  mutate(Matrimonial = recode(M,
         "1" = "Célibataire",
         "2" = "Marié",
         "3" = "Veuf",
         "4" = "Divorcé"))

freq(data$Matrimonial)  

data <- data %>%
  mutate(Celib = case_when(
    M %in% c("1") ~ 1,
    TRUE ~ 0
  ))

freq(data$Celib)

#Diploma

freq(data$DIPC)

data <- data %>%
  mutate(LowEduc = case_when(
    DIPC %in% c("0", "1", "2") ~ 1,
    TRUE ~ 0
  ))

freq(data$LowEduc)

#Economic Status (AE100)

freq(data$AE100)

data <- data %>%
  mutate(RegularizedSector = case_when(
    AE100 %in% c("01", "02", "03", "44", "45", "46", "47", "55", "57", "58", "59",
                 "60", "61", "62", "63", "64", "67", "98") ~ 1,
    TRUE ~ 0
  ))

freq(data$RegularizedSector)

#Dummy for ImmiRegularized

library(dplyr)

data <- data %>%
  mutate(
    ImmiRegularized = case_when(
      Age < 32 & 
      Immi == 1 & 
      Celib == 1 & 
      NatRegularized == 1 & 
      RegularizedSector == 1 & 
      Male == 1 & 
      Active == 1 & 
      LowEduc == 1 & 
      Region %in% c("Ile-de-France", "Provence-Alpes-Côte d'Azur", "Rhône-Alpes") ~ 1,
      TRUE ~ 0 
    ))

freq(data$ImmiRegularized)



data <- data %>%
  mutate(
    ImmiRegularized = case_when(
      Age < 32 & 
      Immi == 1 & 
      Celib == 1 & 
      NatRegularized == 1 & 
      RegularizedSector == 1 & 
      Male == 1 & 
      Active == 1 & 
      LowEduc == 1 & 
      Region %in% c("Ile-de-France", "Provence-Alpes-Côte d'Azur", "Rhône-Alpes") ~ 1,
      TRUE ~ 0 
    ))


data <- data %>%
  mutate(
    Illegals = case_when(
      Age < 32 & 
      Immi == 1 & 
      Celib == 1 & 
      NatRegularized == 1 & 
      RegularizedSector == 1 & 
      Male == 1 & 
      Active == 1 & 
      LowEduc == 1 & 
      Region %in% c("Ile-de-France", "Provence-Alpes-Côte d'Azur", "Rhône-Alpes") ~ 1,
      TRUE ~ 0 
    ))

freq(data$IN)

freq(data$IMA)
freq(data$PRA)

freq(data$IRA75)
freq(data$IRA)

data <- data %>%
  mutate(
    Illegals = case_when(
     IN == "20" ~ 1,
     IMA == "3" ~ 1,
     IRA == "4" ~ 4,
     IRA75 == "4" ~ 4,
      TRUE ~ 0 
    ))


data <- data %>%
  mutate(
    Illegals = case_when(
      IN == "20" ~ 1,               
      IMA == "3" ~ 1,               
      IRA == "4" ~ 4,               
      IRA75 == "4" ~ 4,              
      N %in% c("31", "33", "34", "35", "36", "37", "39", "40", "42", "43", "44", "45", "46", "47", "48", "49", "50", "51", "52", "53", "54", "55", "56", "57", "59") ~ 1, # Africa
      N %in% c("70", "71", "72", "73", "74", "75", "76", "77", "78", "79", "80", "81", "82", "83", "84") ~ 1, # Asian nationalities
      N %in% c("16", "06", "22") ~ 1, # Portuguese, Spanish, and Yougoslavians
      TRUE ~ 0 
    )
  )

sum(data$Illegals * data$SOND, na.rm = TRUE)

data <- data %>%
  mutate(
    Illegals = case_when(
      IN == "20" & IMA == "3" & IRA == "4" & IRA75 == "4" & TA == "1" &
      N %in% c("31", "33", "34", "35", "36", "37", "39", "40", "42", "43", "44", "45", "46", "47", "48", "49", "50", "51", "52", "53", "54", "55", "56", "57", "59") | 
      N %in% c("70", "71", "72", "73", "74", "75", "76", "77", "78", "79", "80", "81", "82", "83", "84") | 
      N %in% c("16", "06", "22") ~ 1,
      TRUE ~ 0 
    )
  )

class(data$IN)
class(data$IMA)
class(data$IRA)
class(data$IRA75)
class(data$N)

#write_parquet(data, "dataRP1982.parquet")


#II) Socio-demographic variables 

#A) French Departments 

freq(data$D)

#B) Municipality category (Rural vs. Urban)

freq(data$CC) 

data <- data %>%
  mutate(Rural = case_when(
    CC %in% c("00*", "01*") ~ 1,
    TRUE ~ 0
  ))


freq(data$Rural)

sum(data$Rural * data$SOND, na.rm = TRUE)

#C) Diploma 


freq(data$DEG)

data <- data %>%
  mutate(LowEduc = case_when(
    DEG %in% c("", "1") ~ 1, 
    TRUE ~ 0
  ))

freq(data$LowEduc)

data <- data %>%
  mutate(MidEduc = case_when(
    DEG %in% c("2") ~ 1, 
    TRUE ~ 0
  ))

freq(data$MidEduc)

data <- data %>%
  mutate(HighEduc = case_when(
    DEG %in% c("3") ~ 1, 
    TRUE ~ 0
  ))

freq(data$HighEduc)

#D) Labor Market

freq(data$TA)

data <- data %>%
  mutate(Unemployed = case_when(
    TA %in% c("3") ~ 1, 
    TRUE ~ 0
  ))

freq(data$Unemployed)

data <- data %>%
  mutate(Occupied = case_when(
    TA %in% c("1") ~ 1, 
    TRUE ~ 0
  ))

freq(data$Occupied)

#E) Numeric Age 

freq(data$AD)

data$AD <- as.numeric(data$AD)

data$Young <- ifelse(data$AD >= 18 & data$AD <= 30, 1, 0)

data$Adult <- ifelse(data$AD >= 18, 1, 0)

data$WorkingAge <- ifelse(data$AD >= 18 & data$AD <= 64, 1, 0)

#F) Socio-professional / Occupation categories 

freq(data$CS8)

data <- data %>%
  mutate(Farmer = case_when(
    CS8 %in% c("1") ~ 1, 
    TRUE ~ 0
  ))

data <- data %>%
  mutate(BlueCollar = case_when( 
    CS8 %in% c("5", "-") ~ 1, 
    TRUE ~ 0
  ))

data <- data %>%
  mutate(Worker = case_when( 
    CS8 %in% c("6") ~ 1, 
    TRUE ~ 0
  ))

data <- data %>%
  mutate(Executive = case_when(
    CS8 %in% c("3") ~ 1, 
    TRUE ~ 0
  ))

data <- data %>%
  mutate(Inactive = case_when(
    CS8 %in% c("7", "8") ~ 1, 
    TRUE ~ 0
  ))

#H) Enrolled in Study 

freq(data$AFE)

data <- data %>%
  mutate(Study = case_when(
    AFE %in% c("99") ~ 1, 
    TRUE ~ 0
  ))

freq(data$Study)



#III) Creation of the (weighted) controls dataset at the departemental level

data_dep82 <- data %>% 
  group_by(D) %>% 
  summarise(
    RuralShare = sum((Rural == 1) * SOND) / sum(SOND), 
    UnemploymentRate = sum((Unemployed == 1) * SOND) / sum((Unemployed == 1 | Occupied == 1) * SOND),
    YoungShare = sum((Young == 1) * SOND) / sum((Adult == 1) * SOND),
    FarmerShare = sum((Farmer == 1) * SOND) / sum((Occupied == 1) * SOND),
    ExecutiveShare = sum((Executive == 1) * SOND) / sum((Occupied == 1) * SOND),
    WorkerShare = sum((Worker == 1) * SOND) / sum((Occupied == 1) * SOND),
    InactiveShare = sum((Inactive == 1) * SOND) / sum((WorkingAge == 1) * SOND),
    HighEducShare = sum((HighEduc == 1) * SOND) / sum((WorkingAge == 1 & Study == 0) * SOND)
  )

mean(data_dep82$UnemploymentRate)

write_parquet(data_dep82, "data_dep82.parquet")

# Immigration / Preferences for redistribution for CREDOC dataset

data <- data %>%
  mutate(Immigrant = case_when(
    IN %in% c("20") ~ 1,
    TRUE ~ 0
  ))

data <- data %>%
  mutate(Nationality = recode(N,
    "01" = "Europe",
    "02" = "Europe",
    "03" = "Europe",
    "04" = "Europe",
    "05" = "Europe",
    "06" = "Europe",
    "07" = "Europe",
    "08" = "Europe",
    "09" = "Europe",
    "10" = "Europe",
    "11" = "Europe",
    "12" = "Europe",
    "13" = "Europe",
    "14" = "Europe",
    "15" = "Europe",
    "16" = "Europe",
    "17" = "Europe",
    "18" = "Europe",
    "19" = "Europe",
    "20" = "Europe",
    "21" = "Europe",
    "22" = "Europe",
    "23" = "Europe",
    "24" = "Europe",
    "25" = "Europe",
    "29" = "Europe",
    "31" = "Maghreb",
    "33" = "Africa",
    "34" = "Africa",
    "35" = "Africa",
    "36" = "Africa",
    "37" = "Africa",
    "39" = "Africa",
    "40" = "Africa",
    "42" = "Africa",
    "43" = "Africa",
    "44" = "Africa",
    "45" = "Maghreb",
    "46" = "Africa",
    "47" = "Africa",
    "48" = "Africa",
    "49" = "Africa",
    "50" = "Africa",
    "51" = "Africa",
    "52" = "Maghreb",
    "53" = "Africa",
    "54" = "Africa",
    "55" = "Africa",
    "56" = "Africa",
    "57" = "Africa",
    "59" = "Africa",
    "60" = "North_America",
    "61" = "North_America",
    "62" = "North_America",
    "63" = "South_America",
    "64" = "South_America",
    "65" = "South_America",
    "66" = "South_America",
    "67" = "South_America",
    "68" = "South_America",
    "69" = "South_America",
    "70" = "Asia",
    "71" = "Asia",
    "72" = "Asia",
    "73" = "Asia",
    "74" = "Asia",
    "75" = "Asia",
    "76" = "Asia",
    "77" = "Asia",
    "78" = "Asia",
    "79" = "Asia",
    "80" = "Turquie",
    "81" = "Asia",
    "82" = "Asia",
    "83" = "Asia",
    "84" = "Asia",
    "85" = "URSS",
    "86" = "Océanie",
    "89" = "Océanie" ))

freq(data$D)