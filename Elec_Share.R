# RP 1982 (INSEE)

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
library(COGugaison)

#I) Data --------------------------------------------------------------------------

#RP 1982

data <- read_sas("verdugo_rp82_fdq_10.sas7bdat", col_select = c("D", "C", "AE100", "AE600", "SOND"))

# II) Variables 

# A) Creation of the municipality INSEE number

data <- data %>%
  mutate(code_commune = paste0(D, C)) # Code DC = D + C = Municipality code (Lyon + Marseille = 2 municipalities)

colnames(data)

COG_akinator(vecteur_codgeo = data$code_commune, donnees_insee = TRUE) # COG1982

length(unique(data$code_commune)) # 36,420

# B) AE100 

freq(data$AE100)

# C) Total pop

data %>%
  summarise(pop_totale = sum(SOND, na.rm = TRUE))

# III) Dataset

data_commune <- data %>% 
  group_by(code_commune) %>% 
  summarise(
    pop_totale = sum(SOND, na.rm = TRUE),
    pop_elec = sum(SOND[AE100 == "06"], na.rm = TRUE)
  ) %>% 
  mutate(share_elec = pop_elec / pop_totale)

write_parquet(data_commune, "share_elec82.parquet")