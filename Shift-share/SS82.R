# RP 82

# 0) Packages 

library(arrow)
library(haven)        # For reading .dta files
library(dplyr)        # For data manipulation (mutate, case_when, group_by, etc.)
library(tidyverse)    # Includes ggplot2, dplyr, tidyr, etc.
library(janitor)      # For cleaning data, e.g., renaming variables
library(summarytools) # For frequency tables (freq)
library(reshape2)     # For reshaping data (melt, cast)
library(stargazer)    # For regression tables (if needed)

#I) Dataset

data_1982 <- read_sas("verdugo_rp82_fdq_10.sas7bdat", col_select = c("D", "IN", "N", "DIPC", "SOND"))  

#II) Variables ----------------------------------------

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

# C) Diploma 

freq(data_1982$DIPC)

data_1982 <- data_1982 %>%
  mutate(Diploma = case_when(
    DIPC == "*" ~ NA_character_,  # < 15 yo, N.A
    DIPC %in% c("0") ~ "Low",  # No diploma, CEP, DFEO, etc.
    DIPC %in% c("1", "2") ~ "Mid",  # BEPC, BEP, CAP, etc.
    DIPC %in% c("3", "4", "5") ~ "High",  # BAC or more
    TRUE ~ NA_character_
  )) %>%
  mutate(Diploma = factor(Diploma, levels = c("Low", "Mid", "High")))

freq(data_1982$Diploma)

# D) Departement

data_1982$Departement <- data_1982$D

# III) Share 1982 

# A) Immigrant share 

immi_nat <- data_1982 %>%
  filter(
    Nationality == "Immigrant", !is.na(Origin), Origin != "N.A", !is.na(Diploma), Diploma != "N.A"
  ) %>%
  group_by(Origin, Diploma) %>%
  summarise(immi_nat = sum(SOND, na.rm = TRUE), .groups = "drop")

immi_dep <- data_1982 %>%
  filter(
    Nationality == "Immigrant",
    !is.na(Origin), Origin != "N.A",
    !is.na(Diploma), Diploma != "N.A"
  ) %>%
  group_by(Departement, Origin, Diploma) %>%
  summarise(immi_dep = sum(SOND, na.rm = TRUE), .groups = "drop")

immi_share82 <- immi_dep %>%
  complete(
    Departement,
    Origin,
    Diploma,
    fill = list(immi_dep = 0)
  ) %>%
  left_join(immi_nat, by = c("Origin", "Diploma")) %>%
  mutate(
    immi_share = ifelse(immi_nat > 0, immi_dep / immi_nat, 0)
  )

# B) Native share

native_nat <- data_1982 %>%
  filter(Nationality == "Native",
         !is.na(Diploma), Diploma != "N.A") %>%
  mutate(Origin = "French") %>%
  group_by(Origin, Diploma) %>%
  summarise(native_nat = sum(SOND, na.rm = TRUE), .groups = "drop")

native_dep <- data_1982 %>%
  filter(Nationality == "Native",
         !is.na(Diploma), Diploma != "N.A") %>%
  mutate(Origin = "French") %>%
  group_by(Departement, Origin, Diploma) %>%
  summarise(native_dep = sum(SOND, na.rm = TRUE), .groups = "drop")

sum(native_dep$native_dep)

native_share82 <- native_dep %>%
  complete(
    Departement,
    Origin,
    Diploma,
    fill = list(native_dep = 0)
  ) %>%
  left_join(native_nat, by = c("Origin", "Diploma")) %>%
  mutate(native_share = ifelse(native_nat > 0, native_dep / native_nat, 0))

# C) Naturalized share

naturalized_nat <- data_1982 %>%
  filter(Nationality == "Naturalized",
         !is.na(Diploma), Diploma != "N.A") %>%
  mutate(Origin = "French") %>%
  group_by(Origin, Diploma) %>%
  summarise(naturalized_nat = sum(SOND, na.rm = TRUE), .groups = "drop")

naturalized_dep <- data_1982 %>%
  filter(Nationality == "Naturalized",
         !is.na(Diploma), Diploma != "N.A") %>%
  mutate(Origin = "French") %>%
  group_by(Departement, Origin, Diploma) %>%
  summarise(naturalized_dep = sum(SOND, na.rm = TRUE), .groups = "drop")

naturalized_share82 <- naturalized_dep %>%
  complete(
    Departement,
    Origin,
    Diploma,
    fill = list(naturalized_dep = 0)
  ) %>%
  left_join(naturalized_nat, by = c("Origin", "Diploma")) %>%
  mutate(naturalized_share = ifelse(naturalized_nat > 0, naturalized_dep / naturalized_nat, 0))

# IV) Final dataset 

write_parquet(immi_share82, "Shift-share/immi_share82.parquet")
write_parquet(native_share82, "Shift-share/native_share82.parquet")
write_parquet(naturalized_share82, "Shift-share/naturalized_share82.parquet")

### Combining the 1982 share and subsequent shifts 

# I) Datasets 

# A) 1982 Shares

immi_share82 <- read_parquet("Shift-share/immi_share82.parquet")
native_share82 <- read_parquet("Shift-share/native_share82.parquet")
naturalized_share82 <- read_parquet("Shift-share/naturalized_share82.parquet")

