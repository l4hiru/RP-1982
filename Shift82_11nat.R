#0) Packages 

library(arrow)
library(haven)        # For reading .dta files
library(dplyr)        # For data manipulation (mutate, case_when, group_by, etc.)
library(tidyverse)    # Includes ggplot2, dplyr, tidyr, etc.
library(janitor)      # For cleaning data, e.g., renaming variables
library(summarytools) # For frequency tables (freq)
library(reshape2)     # For reshaping data (melt, cast)
library(stargazer)    # For regression tables (if needed)
library(plm)          # For panel data models (if needed)

#I) Dataset

data_1982 <- read_sas("verdugo_rp82_fdq_10.sas7bdat", col_select = c("D", "IN", "N", "SOND"))  # Shift for the shift-share IV (Edo et al. 2019)

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
