library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(RColorBrewer)
library(openxlsx)
library(treemap)

# #-----------------
# # GROUP OF 100
# #----------------
# 
inds <- read.xlsx("G100_Data_NFG.xlsx", sheet = "G100") %>%
  as_tibble()
  # group_by(TRBC.Activity.Name) %>%
  # summarise(n=n())

owns <- read.xlsx("G100_Data_NFG.xlsx", sheet = "Owners") %>%
  as_tibble() %>%
  rename(value_held = `value_held.(USD)`)

creds <- read.xlsx("G100_Data_NFG.xlsx", sheet = "Creditors") %>%
  as_tibble() %>%
  rename(value_issued = `Loan_Manager_Amount.(USD)`)

#------------------
# SECTORAL GROUPS
#-----------------

# inds <- read.xlsx("Sector_Data_NFG.xlsx", sheet = "Companies") %>%
#   as_tibble() 
# # group_by(TRBC.Activity.Name) %>%
# # summarise(n=n())
# 
# owns <- read.xlsx("Sector_Data_NFG.xlsx", sheet = "Owners") %>%
#   as_tibble() %>%
#   rename(value_held = `value_held.(USD)`)
# 
# creds <- read.xlsx("Sector_Data_NFG.xlsx", sheet = "Creditors") %>%
#   as_tibble() %>%
#   rename(value_issued = `Loan_Manager_Amount.(USD)`)


# Largest Owners 

largest_owners = owns %>%
  #--------------------------------------------------
  # Fill With Companies to INCLUDE in sample (use ! before "Company_Name" to EXCLUDE)
  # filter(Company_Name %in% c(
  # )) %>%
  # --------------------------------------------------
  group_by(Investor) %>%
  summarise(total_ownership = sum(value_held)) %>%
  #---------------------------------------------------
  # Fill with investors to EXCLUDE from results
  filter(!Investor %in% c(
    "Bezos (Jeffrey P)",
    "Walton Enterprises, L.L.C.",
    "China Kweichow Moutai Distillery (Group) Co., Ltd.",
    "Walton Family Holdings Trust",
    "Kretinsky (Daniel) & Tkac (Patrik )",
    "Capital Research Global Investors",
    "Srinivasan (Mallika)",
    "Shareholders group of  Metro AG",
    "Stichting Anheuser-Busch InBev",
    "Giovanni Agnelli B V"
  )) %>%
  #--------------------------------------------------
  mutate(total = sum(total_ownership), 
         perc = total_ownership*100 / total,
         label = paste0(Investor, "\n", round(perc, 2), "%")
         ) %>%
  slice_max(perc, n = 20)

largest_owners %>%
  treemap(index = "label",
          vSize = "total_ownership",
          type = "index",
          palette = "Set3",
          border.col = "White",
          title = "Top 20 Owners of")

# Largest Creditors

largest_creditors = creds %>%
  #--------------------------------------------------
  # Fill With Companies to INCLUDE in sample (use ! before "Company_Name" to EXCLUDE)
# filter(Company_Name %in% c(
# )) %>%
  #--------------------------------------------------
  group_by(Loan_Manager) %>%
  summarise(total_credit = sum(value_issued)) %>%
  mutate(total = sum(total_credit), 
         perc = total_credit*100 / total,
         label = paste0(Loan_Manager, "\n", round(perc, 2), "%")
  ) %>%
  slice_max(perc, n = 20)

largest_creditors %>%
  treemap(index = "label",
          vSize = "total_credit",
          type = "index",
          palette = "Set3",
          border.col = "White",
          title = "Top 20 Creditors")






