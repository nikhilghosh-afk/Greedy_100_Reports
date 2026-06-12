library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(RColorBrewer)
library(openxlsx)
library(treemap)

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


# Largest Owners 

largest_owners = owns %>%
  #--------------------------------------------------
  # Fill With Companies to INCLUDE in sample (use ! before "Company_Name" to EXCLUDE)
  # filter(Company_Name %in% c(
  #   "Nestle SA",
  #   "Mondelez International Inc",
  #   "Kraft Heinz Co",
  #   "Kroger Co",
  #   "Tyson Foods Inc"
  # )) %>%
  # --------------------------------------------------
  group_by(Investor) %>%
  summarise(total_ownership = sum(value_held)) %>%
  #---------------------------------------------------
  # Fill with investors to EXCLUDE from results
  # filter(!Investor %in% c(
  #   "Bezos (Jeffrey P)",
  #   "Walton Enterprises, L.L.C.",
  #   "China Kweichow Moutai Distillery (Group) Co., Ltd.",
  #   "Walton Family Holdings Trust",
  #   "Kretinsky (Daniel) & Tkac (Patrik )",
  #   "Capital Research Global Investors",
  #   "Shareholders group of  Metro AG",
  #   "Stichting Anheuser-Busch InBev"
  # )) %>%
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
          title = "Top 20 Ownership in Processing Market Leaders (Nestle, Mondelez, Kraft Heinz, Kroger, Tyson Foods)")

# Largest Creditors

largest_creditors = creds %>%
  #--------------------------------------------------
  # Fill With Companies to INCLUDE in sample (use ! before "Company_Name" to EXCLUDE)
  # filter(Company_Name %in% c(
  #   "Archer-Daniels-Midland Co",
  #   "Bunge North America Inc",
  #   "Cargill Inc",
  #   "COFCO Corp",
  #   "Louis Dreyfus Co BV"
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
          title = "Top 20 Creditors to Group of 100")


