library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(RColorBrewer)
library(openxlsx)

inds <- read.xlsx("G100_Data_NFG.xlsx", sheet = "G100") %>%
  as_tibble() %>%
  group_by(TRBC.Activity.Name) %>%
  summarise(n=n())

owns <- read.xlsx("G100_Data_NFG.xlsx", sheet = "Owners") %>%
  as_tibble() %>%
  rename(value_held = `value_held.(USD)`)

creds <- read.xlsx("G100_Data_NFG.xlsx", sheet = "Creditors") %>%
  as_tibble()


# Largest Owners

largest_owners = owns %>%
  group_by(Company_Name) %>%
  slice_max(value_held, n = 10) %>%
  group_by(Investor) %>%
  summarise(total_ownership = sum(value_held)) %>%
  slice_max(total_ownership, n = 10)

largest_owners %>%
  ggplot(aes(x = reorder(Investor, -total_ownership), y = total_ownership)) +
  geom_point() +
  scale_color_brewer(palette = "Accent") +
  labs(x = "Investor", y = "Amount", title = "Top 20 Shareholders")






owns$Investor <- gsub(".*BlackRock.*", owners[1], owns$Investor)
owns$Investor <- gsub(".*Vanguard.*", owners[2], owns$Investor)
owns$Investor <- gsub(".*State Street.*", owners[3], owns$Investor)

owns$Instrument <- gsub("NESN.S", corps[1], owns$Instrument)
owns$Instrument <- gsub("PEP.O", corps[2], owns$Instrument)
owns$Instrument <- gsub("KO", corps[3], owns$Instrument)
owns$Instrument <- gsub("MDLZ.O", corps[4], owns$Instrument)
owns$Instrument <- gsub("DANO.PA", corps[5], owns$Instrument)
owns$Instrument <- gsub("BIMBOA.MX", corps[6], owns$Instrument)
owns$Instrument <- gsub("KHC.O", corps[7], owns$Instrument)
owns$Instrument <- gsub("GIS", corps[8], owns$Instrument)
owns$Instrument <- gsub("KDP.O", corps[9], owns$Instrument)
owns$Instrument <- gsub("HRL", corps[10], owns$Instrument)


owns <- owns %>%
  filter(Investor == "BlackRock" | 
           Investor == "Vanguard" | 
           Investor == "State Street") %>%
  group_by(Instrument, Investor) %>% 
  summarise(Holdings = sum(Holdings), .groups = "drop") %>%
  arrange(Instrument)

p <- owns %>%
  ggplot(aes(y = Holdings, x = Instrument, fill = Investor)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20)) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    y = "Percentage Holdings",
    x = "")


ggsave("plot.png", p, width = 15, height = 6, dpi = 300)

write.csv(owns, "owns_Big3.csv", row.names = TRUE)
