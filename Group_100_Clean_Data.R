library(tidyverse)

# Load and Clean Greedy 100 Data

g100 <- read.csv("g100.csv", header = TRUE) %>%
  mutate(across(where(is.character), ~na_if(.,"")))

cred <- read.csv("creditors_sec.csv", header = TRUE)
cred <- cred %>%
  na.omit(Deal.Permid) %>%
  mutate(across(where(is.character), ~na_if(.,""))) %>%
  fill(Company.Common.Name) 

own <- read.csv("owners_sec.csv", header = TRUE)
own <- own %>%
  filter(Investor.Shares.Held > 0) %>%
  fill(Close.Price) %>%
  mutate(value_held = Investor.Shares.Held*Close.Price) %>%
  mutate(across(where(is.character), ~na_if(.,""))) %>%
  fill(Company.Common.Name) 


# Identify Top Creditors

cred <- cred %>%
  mutate(Loan.Manager.Commitment.Amount = if_else(Loan.Manager.Commitment.Amount == 0, 
                                                  Loan.Package.Amount / Number.of.all.Managers..inc..Int.l.Co.Managers,
                                                  Loan.Manager.Commitment.Amount))

cred %>%
  group_by(Company.Common.Name) %>%
  summarise(n=n())

write.csv(cred, "sec_Creditors.csv")

# Identify Top Owners

own <- own %>%
  mutate(investor_parent_clean = case_when(
    # ---------- BlackRock ----------
    str_detect(Investor.Full.Name, regex("blackrock", ignore_case = TRUE)) ~ "BlackRock",
    # ---------- UBS ----------
    str_detect(Investor.Full.Name, regex("\\bubs\\b", ignore_case = TRUE)) &
      !str_detect(Investor.Full.Name, regex("icbc ubs|ubs hana", ignore_case = TRUE)) ~ "UBS",
    # ---------- Nomura ----------
    str_detect(Investor.Full.Name, regex("nomura", ignore_case = TRUE)) ~ "Nomura",
    # ---------- Franklin Templeton ----------
    str_detect(Investor.Full.Name, regex("franklin templeton|templeton", ignore_case = TRUE)) ~ "Franklin Templeton",
    # ---------- Invesco ----------
    str_detect(Investor.Full.Name, regex("invesco", ignore_case = TRUE)) ~ "Invesco",
    # ---------- HSBC ----------
    str_detect(Investor.Full.Name, regex("\\bhsbc\\b", ignore_case = TRUE)) ~ "HSBC",
    # ---------- BNP Paribas ----------
    str_detect(Investor.Full.Name, regex("bnp paribas", ignore_case = TRUE)) &
      !str_detect(Investor.Full.Name, regex("baroda bnp paribas", ignore_case = TRUE)) ~ "BNP Paribas",
    # ---------- Morgan Stanley ----------
    str_detect(Investor.Full.Name, regex("morgan stanley", ignore_case = TRUE)) &
      !str_detect(Investor.Full.Name, regex("mufg|mitsubishi ufj", ignore_case = TRUE)) ~ "Morgan Stanley",
    # ---------- Goldman Sachs ----------
    str_detect(Investor.Full.Name, regex("goldman sachs", ignore_case = TRUE)) ~ "Goldman Sachs",
    # ---------- State Street ----------
    str_detect(Investor.Full.Name, regex("state street", ignore_case = TRUE)) ~ "State Street",
    # ---------- JPMorgan ----------
    str_detect(Investor.Full.Name, regex("j\\.?p\\.?morgan|jpmorgan", ignore_case = TRUE)) ~ "JPMorgan",
    # ---------- Schroders ----------
    str_detect(Investor.Full.Name, regex("schroder|schroders", ignore_case = TRUE)) &
      !str_detect(Investor.Full.Name, regex("bank of communications schroder", ignore_case = TRUE)) ~ "Schroders",
    # ---------- Fidelity ----------
    str_detect(Investor.Full.Name, regex("fidelity", ignore_case = TRUE)) ~ "Fidelity",
    # ---------- Amundi ----------
    str_detect(Investor.Full.Name, regex("\\bamundi\\b", ignore_case = TRUE)) &
      !str_detect(Investor.Full.Name, regex("nh-amundi", ignore_case = TRUE)) ~ "Amundi",
    # ---------- Allianz / Allianz Global Investors ----------
    str_detect(Investor.Full.Name, regex("allianz", ignore_case = TRUE)) ~ "Allianz",
    # ---------- Neuberger Berman ----------
    str_detect(Investor.Full.Name, regex("neuberger berman", ignore_case = TRUE)) ~ "Neuberger Berman",
    # ---------- Columbia Threadneedle ----------
    str_detect(Investor.Full.Name, regex("columbia threadneedle|threadneedle", ignore_case = TRUE)) ~ "Columbia Threadneedle",
    # ---------- Aviva ----------
    str_detect(Investor.Full.Name, regex("aviva investors|\\baviva\\b", ignore_case = TRUE)) ~ "Aviva",
    # ---------- Legal & General ----------
    str_detect(Investor.Full.Name, regex("legal & general|legal and general", ignore_case = TRUE)) ~ "Legal & General",
    # ---------- RBC ----------
    str_detect(Investor.Full.Name, regex("\\brbc\\b|royal bank of canada", ignore_case = TRUE)) ~ "RBC",
    # ---------- Wells Fargo ----------
    str_detect(Investor.Full.Name, regex("wells fargo", ignore_case = TRUE)) ~ "Wells Fargo",
    # ---------- Vanguard ----------
    str_detect(Investor.Full.Name, regex("vanguard", ignore_case = TRUE)) ~ "Vanguard",
    # ---------- Sumitomo Mitsui ----------
    str_detect(Investor.Full.Name, regex("sumitomo mitsui", ignore_case = TRUE)) ~ "Sumitomo Mitsui",
    # ---------- Mitsubishi UFJ ----------
    str_detect(Investor.Full.Name, regex("mitsubishi ufj|\\bmufg\\b", ignore_case = TRUE)) &
      !str_detect(Investor.Full.Name, regex("morgan stanley", ignore_case = TRUE)) ~ "Mitsubishi UFJ",
    # ---------- Credit Suisse ----------
    str_detect(Investor.Full.Name, regex("credit suisse", ignore_case = TRUE)) &
      !str_detect(Investor.Full.Name, regex("icbc credit suisse", ignore_case = TRUE)) ~ "Credit Suisse",
    # ---------- First Sentier ----------
    str_detect(Investor.Full.Name, regex("first sentier", ignore_case = TRUE)) ~ "First Sentier",
    TRUE ~ Investor.Full.Name))
   
own <- own %>%
  group_by(investor_parent_clean, Company.Common.Name) %>%
  summarise(Investor.Shares.Held = sum(Investor.Shares.Held),
            Holdings.Pct.Of.Traded.Shares.Held = sum(Holdings.Pct.Of.Traded.Shares.Held),
            value_held = sum(value_held),
            Company.Common.Name = first(Company.Common.Name)) %>%
  arrange(Company.Common.Name)

write.csv(own, "sec_Owners.csv")
