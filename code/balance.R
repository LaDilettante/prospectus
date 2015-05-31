rm(list = ls())
source("functions.R")
packs <- c("foreign", "plyr", "dplyr", "ggplot2", "Matching")
f_install_and_load(packs)

# ---- Load data ----
d_pci <- read.dta("../clean_data/pci_panel.dta")
d_pci_lab <- f_stata_to_df(d_pci)
d_pci_env <- f_stata_to_env(d_pci)

# ---- Check balance ----

f_create_balancetable()

m_bal <- MatchBalance(treatment ~ equity_est, data=d_pci)$BeforeMatching
f_create_balancetable(df = d_pci, balance_vars = c("equity_est"), bal_result = m_bal)
