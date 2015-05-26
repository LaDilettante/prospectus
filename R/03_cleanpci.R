rm(list=ls())
source("functions.R")
packs <- c("foreign", "plyr", "dplyr")
f_install_and_load(packs)

d_prov <- read.dta("../data/private//PCI//pci_province_paneldata.dta")
d_prov_lab <- f_stata_to_df(d_prov)
d_prov_env <- f_stata_to_env(d_prov)

# Save
save(d_prov, d_prov_lab, d_prov_env, file="../clean_data/vietnam_province.RData")