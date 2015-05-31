rm(list=ls())
source("functions.R")
packs <- c("foreign", "plyr", "dplyr")
f_install_and_load(packs)

# ---- Provincial panel ----
d_prov <- read.dta("../data/private//PCI//PCI_Province/pci_province_paneldata.dta")
d_prov_lab <- f_stata_to_df(d_prov)
d_prov_env <- f_stata_to_env(d_prov)

save(d_prov, d_prov_lab, d_prov_env, file="../clean_data/d_prov.RData")

# ---- FDI ----
d_fdi10 <- read.dta("../data/private/PCI/PCI_FDI/data/PCI_FDI_2010.dta")
d_fdi10_lab <- f_stata_to_df(d_fdi10)
d_fdi10_env <- f_stata_to_env(d_fdi10)
save(d_fdi10, d_fdi10_lab, d_fdi10_env, file="../clean_data/d_fdi10.RData")

d_fdi11 <- read.dta("../data/private/PCI/PCI_FDI/data/PCI_FDI_2011.dta")
d_fdi11_lab <- f_stata_to_df(d_fdi11)
d_fdi11_env <- f_stata_to_env(d_fdi11)
save(d_fdi11, d_fdi11_lab, d_fdi11_env, file="../clean_data/d_fdi11.RData")

d_fdi12 <- read.dta("../data/private/PCI/PCI_FDI/data/PCI_FDI_2012.dta")
d_fdi12_lab <- f_stata_to_df(d_fdi12)
d_fdi12_env <- f_stata_to_env(d_fdi12)
save(d_fdi12, d_fdi12_lab, d_fdi12_env, file="../clean_data/d_fdi12.RData")

d_fdi13 <- read.dta("../data/private/PCI/PCI_FDI/data/PCI_FDI_2013.dta")
d_fdi13_lab <- f_stata_to_df(d_fdi13)
d_fdi13_env <- f_stata_to_env(d_fdi13)
save(d_fdi13, d_fdi13_lab, d_fdi13_env, file="../clean_data/d_fdi13.RData")