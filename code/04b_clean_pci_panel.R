rm(list = ls())
source("functions.R")
source("_function_multiplot.R")
source("_function_corstars.R")
source("_constants.R")

packs <- c("foreign", "Hmisc", "reshape2", "plyr", "dplyr",
           "ggplot2", "scales")
f_install_and_load(packs)

# ---- Load data ----
d_pci_raw <- read.dta("../clean_data/pci_panel.dta")
d_pci_lab <- f_stata_to_df(d_pci_raw)
d_pci_env <- f_stata_to_env(d_pci_raw)

# ---- Collapse to the province level ----

# Observational corruption
d_corr_obs <- d_pci_raw %>%
  mutate(bureaucratic_rents_agree = bureaucratic_rents == "Agree" | bureaucratic_rents == "Strongly agree",
         bribe_size = max(as.numeric(bribe_size), na.rm=T) - as.numeric(bribe_size) + 1,
         custom_corrupt = custom_corrupt == "Yes") %>%
  group_by(pci_id, year, FDI) %>%
  summarise_each(funs(m = mean(., na.rm=TRUE)),
                 bureaucratic_rents_agree, pctsale_foreign, bribe_size, custom_corrupt)
molten <- melt(d_corr_obs, id.vars = c("pci_id", "year", "FDI"))
molten$FDI <- revalue(factor(molten$FDI), replace = c("0"="DDI", "1"="FDI"))
d_corr_obs <- dcast(molten, pci_id + year ~ variable + FDI)

# Corruption measured via experiment
d_reg <- d_pci_raw %>%
  filter(restrict_all == 0, restrict == 0 | FDI == 0) %>%
  group_by(pci_id, FDI, treatment_reg) %>%
  summarise(m_reg = mean(reg_corrupt, na.rm=TRUE)) %>%
  group_by(pci_id) %>%
  mutate(reg_corrupt = c(NA, diff(m_reg))) %>%
  filter(treatment_reg == 1)
molten <- melt(d_reg, id.vars = c("pci_id", "FDI"),
               measure.vars = c("reg_corrupt"))
molten$FDI <- revalue(factor(molten$FDI), replace = c("0"="DDI", "1"="FDI"))
d_reg <- dcast(molten, pci_id ~ variable + FDI)

d_govcontract <- d_pci_raw %>%
  group_by(pci_id, year, FDI, treatment_govcontract) %>%
  summarise(m_govcontract = mean(govcontract_corrupt, na.rm=TRUE)) %>%
  group_by(pci_id, year) %>%
  mutate(govcontract_corrupt = c(NA, diff(m_govcontract))) %>%
  filter(treatment_govcontract == 1)
molten <- melt(d_govcontract, id.vars = c("pci_id", "year", "FDI"),
               measure.vars = c("govcontract_corrupt"))
molten$FDI <- revalue(factor(molten$FDI), replace = c("0"="DDI", "1"="FDI"))
d_govcontract <- dcast(molten, pci_id + year ~ variable + FDI)

# Merge
d_corr <- d_corr_obs %>%
  inner_join(d_reg, by=c("pci_id")) %>%
  inner_join(d_govcontract, by=c("pci_id", "year"))

# ---- Merge into main data ----

d_pci_raw <- d_pci_raw %>%
  inner_join(d_corr, by=c("pci_id", "year"))

# Import Stata label into R
for (varname in names(d_pci_raw)) {
  if (!is.null(d_pci_env[[varname]])) {
    label(d_pci_raw[ , varname]) <- d_pci_env[[varname]]
  }
}

label(d_pci_raw$reg_corrupt_DDI) <- "Fraction of firms engaging in registration corruption, DDI, province"
label(d_pci_raw$reg_corrupt_FDI) <- "Fraction of firms engaging in registration corruption, FDI, province"
label(d_pci_raw$govcontract_corrupt_FDI) <- "Fraction of firms engaging in procurement corruption, FDI, province-year"
label(d_pci_raw$bureaucratic_rents_agree_FDI) <- "Fraction of firms agreeing that provinces use inspections to extract fees, FDI, province-year"
label(d_pci_raw$bribe_size_FDI) <- "Average informal fees as pct of firms' revenue, FDI, province-year"
label(d_pci_raw$govcontract_corrupt_FDI) <- "Fraction of firms reporting paying bribe at port, FDI, province-year"

# ---- Save ----
saveRDS(d_pci_raw, file="../clean_data/pci_panel.RData")
saveRDS(d_corr, file="../clean_data/pci_province.RData")
