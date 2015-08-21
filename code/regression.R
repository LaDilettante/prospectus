rm(list = ls())
source("functions.R")
source("_function_multiplot.R")
source("_constants.R")

packs <- c("Hmisc", "reshape2", "plyr", "dplyr",
           "ggplot2", "scales",
           "lme4", "stargazer")
f_install_and_load(packs)

# ---- Load data ----
d_ddi <- readRDS("../clean_data/pci_panel.RData") %>%
  filter(FDI == 0)
d_ddi_highfdi <- d_ddi %>%
  filter(pci_id %in% c_provinces_withfdi)

# ---- regression ----

m_bureaucratic <- lmer(pctsale_foreign ~ as.numeric(equity_thisyear) + as.numeric(labor_thisyear) +
  ownership_type + isic_rev4_2digitcode +
  bureaucratic_rents_agree_FDI +
  log(active_enterprises_stacked / pop_stacked * 1000) + log(pop_stacked) + log(gdp) + log(distance_hnhcmc + 0.01) +
  (1 | pci_id) + (1 | year),
  data=d_ddi_highfdi)

m_custom <- lmer(pctsale_foreign ~ as.numeric(equity_thisyear) + as.numeric(labor_thisyear) +
  ownership_type + isic_rev4_2digitcode +
  custom_corrupt_FDI +
  log(active_enterprises_stacked / pop_stacked * 1000) + log(pop_stacked) + log(gdp) + log(distance_hnhcmc + 0.01) +
  (1 | pci_id) + (1 | year),
  data=d_ddi_highfdi)

m_bribesize <- lmer(pctsale_foreign ~ as.numeric(equity_thisyear) + as.numeric(labor_thisyear) +
  ownership_type + isic_rev4_2digitcode +
  bribe_size_FDI +
  log(active_enterprises_stacked / pop_stacked * 1000) + log(pop_stacked) + log(gdp) + log(distance_hnhcmc + 0.01) +
  (1 | pci_id) + (1 | year),
  data=d_ddi_highfdi)

m_reg <- lmer(pctsale_foreign ~ as.numeric(equity_thisyear) + as.numeric(labor_thisyear) +
  ownership_type + isic_rev4_2digitcode +
  reg_corrupt_FDI +
  log(active_enterprises_stacked / pop_stacked * 1000) + log(pop_stacked) + log(gdp) + log(distance_hnhcmc + 0.01) +
  (1 | pci_id) + (1 | year),
  data=d_ddi_highfdi)

m_govcontract <- lmer(pctsale_foreign ~ as.numeric(equity_thisyear) + as.numeric(labor_thisyear) +
  ownership_type + isic_rev4_2digitcode +
  govcontract_corrupt_FDI +
  log(active_enterprises_stacked / pop_stacked * 1000) + log(pop_stacked) + log(gdp) + log(distance_hnhcmc + 0.01) +
  (1 | pci_id) + (1 | year),
  data=d_ddi_highfdi)

stargazer(m_bureaucratic, m_custom, m_bribesize, m_reg, m_govcontract,
          model.names = T,
          omit=c("^isic", "^ownership"),
          omit.labels=c("Industry fixed effect (ISIC 2-digit)", "Ownership type fixed effect"),
          dep.var.labels = c("\\% of sale to foreign firms"),
          covariate.labels=c("equity size", "labor size",
                             "bureaucratic corruption", "custom corruption", "bribe size", "registration corruption", "procurement corruption",
                             "log(\\# of domestic firms per 1000 people)",
                             "log population", "log GDP", "log distance to HN / HCMC"),
          keep.stat = c('n', 'll'),
          title = "Relationship between FDI corruption and spill-over effect. Hierarchical model with varying intercepts for provinces and survey years",
          label = "tab:pci_corruption",
          no.space=T, font.size = "small",
          float = T, float.env = "sidewaystable",
          out="../table/pci_corruption.tex")

# ---- Graph the residual ----

m_nocorr <- lmer(pctsale_foreign ~ as.numeric(equity_thisyear) + as.numeric(labor_thisyear) +
  ownership_type + isic_rev4_2digitcode +
  log(active_enterprises_stacked / pop_stacked * 1000) + log(pop_stacked) + log(gdp) + log(distance_hnhcmc + 0.01) +
  (1 | pci_id) + (1 | year),
  data=d_ddi_highfdi)

d <- m_nocorr@frame
d$res <- residuals(m_nocorr)
d_prov <- readRDS("../clean_data/pci_province.RData")

pd <- d %>%
  inner_join(d_prov[d_prov$pci_id %in% c_provinces_withfdi, ],
            by=c("pci_id", "year")) %>%
  group_by(pci_id, year) %>%
  summarise(res = mean(res),
            bureaucratic_rents = mean(bureaucratic_rents_agree_FDI),
            custom_corrupt = mean(custom_corrupt_FDI))

ggplot(pd, aes(bureaucratic_rents, res)) +
  geom_text(aes(label = paste(pci_id, year))) + scale_x_continuous(labels=percent) +
  expand_limits(x = c(0, 0.6)) +
  geom_smooth(method="lm") +
  labs(y = "Residual of pctsale_foreign in model without corruption",
       x = "% of FDI firms agree that provinces use inspection to get bribes")
ggsave(filename="../figure/bureaucratic_rents_residual.pdf")

ggplot(pd, aes(custom_corrupt, res)) +
  geom_text(aes(label = paste(pci_id, year))) + scale_x_continuous(labels=percent) +
  geom_smooth(method="lm") +
  expand_limits(x = c(0.1, 0.9)) +
  labs(y = "Residual of pctsale_foreign in model without corruption",
       x = "% of FDI firms report paying bribe at port")
ggsave(filename="../figure/custom_corrupt_residual.pdf")