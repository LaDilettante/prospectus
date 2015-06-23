rm(list = ls())
source("functions.R")
source("_function_multiplot.R")
packs <- c("ggplot2", "reshape2", "plyr", "dplyr", "tidyr", "foreign", "scales", "lmerTest")
f_install_and_load(packs)

# ---- Constants ----
c_provinces_withfdi <- c("Ha Noi", "Hai Phong", "Da Nang", "HCMC", "Can Tho",
                         "Tay Ninh", "Long An", "BRVT", "Bac Ninh", "Binh Duong",
                         "Dong Nai", "Hai Duong", "Hung Yen", "Vinh Phuc")

# ---- Load data ----
d_pci_raw <- read.dta("../clean_data/pci_panel.dta")
d_pci_lab <- f_stata_to_df(d_pci_raw)
d_pci_env <- f_stata_to_env(d_pci_raw)

# ---- regression ----

d_prov <- d_pci_raw %>%
  mutate(bureaucratic_rents_agree = bureaucratic_rents == "Agree" | bureaucratic_rents == "Strongly agree") %>%
  group_by(pci_id, year, FDI) %>%
  summarise(bureaucratic_rents_agree = mean(bureaucratic_rents_agree, na.rm=TRUE),
            pctsale_foreign = mean(pctsale_foreign, na.rm=TRUE))
molten <- melt(d_prov, id.vars = c("pci_id", "year", "FDI"))
molten$FDI <- revalue(factor(molten$FDI), replace = c("0"="DDI", "1"="FDI"))
d_prov <- dcast(molten, pci_id + year ~ variable + FDI)
d_prov <- d_prov %>%
  mutate(bureaucratic_rents_agree_diff = bureaucratic_rents_agree_FDI - bureaucratic_rents_agree_DDI)

d_prov2 <- d_pci_raw %>%
  mutate(bureaucratic_rents_agree = bureaucratic_rents == "Agree" | bureaucratic_rents == "Strongly agree") %>%
  group_by(pci_id, FDI) %>%
  summarise(bureaucratic_rents_agree = mean(bureaucratic_rents_agree, na.rm=TRUE),
            pctsale_foreign = mean(pctsale_foreign, na.rm=TRUE))
molten <- melt(d_prov2, id.vars = c("pci_id", "FDI"))
molten$FDI <- revalue(factor(molten$FDI), replace = c("0"="DDI", "1"="FDI"))
d_prov2 <- dcast(molten, pci_id ~ variable + FDI)

d_corr <- d_pci_raw %>%
  group_by(pci_id, year, FDI, treatment_reg) %>%
  summarise(m = mean(reg_corrupt, na.rm=TRUE)) %>%
  group_by(pci_id) %>% mutate(reg_corrupt = c(NA, diff(m))) %>%
  filter(treatment_reg == 1, pci_id %in% c_provinces_withfdi)
molten <- melt(d_corr, id.vars = c("pci_id", "year", "FDI"), measure.vars = c("reg_corrupt"))
molten$FDI <- revalue(factor(molten$FDI), replace = c("0"="DDI", "1"="FDI"))
d_corr <- dcast(molten, pci_id + year ~ variable + FDI)

ggplot(filter(d_prov, pci_id %in% c_provinces_withfdi),
       aes(bureaucratic_rents_agree_FDI, pctsale_foreign_DDI)) +
  geom_text(aes(label=paste(pci_id, year)), position="jitter") +
  geom_line(aes(group=pci_id)) + scale_x_continuous(labels=percent) +
  expand_limits(x = c(0, 0.6)) +
  labs(y = "% of sale from DDI firms to FDI firms",
       x = "% of FDI firms agree that provinces use inspection to get bribes")


ggplot(filter(d_prov2, pci_id %in% c_provinces_withfdi),
       aes(bureaucratic_rents_agree_FDI, pctsale_foreign_DDI)) +
  geom_text(aes(label=paste(pci_id)), position="jitter") +
  geom_smooth(method="lm")

ggplot(data=filter(d_prov, pci_id %in% c_provinces_withfdi),
       aes(x=factor(year), y=bureaucratic_rents_agree_FDI)) +
  geom_line(aes(group=pci_id)) +
  geom_text(aes(label=pci_id), data=filter(d_prov, pci_id %in% c_provinces_withfdi,
                                           year==2010))

d_pci <- d_pci_raw %>%
  filter(FDI == 0) %>%
  mutate(pctsale_foreign_dich = pctsale_foreign > 0) %>%
  inner_join(d_prov, by=c("pci_id","year")) %>%
  inner_join(d_corr, by=c("pci_id", "year"))
d_pci_highfdi <- filter(d_pci, pci_id %in% c_provinces_withfdi)

ggplot(data=d_pci_highfdi,
       aes(x=bureaucratic_rents_agree_FDI, y=pctsale_foreign)) +
  geom_point() +
  geom_smooth(method="lm")

m1 <- lmer(pctsale_foreign ~ equity_thisyear + labor_thisyear +
             ownership_type + isic_rev4_2digitcode +
             bureaucratic_rents_agree_FDI +
             log(active_enterprises_stacked / pop_stacked * 1000) + log(pop_stacked) + log(gdp) + log(distance_hnhcmc + 0.01) +
            (1 | pci_id) + (1 | year),
     data=d_pci_highfdi)
summary(m1)

m1nocorr <- lmer(pctsale_foreign ~ equity_thisyear + labor_thisyear +
                   ownership_type + isic_rev4_2digitcode +
                  log(pop_stacked) + log(gdp) + log(distance_hnhcmc + 0.01) +
                   (1 | pci_id) + (1 | year),
                 data=d_pci_highfdi)
d <- m1nocorr@frame
d$res <- residuals(m1nocorr)
d <- inner_join(d, d_prov[d_prov$pci_id %in% c_provinces_withfdi,
                          c("pci_id", "year", "bureaucratic_rents_agree_FDI")], by=c("pci_id", "year"))
d <- d %>% group_by(pci_id, year) %>%
  summarise(res = mean(res),
         bureaucratic_rents_agree_FDI = mean(bureaucratic_rents_agree_FDI))

ggplot(d, aes(bureaucratic_rents_agree_FDI, res)) +
  geom_text(aes(label = paste(pci_id, year))) + scale_x_continuous(labels=percent) +
  expand_limits(x = c(0, 0.6)) +
  labs(y = "Residual of pctsale_foreign in model without corruption",
       x = "% of FDI firms agree that provinces use inspection to get bribes")


m12 <- lmer(pctsale_foreign ~ equity_thisyear + labor_thisyear +
              ownership_type + isic_rev4_2digitcode +
              reg_corrupt_agg_FDI + log(pop_stacked) + log(gdp) + log(distance_hnhcmc + 0.01) +
              (1 | pci_id) + (1 | year),
            data=d_pci_highfdi)
summary(m12)

m2 <- lmer(pctsale_foreign ~ equity_thisyear + labor_thisyear +
             ownership_type + isic_rev4_2digitcode +
             bureaucratic_rents_agree_diff + log(pop_stacked) + log(gdp) + log(distance_hnhcmc + 0.01) +
             (1 | pci_id) + (1 | year),
           data=filter(d_pci, pci_id %in% c_provinces_withfdi))
summary(m2)

m3 <- lmer(pctsale_foreign ~ equity_thisyear + labor_thisyear +
             ownership_type + isic_rev4_2digitcode +
             bureaucratic_rents_agree_DDI + log(pop_stacked) + log(gdp) + log(distance_hnhcmc + 0.01) +
             (1 | pci_id) + (1 | year),
           data=filter(d_pci, pci_id %in% c_provinces_withfdi))
summary(m3)


