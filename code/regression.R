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

d_corr <- d_pci_raw %>%
  group_by(pci_id, year, FDI, treatment) %>%
  summarise(m = mean(reg_corrupt, na.rm=TRUE)) %>%
  group_by(pci_id) %>% mutate(reg_corrupt_agg = c(NA, diff(m))) %>%
  filter(treatment == 1, pci_id %in% c_provinces_withfdi) %>%
  select(-m, -treatment)
molten <- melt(d_corr, id.vars = c("pci_id", "year", "FDI"))
molten$FDI <- revalue(factor(molten$FDI), replace = c("0"="DDI", "1"="FDI"))
d_corr <- dcast(molten, pci_id + year ~ variable + FDI)



ggplot(filter(d_prov, pci_id %in% c_provinces_withfdi),
       aes(bureaucratic_rents_agree_fdi, pctsale_foreign_prov)) +
  geom_point() +
  geom_text(aes(label=paste(pci_id, year)))

ggplot(data=filter(d_prov, pci_id %in% c_provinces_withfdi),
       aes(x=factor(year), y=bureaucratic_rents_agree_fdi)) +
  geom_line(aes(group=pci_id)) +
  geom_text(aes(label=pci_id), data=filter(d_prov, pci_id %in% c_provinces_withfdi,
                                           year==2010))

d_pci <- d_pci_raw %>%
  filter(FDI == 0) %>%
  mutate(pctsale_foreign_dich = pctsale_foreign > 0) %>%
  inner_join(d_prov, by=c("pci_id","year")) %>%
  inner_join(d_corr, by=c("pci_id", "year"))
d_pci_highfdi <- filter(d_pci, pci_id %in% c_provinces_withfdi)

ggplot(data=d_pci_highfdi) +
  geom_text(aes(x=bureaucratic_rents_agree_FDI, y=pctsale_foreign_DDI,
                 label=paste(pci_id, year)))

ggplot(data=d_pci_highfdi) +
  geom_text(aes(x=bureaucratic_rents_agree_diff, y=pctsale_foreign_DDI,
                label=paste(pci_id, year)))

m1 <- lmer(pctsale_foreign ~ equity_thisyear + labor_thisyear +
             ownership_type + isic_rev4_2digitcode +
             bureaucratic_rents_agree_FDI + pop_stacked + gdp + distance_hnhcmc +
            (1 | pci_id) + (1 | year),
     data=d_pci_highfdi)
summary(m1)

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


