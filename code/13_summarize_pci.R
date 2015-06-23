rm(list = ls())
source("functions.R")
packs <- c("plyr", "dplyr", "foreign",
           "ggplot2", "scales")
f_install_and_load(packs)

# ---- Load data ----

d_pci_raw <- read.dta("../clean_data/pci_panel.dta")
d_pci_lab <- f_stata_to_df(d_pci_raw)

# ---- Some constant ----
c_provinces_withfdi <- c("Ha Noi", "Hai Phong", "Da Nang", "HCMC", "Can Tho",
                         "Tay Ninh", "Long An", "BRVT", "Bac Ninh", "Binh Duong",
                         "Dong Nai", "Hai Duong", "Hung Yen", "Vinh Phuc")

# ---- graph ----
# reg_corruption by province
d_reg <- d_pci_raw %>%
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
  group_by(pci_id, FDI, treatment_govcontract) %>%
  summarise(m_govcontract = mean(govcontract_corrupt, na.rm=TRUE)) %>%
  group_by(pci_id) %>%
  mutate(govcontract_corrupt = c(NA, diff(m_govcontract))) %>%
  filter(treatment_govcontract == 1)
molten <- melt(d_govcontract, id.vars = c("pci_id", "FDI"),
               measure.vars = c("govcontract_corrupt"))
molten$FDI <- revalue(factor(molten$FDI), replace = c("0"="DDI", "1"="FDI"))
d_govcontract <- dcast(molten, pci_id ~ variable + FDI)

d_corr <- inner_join(d_reg, d_govcontract, by=c("pci_id"))
d_corr_highfdi <- filter(d_corr, pci_id %in% c_provinces_withfdi)

ggplot(filter(d_corr, pci_id %in% c_provinces_withfdi),
       aes(reg_corrupt_DDI, reg_corrupt_FDI)) +
  geom_text(aes(label=paste(pci_id, year)))

ggplot(filter(d_corr, pci_id %in% c_provinces_withfdi),
       aes(govcontract_corrupt_FDI, reg_corrupt_FDI)) +
  geom_text(aes(label=paste(pci_id, year)))


#

pdf('../figure/FDI_bias.pdf', w=12, h=7)
pd_fdi_bias <- filter(d_prov, year==2009)
pd_fdi_bias <- left_join(pd_fdi_bias, d_corr, by="pci_id")
ggplot(data=pd_fdi_bias, aes(h1_new, h27_new)) +
  geom_text(data=filter(pd_fdi_bias, pci_id %in% c_provinces_withfdi),
            aes(label=pci_id, col='provinces with\nsubstantial FDI')) +
  geom_smooth(data=filter(pd_fdi_bias, pci_id %in% c_provinces_withfdi),
              aes(col='provinces with\nsubstantial FDI'),
              method='lm', se=FALSE) +
  geom_text(aes(label=pci_id, col='all provinces'), alpha=0.2) +
  geom_smooth(aes(col='all provinces'), method='lm', se=FALSE) +
  annotate("text", x = 0.6, y = 0.6, col="lightblue",
                label = f_lm_eqn(lm(h27_new ~ h1_new, filter(pd_fdi_bias, pci_id %in% c_provinces_withfdi))),
            , parse=TRUE) +
  geom_point(data=pd_fdi_bias, aes(size=d)) +
  labs(x=d_prov_env[['h1_new']], y=d_prov_env[['h27_new']], title="Year 2009") +
  guides(col=guide_legend(title="provincial group"), size=guide_legend(title="proportion engaged in corruption\nduring registration"))
dev.off()

# ---- list experiment ----
summary(d_fdi10$corruption)

ddply(d_fdi10, .(form, as.character(pci_id)), summarise, mean(c6, na.rm=TRUE))
ddply(d_fdi10, .(form), summarise, mean(c6, na.rm=TRUE))

table(d_fdi11[ , "pci_id"])
table(d_fdi13[ , "province"])

ddply(d_fdi13, .(province, form), summarise, mean(c6, na.rm = TRUE))

table(d_fdi13[ , "province"])

margin.table(d_fdi13[ , c("province", "form")])
xtabs(~ province + form, data=d_fdi13)


d_fdi10 %>%
  filter(a1 > 2000) %>%
  group_by(pci_id, form) %>%
  summarise(m = mean(c6, na.rm = TRUE)) %>%
  group_by(pci_id) %>%
  mutate(diff = c(NA, diff(m))) %>%
  print.data.frame()


a1 > 2000