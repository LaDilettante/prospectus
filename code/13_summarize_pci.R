rm(list = ls())
source("functions.R")
packs <- c("ggplot2", "plyr", "dplyr", "foreign")
f_install_and_load(packs)

# ---- Load data ----
load("../clean_data//d_prov.RData")
load("../clean_data/d_fdi10.RData")
load("../clean_data/d_fdi11.RData")
load("../clean_data/d_fdi12.RData")
load("../clean_data/d_fdi13.RData")

d_pci <- read.dta("../clean_data/pci_panel.dta")
d_pci_lab <- f_stata_to_df(d_pci)

# ---- Some constant ----
c_provinces_withfdi <- c("Ha Noi", "Hai Phong", "Da Nang", "HCMC", "Can Tho",
                         "Tay Ninh", "Long An", "BRVT", "Bac Ninh", "Binh Duong",
                         "Dong Nai", "Hai Duong", "Hung Yen", "Vinh Phuc")

# ---- graph ----
# FDI corruption by province
d_corr <- d_pci %>% filter(FDI == 1) %>%
  group_by(pci_id, treatment) %>% summarise(m = mean(reg_corrupt, na.rm=TRUE)) %>%
  group_by(pci_id) %>% mutate(d = c(NA, diff(m))) %>%
  filter(!is.na(d), d >= 0, pci_id %in% c_provinces_withfdi)


ggplot(data=d_prov) +
  geom_line(aes(year, h1_new)) + facet_wrap( ~ pci_id)

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