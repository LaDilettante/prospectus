rm(list = ls())
source("functions.R")
source("_function_corstars.R")
source("_constants.R")

packs <- c("plyr", "dplyr", "foreign",
           "ggplot2", "scales", "xtable")
f_install_and_load(packs)

# ---- Load data ----

d_pci_raw <- read.dta("../clean_data/pci_panel.dta")
d_pci_lab <- f_stata_to_df(d_pci_raw)

d_prov <- readRDS("../clean_data/pci_province.RData")

# --- Correlation between corruption measure ----
d_prov_diff <- d_prov %>%
  mutate_each(funs( c(NA, diff(.)) ), -pci_id, -year)

print(xtable(
  corstars(d_prov_diff[ , grepl("^(?!pctsale|reg|custom).*DDI$", names(d_prov_diff), perl=T)]),
           caption = "Correlation of year-on-year change across corruption measures, DDI",
           label = "tab:cor_corrupt_ddi"),
      file="../table/cor_corrupt_ddi.tex")

print(xtable(
  corstars(d_prov_diff[d_prov_diff$pci_id %in% c_provinces_withfdi,
                       grepl("^(?!pctsale|reg).*FDI$", names(d_prov_diff), perl=T)]),
           caption = "Correlation of year-on-year change across corruption measures, FDI",
           label = "tab:cor_corrupt_fdi"),
      file="../table/cor_corrupt_fdi.tex")

hist.data.frame(select(d_corr, -pci_id, -year, -starts_with("pctsale")),
                rugs = TRUE)

# ---- graph ----
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

d_corr_highfdi <- filter(d_corr, pci_id %in% c_provinces_withfdi)

ggplot(filter(d_corr, pci_id %in% c_provinces_withfdi),
       aes(reg_corrupt_DDI, reg_corrupt_FDI)) +
  geom_text(aes(label=paste(pci_id, year)))

ggplot(filter(d_corr, pci_id %in% c_provinces_withfdi),
       aes(govcontract_corrupt_FDI, reg_corrupt_FDI)) +
  geom_text(aes(label=paste(pci_id, year)))

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