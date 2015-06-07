rm(list = ls())
source("functions.R")
souce("_function_multiplot.R")
packs <- c("reshape2", "ggplot2", "plyr", "dplyr", "foreign", "scales")
f_install_and_load(packs)

# ---- Load data ----
d_pci_raw <- read.dta("../clean_data/pci_panel.dta")
d_pci_lab <- f_stata_to_df(d_pci_raw)
d_pci_env <- f_stata_to_env(d_pci_raw)

# ---- Some constant ----
c_provinces_withfdi <- c("Ha Noi", "Hai Phong", "Da Nang", "HCMC", "Can Tho",
                         "Tay Ninh", "Long An", "BRVT", "Bac Ninh", "Binh Duong",
                         "Dong Nai", "Hai Duong", "Hung Yen", "Vinh Phuc")

# ---- graph ----
# FDI corruption by province
d_pci <- d_pci_raw %>%
  mutate(bureaucratic_rents_agree = bureaucratic_rents == "Strongly agree" | bureaucratic_rents == "Agree",
         fdi_favoritism_agree = fdi_favoritism == "Strongly agree" | fdi_favoritism == "Agree",
         psd_attitude_positive = psd_attitude == "Positive" | psd_attitude == "Somewhat positive",
         service_delivered_dich = service_delivered == "Always" | service_delivered == "Usually")

d_corr <- d_pci %>% filter(FDI == 1) %>%
  group_by(pci_id, treatment) %>% summarise(m = mean(reg_corrupt, na.rm=TRUE)) %>%
  group_by(pci_id) %>% mutate(d = c(NA, diff(m))) %>%
  filter(!is.na(d), d >= 0, pci_id %in% c_provinces_withfdi)

d_prov <- d_pci %>% group_by(pci_id, FDI) %>%
  summarise(fdi_favoritism = mean(fdi_favoritism_agree, na.rm=TRUE),
            bureaucratic_rents = mean(bureaucratic_rents_agree, na.rm=TRUE),
            psd_attitude = mean(psd_attitude_positive, na.rm=TRUE),
            inspections = mean(inspections, na.rm=TRUE),
            service_delivered = mean(service_delivered_dich, na.rm=TRUE)) %>%
  mutate(rent_diff = c(NA, diff(bureaucratic_rents)),
         inspections_diff = c(NA, diff(inspections)),
         service_delivered_diff = c(NA, diff(service_delivered)))

# Melt and cast
molten <- melt(d_prov, id.vars = c("pci_id", "FDI"))
molten$FDI <- as.factor(molten$FDI)
molten$FDI <- revalue(molten$FDI, c("1"="FDI", "0"="DDI"))
pd_prov <- dcast(molten, pci_id ~ variable + FDI)
pd_prov_withfdi <- filter(pd_prov, pci_id %in% c_provinces_withfdi)

# Contour
tmp <- melt(pd_prov_inter$z)
tmp$x <- pd_prov_inter$x[tmp$Var1]
tmp$y <- pd_prov_inter$y[tmp$Var2]
tmp$z <- tmp$value

image(pd_prov_inter)
ggplot() + geom_tile(aes(x, y, fill=z), data=tmp) +
  scale_fill_gradient(low="white", high="orange")

# --- With contour ----
ggplot(data=pd_prov, aes(bureaucratic_rents_FDI, fdi_favoritism_DDI)) +
  geom_point() +
  geom_tile(aes(x, y, fill=z), data=tmp) + scale_fill_gradient(low="white", high="orange") +
  geom_text(aes(label=pci_id, col='all provinces'), alpha=0.2) +
  geom_text(aes(label=pci_id, col='provinces with\nsubstantial FDI'),
            pd_prov_withfdi) +
  geom_smooth(aes(col='all provinces'),
              pd_prov, method='lm', se=FALSE) +
  geom_smooth(aes(col='provinces with\nsubstantial FDI'),
              pd_prov_withfdi, method='lm', se=FALSE) +
  annotate("text", x = 0.5, y = 0.5, parse=TRUE, col="red",
           label = f_lm_eqn(lm(fdi_favoritism_DDI ~ bureaucratic_rents_FDI, pd_prov))) +
  annotate("text", x = 0.6, y = 0.6, parse=TRUE, col="blue",
           label = f_lm_eqn(lm(fdi_favoritism_DDI ~ bureaucratic_rents_FDI, pd_prov_withfdi)))

# ---- Without contour ----
ggplot(data=pd_prov, aes(bureaucratic_rents_FDI, fdi_favoritism_DDI)) +
  geom_text(aes(label=pci_id, col='all provinces'), alpha=0.2) +
  geom_text(aes(label=pci_id, col='provinces with\nsubstantial FDI'),
            pd_prov_withfdi) +
  geom_smooth(aes(col='all provinces'),
              pd_prov, method='lm', se=T) +
  geom_smooth(aes(col='provinces with\nsubstantial FDI'),
              pd_prov_withfdi, method='lm', se=T) +
  annotate("text", x = 0.65, y = 0.5, parse=TRUE, col="red",
           label = f_lm_eqn(lm(fdi_favoritism_DDI ~ bureaucratic_rents_FDI, pd_prov))) +
  annotate("text", x = 0.65, y = 0.52, parse=TRUE, col="turquoise4",
           label = f_lm_eqn(lm(fdi_favoritism_DDI ~ bureaucratic_rents_FDI, pd_prov_withfdi))) +
  scale_x_continuous(labels=percent) + scale_y_continuous(labels=percent) +
  labs(y="% of domestic firms agree that officials favor FIEs",
       x="% of FIEs report officials using regulation to extract rents")

ggplot(mapping=aes(fdi_favoritism_DDI, rent_diff_FDI)) +
  geom_text(aes(label=pci_id, col='all provinces'), pd_prov, alpha=0.2) +
  geom_text(aes(label=pci_id, col='provinces with\nsubstantial FDI'),
            pd_prov_withfdi) +
  geom_smooth(aes(col='all provinces'),
              pd_prov, method='lm', se=T) +
  geom_smooth(aes(col='provinces with\nsubstantial FDI'),
              pd_prov_withfdi, method='lm', se=T) +
  annotate("text", x = 0.65, y = 0.5, parse=TRUE, col="red",
           label = f_lm_eqn(lm(fdi_favoritism_DDI ~ bureaucratic_rents_FDI, pd_prov))) +
  annotate("text", x = 0.65, y = 0.52, parse=TRUE, col="turquoise4",
           label = f_lm_eqn(lm(fdi_favoritism_DDI ~ bureaucratic_rents_FDI, pd_prov_withfdi))) +
  scale_x_continuous(labels=percent) + scale_y_continuous(labels=percent) +
  labs(y="% of domestic firms agree that officials favor FIEs",
       x="% of FIEs report officials using regulation to extract rents")


# ---- fdi_favoritism ~ psd_attitude ----
ggplot(data=pd_prov, aes(fdi_favoritism_DDI, psd_attitude_DDI)) +
  geom_text(aes(label=pci_id, col='all provinces'), alpha=0.2) +
  geom_text(aes(label=pci_id, col='provinces with\nsubstantial FDI'),
            pd_prov_withfdi) +
  geom_smooth(aes(col='all provinces'),
              pd_prov, method='lm', se=FALSE) +
  geom_smooth(aes(col='provinces with\nsubstantial FDI'),
              pd_prov_withfdi, method='lm', se=FALSE) +
  annotate("text", x = 0.5, y = 0.5, parse=TRUE, col="red",
           label = f_lm_eqn(lm(psd_attitude_DDI ~ fdi_favoritism_DDI, pd_prov))) +
  annotate("text", x = 0.6, y = 0.6, parse=TRUE, col="blue",
           label = f_lm_eqn(lm(psd_attitude_DDI ~ fdi_favoritism_DDI, pd_prov_withfdi)))

# ---- Inspection ----
ggplot(mapping=aes(inspections_diff_FDI, rent_diff_FDI)) +
  geom_text(aes(label=pci_id, col='all provinces'), pd_prov, alpha=0.2) +
  geom_text(aes(label=pci_id, col='provinces with\nsubstantial FDI'),
            pd_prov_withfdi)

ggplot(mapping=aes(inspections_FDI, inspections_DDI)) +
  geom_text(aes(label=pci_id, col='provinces with\nsubstantial FDI'),
            pd_prov_withfdi) +
  coord_equal() + expand_limits(x=1, y=1)

# ---- fdi_favoritism driving everything else ----
p = ggplot(pd_prov_withfdi, aes(x=fdi_favoritism_DDI, label=pci_id)) + labs(x="% domestic firms agree that officials favor FIEs") +
  scale_x_continuous(labels=percent)
p + geom_text(aes(y=inspections_FDI))
p + geom_text(aes(y=inspections_DDI))
p + geom_text(aes(y=inspections_diff_FDI)) + geom_smooth(aes(y=inspections_diff_FDI))
p + geom_text(aes(y=bureaucratic_rents_FDI)) + geom_smooth(aes(y=bureaucratic_rents_FDI))
p + geom_text(aes(y=bureaucratic_rents_DDI))
p + geom_text(aes(y=rent_diff_FDI)) + geom_smooth(aes(y=rent_diff_FDI))
p + geom_text(aes(y=service_delivered_FDI))
p + geom_text(aes(y=service_delivered_FDI), data=pd_prov)
p + geom_text(aes(y=service_delivered_diff_FDI)) + geom_smooth(aes(y=service_delivered_diff_FDI))
p + geom_text(aes(y=psd_attitude_DDI, col="all"), data = pd_prov) +
  geom_text(aes(y=psd_attitude_DDI, col="fdi rich"))


ggplot(pd_prov_withfdi, aes(bureaucratic_rents_DDI, bureaucratic_rents_FDI)) +
  geom_text(aes(label=pci_id)) + coord_equa

p1 <- p + geom_text(aes(y=inspections_diff_FDI)) + geom_smooth(aes(y=inspections_diff_FDI)) +
  labs(y="diff in # of inspections, FIEs vs domestic")
p2 <- p + geom_text(aes(y=rent_diff_FDI)) + geom_smooth(aes(y=rent_diff_FDI)) +
  scale_y_continuous(labels=percent) +
  labs(y="diff in % agree: 'regulation is to extract fee', FIEs vs domestic")

multiplot(p1, p2)

ggplot(pd_prov_withfdi, aes(rent_diff_FDI, psd_attitude_DDI)) +
  geom_text(aes(label=pci_id))

p2 = ggplot(pd_prov, aes(x=fdi_favoritism_DDI, label=pci_id))
p2 + geom_text(aes(y=inspections_diff_FDI)) + geom_smooth(aes(y=inspections_diff_FDI))


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