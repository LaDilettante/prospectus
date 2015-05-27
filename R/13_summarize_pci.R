rm(list=ls())
source("functions.R")
packs <- c("ggplot2", "plyr", "dplyr")
f_install_and_load(packs)

# ---- Load data ----
load("../clean_data//vietnam_province.RData")

# ---- Some constant ----
c_provinces_withfdi <- c("Ha Noi", "Hai Phong", "Da Nang", "HCMC", "Can Tho",
                         "Tay Ninh", "Long An", "BRVT", "Bac Ninh", "Binh Duong",
                         "Dong Nai", "Hai Duong", "Hung Yen", "Vinh Phuc")

# ---- graph ----
ggplot(data=d_prov) +
  geom_line(aes(year, h1_new)) + facet_wrap( ~ pci_id)


summary(d_prov$h27_new)

d_prov %>% filter(!is.na(h27_new)) %>% select(pci_id, h27_new)

ggplot(data=d_prov) + geom_boxplot(aes(x=factor(0), y=h27_new))

pdf('../figure/FDI_bias.pdf', w=11, h=8.5)
ggplot(data=d_prov, aes(h1_new, h27_new)) +
  geom_point() +
  geom_text(data=filter(d_prov, pci_id %in% c_provinces_withfdi),
            aes(label=pci_id, col='provinces with\nsubstantial FDI')) +
  geom_smooth(data=filter(d_prov, pci_id %in% c_provinces_withfdi),
              aes(col='provinces with\nsubstantial FDI'),
              method='lm', se=FALSE) +
  geom_text(aes(label=pci_id, col='all provinces'), alpha=0.2) +
  geom_smooth(aes(col='all provinces'), method='lm', se=FALSE) +
  annotate("text", x = 0.7, y = 0.6,
                label = f_lm_eqn(lm(h27_new ~ h1_new, filter(d_prov, pci_id %in% c_provinces_withfdi))),
            , parse=TRUE) +
  labs(x=d_prov_env[['h1_new']], y=d_prov_env[['h27_new']]) +
  guides(col=guide_legend(title="provincial group"))
dev.off()

ggplot(data=filter(d_prov, pci_id %in% c_provinces_withfdi),
       aes(h1_new, h27_new)) +
  geom_point() + geom_text(aes(label=pci_id)) +
  geom_smooth(method='lm', se=TRUE) +
  labs(x=d_prov_env[['h1_new']], y=d_prov_env[['h27_new']])