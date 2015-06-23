rm(list = ls())
source("functions.R")
source("_function_multiplot.R")
packs <- c("reshape2", "plyr", "dplyr", "tidyr",
           "ggplot2", "scales",
           "lmerTest")
f_install_and_load(packs)

# ---- Load data ----
load("../clean_data/db0206.RData")

# ---- Analysis ----


m1 <- lmer(pctsale_export + pctsale_exportind ~ labor_1y + sector +
             bribesize_FDI + log(population) + lgdp +
             (1 | country) + (1 | year),
           data=filter(d_db, ownership=="domestic"))
summary(m1)

m1tech <- lmer(I(foreign_technology == "Yes") ~ labor_1y + sector +
                 bribesize_FDI + log(population) + lgdp +
                 (1 | country) + (1 | year),
               data=filter(d_db, ownership=="domestic"))
summary(m1tech)

m1tech <- lmer(pctsale_domestic_fie * pctsale_domestic / 100 / 100 ~ labor_1y + sector +
                 bribesize_FDI + log(population) + lgdp +
                 (1 | country) + (1 | year),
               data=filter(d_db, ownership=="domestic"))
summary(m1tech)




pd_db <- d_db %>%
  group_by(country, year) %>%
  summarise(export = mean(pctsale_export + pctsale_exportind, na.rm=TRUE),
            bribesize_FDI = mean(bribesize_FDI))

ggplot(pd_db, aes(bribesize_FDI, export)) +
  geom_text(aes(label=paste(country, year)))

m1_logit <- glmer(I(pctsale_export + pctsale_exportind > 0) ~ labor_1y + sector +
              bribesize_FDI + log(population) + lgdp +
              (1 | country) + (1 | year),
              data=filter(d_db, ownership=="domestic"), family="binomial")
summary(m1_logit)


unique(anti_join(d_db[, c("country", "year")], m1@frame[ , c("country", "year")]))

tmp <- d_db %>% filter(country == "Ireland", year == "2005")
describe(tmp$asset_1y)
