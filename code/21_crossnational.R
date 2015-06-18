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

m1 <- lmer(I(pctsale_export + pctsale_exportind > 0) ~ labor_1y + sector +
             bribesize_FDI + log(population) + lgdp +
             (1 | country) + (1 | year),
           data=d_db)
summary(m1)

m1_logit <- glmer(I(pctsale_export + pctsale_exportind > 0) ~ labor_1y + sector +
              bribesize_FDI + log(population) + lgdp +
              (1 | country) + (1 | year),
              data=d_db, family="binomial")
summary(m1_logit)


unique(anti_join(d_db[, c("country", "year")], m1@frame[ , c("country", "year")]))

tmp <- d_db %>% filter(country == "Ireland", year == "2005")
describe(tmp$asset_1y)
