rm(list=ls())
source("functions.R")
packs <- c("countrycode", "foreign", "plyr", "dplyr")
f_install_and_load(packs)

# ---- Load dd country to match country name later ----
d_dd_raw <- read.dta("../data/public/ddrevisited.dta")
d_dd <- d_dd_raw %>%
  mutate(country = countrycode(cowcode, origin="cown", destination="country.name", warn=TRUE)) %>%
  select(country, year, cowcode, wdicode, un_region)

# ---- Load db survey wave 1 ----

# db data
d_db_raw <- read.dta("../data/doing_business/StandardizedOld-2002_2005--core2-.dta")
d_db <- d_db_raw %>%
  mutate(domestic_firm = c203a > 50,
         foreign_firm = c203b > 50,
         state_firm = c203c > 50,
         firm_type = as.factor(ifelse(domestic_firm, "domestic",
                                      ifelse(foreign_firm, "foreign",
                                             ifelse(state_firm, "state", NA)))))
d_db_lab <- f_stata_to_df(d_db_raw)
d_db_env <- f_stata_to_env(d_db_raw)

# Clean country name
d_db <- f_splitcountryyear(d_db)
d_db$country <- f_cleancname(d_db$country)
sort(unique(setdiff(d_db$country, d_dd$country)))

# ---- Save clean data ----
save(d_db, d_db_lab, d_db_env, file="../clean_data/doingbusiness.RData")