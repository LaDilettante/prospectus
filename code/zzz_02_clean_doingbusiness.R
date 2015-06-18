rm(list=ls())
source("functions.R")
packs <- c("countrycode", "foreign", "plyr", "dplyr")
f_install_and_load(packs)

# ---- Load dd country to match country name later ----
d_dd <- read.dta("../data/public/ddrevisited.dta") %>%
  mutate(country = countrycode(cowcode, origin="cown", destination="country.name", warn=TRUE)) %>%
  select(country, year, cowcode, wdicode, un_region)

# ---- Load db survey wave 1 ----

# db data
d_db_raw <- read.dta("../data/doing_business/StandardizedOld-2002_2005--core2-.dta")

d_db <- d_db_raw %>%
  select(year, country, industry, sector, exporter,
         year_est = c201,
         ownership_type = c202,
         pctownership_domestic = c203a,
         pctownership_foreign  = c203b,
         pctownership_state    = c203c,
         pctownership_other    = c203d,

         pctsale_domestic  = c211a1,
         pctsale_export    = c211a2,
         pctsale_exportind = c211a3,

         pctsale_domestic_gov      = c211b1,
         pctsale_domestic_soe      = c211b2,
         pctsale_domestic_fie      = c211b3,
         pctsale_domestic_parent   = c211b4,
         pctsale_domestic_domestic = c211b5,
         pctsale_domestic_other    = c211b6,

         year_firstexport = c211c1,

         pctinput_domestic = c2121,
         pctinput_import = c2122,
         pctinput_importind = c2123,

         starts_with("c216"),
         starts_with("c218"),

         regulation_time = c238,
         bribesize = c239,
         bribesize_phone = c240b1,
         bribesize_electricity = c240b2,
         bribesize_water = c240b3,
         bribesize_constructionpermit = c240b4,
         bribesize_importlicense = c240b5,
         bribesize_operatinglicense = c240b6,

         bribesizesize_phone = c240c1,
         bribesizesize_electricity = c240c2,
         bribesizesize_water = c240c3,
         bribesizesize_constructionpermit = c240c4,
         bribesizesize_importlicense = c240c5,
         bribesizesize_operatinglicense = c240c6,

         bribe_tax = c242d1,
         bribe_labor = c242d2,
         bribe_building = c242d3,
         bribe_sanitation = c242d4,
         bribe_police = c242d5,
         bribe_environment = c242d6,
         bribe_allagencies = c242d7,

         bribesize_tax = c242e1,
         bribesize_labor = c242e2,
         bribesize_building = c242e3,
         bribesize_sanitation = c242e4,
         bribesize_police = c242e5,
         bribesize_environment = c242e6,
         bribesize_allagencies = c242e7,

         bribesize_pct_govcontract = c243,

         corrupt_legislator = c245a,
         corrupt_gov = c245b,
         corrupt_judge = c245c,
         corrupt_party = c245d,

         foreign_technology = c254,

         labor_3y = c262a3y,
         labor_2y = c262a2y,
         labor_1y = c262a1y,

         pctlabor_foreign = c264,

         asset_1y = c281a1y,
         asset_2y = c281a2y,
         asset_3y = c281a3y,
         liability_1y = c282b1y,
         liability_2y = c282b2y,
         liability_3y = c282b3y)

d_db <- d_db %>%
  mutate(ownership = as.factor(ifelse(pctownership_domestic > 50, "domestic",
                                      ifelse(pctownership_foreign > 50, "foreign",
                                             ifelse(pctownership_state > 50, "state", NA)))))
d_db_lab <- f_stata_to_df(d_db)
d_db_env <- f_stata_to_env(d_db)

# Clean country name
d_db <- f_splitcountryyear(d_db)
d_db$country <- f_cleancname(d_db$country)
sort(unique(setdiff(d_db$country, d_dd$country)))

# ---- Save clean data ----
save(d_db, d_db_lab, d_db_env, file="../clean_data/doingbusiness.RData")