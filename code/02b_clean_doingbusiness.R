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
d_db_raw <- read.dta("../clean_data/doingbusiness_0206.dta")
d_db_lab <- f_stata_to_df(d_db_raw)
d_db_env <- f_stata_to_env(d_db_raw)

d_db <- f_splitcountryyear(d_db_raw)
d_db$country <- f_cleancname(d_db$country)
sort(unique(setdiff(d_db$country, d_dd$country)))

# ---- Merge country ----
load("../clean_data/countrylevel.RData")
d_db <- left_join(d_db, d_country, by=c("country", "year"))
# Only Westbank_Gaza is out
d_db <- moveMe(d_db, tomove = c("country", "iso3c", "year"), where = "first")

# ---- Create some new vars ----
d_countryyear <- d_db %>%
  mutate(FDI = as.numeric(ownership == "foreign")) %>%
  group_by(country, year, FDI) %>%
  summarise(bribesize = mean(bribesize, na.rm=TRUE))
molten <- melt(d_countryyear, .(country, year, FDI))
molten$FDI <- revalue(factor(molten$FDI), replace = c("0"="DDI", "1"="FDI"))
cast <- dcast(molten, country + year ~ variable + FDI)
cast$bribesize_NA <- NULL

# Check merge statistics
# tmp <- anti_join(d_db, cast, by=c("country", "year")) ; unique(tmp[, c("country", "year")])
d_db <- inner_join(d_db, cast, by=c("country", "year"))

# Check the uncertainty of the bribesize aggregation
# tmp <- ddply(d_db, .(country, year),
#              function(df) data.frame(bribesize = mean(df$bribesize, na.rm=TRUE),
#                                      count = sum(!is.na(df$bribesize)),
#                                      miss = sum(is.na(df$bribesize)),
#                                      total = nrow(df),
#                                      missratio = sum(is.na(df$bribesize)) / nrow(df)))

# ---- Save ----

save(d_db, d_db_lab, d_db_env, file="../clean_data/db0206.RData")
