rm(list=ls())
source("functions.R")
packs <- c("countrycode", "psData", "WDI", "foreign", "ggplot2", "plyr", "dplyr")
f_install_and_load(packs) ; rm(packs)

# ---- Some constants ----
c_startyear <- 2002 # First year of survey data

# ---- Load dd data ----
d_dd_raw <- read.dta("../data/public/ddrevisited.dta")
d_dd <- d_dd_raw %>%
  filter(year >= c_startyear) %>%
  mutate(iso3c = countrycode(cowcode, origin="cown", destination="iso3c", warn=TRUE)) %>%
  select(iso3c, year, cowcode, wdicode, un_region,
         exselec:lparty, emil, royal, tenure08, democracy, type2, incumb)
d_dd_lab <- f_stata_to_df(d_dd_raw)
d_dd_env <- f_stata_to_env(d_dd_raw)

# ---- WDI data ----
# gdp, gdppc, resourcerent.pc, milexp.pc, land, population
WDI_INDICATORS <- c("NY.GDP.MKTP.KD", "NY.GDP.PCAP.PP.KD", "MS.MIL.XPND.GD.ZS", "NY.GDP.TOTL.RT.ZS",
                    "AG.LND.TOTL.K2", "SP.POP.TOTL")
d_wdi_raw <- WDI(country="all", indicator=WDI_INDICATORS,
                 start=1975, end=2012, extra=TRUE)
d_wdi <- d_wdi_raw %>%
  mutate(milexp.pc = MS.MIL.XPND.GD.ZS) %>%
  select(iso3c, year,
         resource.pc=NY.GDP.TOTL.RT.ZS, land=AG.LND.TOTL.K2, population=SP.POP.TOTL,
         gdp=NY.GDP.MKTP.KD, gdppc=NY.GDP.PCAP.PP.KD,
         milexp.pc, region) %>%
  mutate(lgdppc = log(gdppc)) %>% select(-gdppc) %>%
  mutate(lgdp = log(gdp)) %>% select(-gdp) %>%
  arrange(iso3c, year) %>%
  filter(year >= c_startyear)

# Geddes data: authoritarian type (1946 - 2010)
d_geddes_raw <- read.table("../data/public/GWF_Autocratic_Regimes_1_2/GWF_AllPoliticalRegimes.txt",
                           header=TRUE, sep="\t")
d_geddes <- d_geddes_raw %>%
  mutate(iso3c = countrycode(cowcode, "cown", "iso3c")) %>%
  mutate(gwf_autocracy = ifelse(is.na(gwf_nonautocracy), 1, 0)) %>%
  select(iso3c, year,
         gwf_military, gwf_personal, gwf_party, gwf_monarchy, gwf_duration, gwf_autocracy) %>%
  filter(year >= c_startyear)
d_other_dem <- ddply(d_geddes, .(year), summarize, other.democracies = 1 - mean(gwf_autocracy))

# Gandhi data: fractionalization
d_gandhi_raw <- read.csv("../data/private/Pol_Inst_Dictatorship_Data.csv")
# Ethnic polarization is unique for each country
d_ethnic_raw <- ddply(d_gandhi_raw, .(Country.name), function(d) unique(d$Ethnic.polarization))
d_ethnic <- d_ethnic_raw %>%
  mutate(iso3c = countrycode(Country.name, origin="country.name", destination="iso3c")) %>%
  select(iso3c, ethnic.polarization=V1)
d_ethnic <- na.omit(d_ethnic)

# ---- Merge ----
d_country <- left_join(d_dd, d_wdi, by=c("iso3c", "year")) %>%
  left_join(d_ethnic, by=c("iso3c")) %>%
  left_join(d_geddes, by=c("iso3c", "year"))

d_country <- d_country %>%
  mutate(country = countrycode(iso3c, origin="iso3c", destination="country.name"))

d_country <- moveMe(d_country, tomove = c("country", "year", "iso3c"), where = "first")
# ----- Save country level data ----

save(d_dd, d_dd_lab, d_dd_env, file="../clean_data/dd.RData")
save(d_country, file="../clean_data/countrylevel.RData")
