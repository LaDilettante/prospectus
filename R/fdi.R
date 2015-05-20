rm(list=ls())
packs <- c("WDI", "foreign", "dplyr", "countrycode", "ggplot2")
lapply(packs, library, character.only=TRUE)

d_fdi <- WDI(indicator = c("BX.KLT.DINV.CD.WD", "NY.GDP.MKTP.KD",
                           "NY.GDP.PCAP.KD"),
             start=1970, end=2008, extra=TRUE) %>%
  rename(fdi_inflow = BX.KLT.DINV.CD.WD,
         gdp = NY.GDP.MKTP.KD, gdppc = NY.GDP.PCAP.KD) %>%
  select(iso2c, country, year, fdi_inflow, gdp, gdppc)

d_regime <- read.dta("../data//ddrevisited.dta") %>%
  mutate(iso2c = countrycode(cowcode, origin="cown", destination="iso2c")) %>%
  select(iso2c, democracy, lparty, year)

d <- inner_join(d_fdi, d_regime, by=c("iso2c", "year"))

ggplot(data=d) + geom_boxplot(aes(x=factor(democracy), y=log(fdi_inflow)))
ggplot(data=d) + geom_boxplot(aes(x=factor(democracy), y=log(gdppc)))

summary(lm(fdi_inflow ~ gdp + gdppc, data=d))
summary(lm(fdi_inflow ~ gdp + gdppc, data=filter(d, democracy==1)))
summary(lm(fdi_inflow ~ gdp + gdppc, data=filter(d, democracy==0)))
