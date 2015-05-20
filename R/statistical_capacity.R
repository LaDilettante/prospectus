rm(list=ls())
packs <- c("WDI", "foreign", "dplyr", "countrycode", "ggplot2", "reshape2")
lapply(packs, library, character.only=TRUE)

d_stat <- read.csv("../data/statistical_capacity/668e44a4-fa26-4ba7-ab2b-094a4b5c55ab_Data.csv",
                   stringsAsFactors=FALSE) %>%
  select(-Series.Code, -Country.Name) %>%
  filter(Series.Name != "", Country.Code != "") # Blank lines in csv, R misinterpreted as vars

d_stat_long <- melt(d_stat, id.vars = c("Series.Name", "Country.Code"),
                    variable.name = "Year") %>%
  mutate(Year = as.character(Year)) %>%
  mutate(Year = substr(Year, nchar(Year) - 4, nchar(Year) - 1))

d_stat_wide <- dcast(d_stat_long, formula=Country.Code + Year ~ Series.Name) %>%
  mutate(iso2c = countrycode(Country.Code, origin="iso3c", destination = "iso2c")) %>%
  mutate(Year = as.numeric(Year))

d_regime <- read.dta("../data//ddrevisited.dta") %>%
  mutate(iso2c = countrycode(cowcode, origin="cown", destination="iso2c")) %>%
  select(iso2c, year, democracy, lparty, type2)

d_wdi <- WDI(indicator = c("BX.KLT.DINV.CD.WD", "NY.GDP.MKTP.KD",
                           "NY.GDP.PCAP.KD"),
             start=1970, end=2008, extra=TRUE) %>%
  rename(fdi_inflow = BX.KLT.DINV.CD.WD,
         gdp = NY.GDP.MKTP.KD, gdppc = NY.GDP.PCAP.KD) %>%
  select(iso2c, country, year, fdi_inflow, gdp, gdppc)

d_pippa <- read.csv("../data/Democracy Crossnational Data Spring 2009.csv",
                    stringsAsFactors=FALSE) %>%
  select(Natabrv, Freepress2006rev) %>%
  mutate(iso2c = countrycode(Natabrv, "iso3c", "iso2c"))

d <- inner_join(d_stat_wide, d_regime, by = c("iso2c", "Year" = "year")) %>%
  mutate(country = countrycode(iso2c, origin="iso2c", destination="country.name")) %>%
  inner_join(d_wdi, by=c("iso2c", "Year" = "year")) %>%
  rename(stat_capacity = `Statistical Capacity score (Overall average)`) %>%
  inner_join(d_pippa, by=c("iso2c")) %>%
  mutate(econs = `Agricultural census` +
         `Balance of payments manual in use`+ `Consumer price index base year`+
         `External debt reporting status`+ `Government finance accounting`+
         `Import and export price indexes`+ `Industrial production index`+
         `National accounts base year`+ `Per capita GDP growth`) %>%
  mutate(social = `Access to water`+ `Child immunization`+ `Child malnutrition`+
         `Child mortality`+ `Gender equality in education`+ `Health survey`+
         `HIV/AIDS`+ `Income poverty`+ `Maternal health`+ `National immunization coverage`+
         `Poverty survey`+ `Primary completion`)

ggplot(data=d) +
  geom_boxplot(aes(x=factor(democracy), y=stat_capacity))

ggplot(data=filter(d, democracy==0)) +
  geom_line(aes(Year, y=stat_capacity)) +
  facet_wrap( ~ country)

ggplot(data=filter(d, Year==2008),
       aes(log(gdppc), stat_capacity, col=factor(democracy) )) +
  geom_point() +
  geom_text(aes(label=iso2c)) +
  geom_smooth(method = "lm", color="black")

ggplot(data=filter(d, Year==2008, democracy==0),
       aes(log(gdppc), stat_capacity, col=factor(democracy) )) +
  geom_point() +
  geom_text(aes(label=iso2c)) +
  geom_smooth(method = "lm", color="black")

ggplot(data=filter(d, Year==2008, democracy==1),
       aes(log(gdppc), stat_capacity, col=factor(democracy) )) +
  geom_point() +
  geom_text(aes(label=iso2c)) +
  geom_smooth(method = "lm", color="black")

ggplot(data=filter(d, Year==2006, democracy==0),
       aes(Freepress2006rev, stat_capacity, col=factor(democracy) )) +
  geom_point() +
  geom_text(aes(label=iso2c)) +
  geom_smooth(method = "lm", color="black")

ggplot(data=filter(d, Year==2006, democracy==0),
       aes(Freepress2006rev, econs, col=factor(democracy) )) +
  geom_point() +
  geom_text(aes(label=iso2c)) +
  geom_smooth(method = "lm", color="black")

ggplot(data=filter(d, Year==2006, democracy==0),
       aes(Freepress2006rev, social, col=factor(democracy) )) +
  geom_point() +
  geom_text(aes(label=iso2c)) +
  geom_smooth(method = "lm", color="black")
