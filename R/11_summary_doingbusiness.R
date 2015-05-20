rm(list=ls())
source("functions.R")
packs <- c("ggplot2", "plyr", "dplyr")
f_install_and_load(packs)

# ---- Load data ----
load("../clean_data//countrylevel.RData")
load("../clean_data//doingbusiness.RData")

# ---- Graph ----
d <- d_db %>%
  inner_join(d_country, by=c("country", "year"))

c_discretionary_obstacles <- c("c218d", "c218e",
                               "c218k", "c218l", "c218m",
                               "c218o", "c218p", "c218q",
                               "c218q", "c218r")

d$obstacle <- rowMeans(data.frame(lapply(d[ , names(d)[grep("c218", names(d))]], as.numeric)),
                      na.rm=TRUE)
d$discretionary_obstacle <- rowMeans(data.frame(lapply(d[ , c_discretionary_obstacles], as.numeric)),
                                     na.rm=TRUE)
d_diff <- d %>%
  group_by(democracy, country, year, firm_type) %>%
  summarise(obstacle = mean(obstacle, na.rm=TRUE),
            discretionary_obstacle = mean(discretionary_obstacle, na.rm=TRUE))

d_diff2 <- ddply(d_diff, .(country, year), transform,
                 diff=c(NA,diff(obstacle)),
                 discretionary_diff = c(NA, diff(discretionary_obstacle)))

ggplot(filter(d_diff2, firm_type=="foreign")) +
  geom_boxplot(aes(factor(democracy), diff))


d_agg <- d %>%
  group_by(country, year, firm_type) %>%
  summarize(c218m_ag = mean(c218m, na.rm=TRUE),
            democracy = mean(democracy))



# c218m : regulatory uncertainty
ggplot(data=d) +
  geom_boxplot(aes(factor(democracy), as.numeric(c218m)))

ggplot(data=filter(d, democracy==0)) +
  geom_boxplot(aes(firm_type, as.numeric(c218m))) + facet_wrap( ~ country)

# c218e tax rate
ggplot(data=filter(d)) +
  geom_boxplot(aes(firm_type, as.numeric(c218e) )) + facet_wrap( ~ country)
ggplot(data=filter(d, democracy==0)) +
  geom_boxplot(aes(firm_type, obstacle)) + facet_wrap( ~ country)
ggplot(data=filter(d, democracy==0)) +
  geom_boxplot(aes(firm_type, discretionary_obstacle)) + facet_wrap( ~ country)

# obstacle
ggplot(data=filter(d)) +
  geom_boxplot(aes(firm_type, obstacle )) + facet_wrap( ~ country)

ggplot(d_diff) +
  geom_boxplot(aes(democracy, ))
