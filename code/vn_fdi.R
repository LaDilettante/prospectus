rm(list=ls())
source("functions.R")
packs <- c("ggplot2", "plyr", "dplyr")
f_install_and_load(packs)

# ---- read data ----
library(readstata13)

d_ddi <- read.dta13("../data/private/PCI/PCI_DDI_2014.dta")
d_fdi <- read.dta13("../data/private/PCI/PCI_FDI_2014.dta")

names(d_ddi)[grep("^sub", names(d_ddi))]

head(d_ddi$h3)
head(as.numeric(d_ddi$h3))

d_ddi <- d_ddi %>%
  mutate(industry = ifelse(a5_1 == "Yes", "manufacturing",
                           ifelse(a5_2 == "Yes", "construction",
                                  ifelse(a5_3 == "Yes", "service",
                                         ifelse(a5_4 == "Yes", "agriculture",
                                                ifelse(a5_5 == "Yes", "mining", NA)))))) %>%
  mutate(h3_agree = ifelse(h3 == "Agree" | h3 == "Strongly agree", 1,
                           ifelse(h3 == "Disagree" | h3 == "Strongly disagree", 0, NA)))

d_ddi %>%
  group_by(province) %>%
  summarize(mean(as.numeric(h3), na.rm=TRUE)) %>%
  print(n=63)

d_ddi %>%
  group_by(province) %>%
  summarize(mean(as.numeric(h3_4), na.rm=TRUE)) %>%
  print(n=63)

tmp <- d_ddi %>%
  group_by(province) %>%
  summarize(mean = mean(h3_agree, na.rm=TRUE))
ggplot(data=tmp) +
  geom_point(aes(province, mean)) + ylim(c(0, 1))

ggplot(data=d_ddi) +
  geom_boxplot(aes(province, h3_agree))



d_ddi %>%
  group_by(province) %>%
  summarize(mean(as.numeric(h4), na.rm=TRUE)) %>%
  print(n=63)

d_ddi %>%
  group_by(industry) %>%
  summarize(mean(as.numeric(h3), na.rm=TRUE))


ggplot(data=d_ddi) +
  geom_boxplot(aes(province, as.numeric(h3)))
