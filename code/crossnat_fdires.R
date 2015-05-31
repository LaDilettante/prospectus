rm(list=ls())
library(ggplot2)
library(dplyr)

d <- read.csv("../data/Pandya_Replication//fdi.entryres.csv")

ggplot(data=d) + geom_boxplot(aes(x=factor(democracy), y=entry_res))

d_p <- d %>%
  group_by(democracy, year) %>%
  summarize(mean_entry_res = mean(entry_res, na.rm=T),
            low_entry_res = mean_entry_res - sd(entry_res, na.rm=T),
            hi_entry_res = mean_entry_res + sd(entry_res, na.rm=T))

ggplot(data=filter(d_p, !is.na(democracy))) +
  geom_line(aes(x=year, y=mean_entry_res, col=factor(democracy))) +
  geom_ribbon(aes(x=year, ymin=low_entry_res, ymax=hi_entry_res,
                  fill=factor(democracy)), alpha=0.1)

ggplot(data=filter(d, democracy==0, cty_1=="indonesia")) +
  geom_line(aes(year, entry_res))
ggplot(data=d) +
  geom_line(aes(year, entry_res)) + facet_wrap( ~ cty_1)
ggplot(data=filter(d, democracy==0)) +
  geom_line(aes(year, entry_res)) + facet_wrap( ~ cty_1)
ggplot(data=filter(d, democracy==1)) +
  geom_line(aes(year, entry_res)) + facet_wrap( ~ cty)


ggplot(data=d) + geom_boxplot(aes(x=factor(regime), y=entry_res))

d %>% filter(democracy==0) %>% select(cty_1, year, entry_res)
