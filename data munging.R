#Grammar data munging;
library(dplyr)
library(shiny)

dat %>%
  group_by(Status) %>%
  summarise(avg = mean(AG_15C)) %>%
  arrange(avg)

library("swirl")
swirl()