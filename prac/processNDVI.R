library(tidyverse)
library(readr)

source("prac/downloadNDVI.R")

dat <- read_csv("prac/MODISdat.csv")

dat %>% filter(calendar_date > as.Date("2015-01-01")) %>%
  ggplot(aes(x = calendar_date, y = value*scale)) + 
  geom_line() +
#  geom_point() +
  facet_wrap(.~ site) +
  ylab("NDVI")
