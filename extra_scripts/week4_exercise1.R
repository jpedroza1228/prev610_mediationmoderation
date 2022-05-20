# Week 4 Exercise 1
week_4 <- rio::import(here::here('data', 'FTC.Week4.csv')) %>% 
  janitor::clean_names()

