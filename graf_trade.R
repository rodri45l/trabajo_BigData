library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)
options(scipen=999)
#https://data.worldbank.org/indicator/NE.TRD.GNFS.ZS


trade<- read_csv("./datos/trade-GDP-germany.csv")

trade1 <- trade %>% pivot_longer(cols = 5:26,names_to = "year", values_to = "Financial Openness")
trade2 <- trade1 %>% select (year,`Country Name`, `Financial Openness`) %>% drop_na()



trade3 <- trade2 %>% mutate(date = seq(from = as.Date("1999-01-01"), to = as.Date("2019-01-01"), by = 'year')) %>% select(date,`Country Name`,`Financial Openness`)



ggplot(trade3, aes(date,`Financial Openness` ,group=1)) +
  geom_line(color="blue") +
  geom_hline(yintercept=100)+
  
  labs(x = "Date", y = "external wealth", 
       title = "Germany external wealth")