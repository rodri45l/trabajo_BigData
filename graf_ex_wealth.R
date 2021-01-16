library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)

# net domestic credit https://data.worldbank.org/indicator/FM.AST.DOMS.CN?locations=DE
#net foreign assets https://data.worldbank.org/indicator/FM.AST.NFRG.CN



options(scipen=999)

remove(list = ls())
for_assets<- read_csv("./datos/for_ass5.csv")
dom_cre<- read_csv("./datos/dom_cre.csv")
for_ass1 <- for_assets %>% pivot_longer(cols = 5:26,names_to = "year", values_to = "Net foreign assets (current LCU)")
for_ass2 <- for_ass1 %>% select ( `Net foreign assets (current LCU)`) %>% drop_na()
dom_cre1 <- dom_cre %>% pivot_longer(cols = 5:26,names_to = "year", values_to = "Net domestic credit (current LCU)")
dom_cre2 <- dom_cre1 %>% select (`Country Name`,year, `Net domestic credit (current LCU)`) %>% drop_na()

ext_wealth <- `bind_cols`(dom_cre2, for_ass2)
for_ass3 <- for_ass1 %>% select ( year,`Net foreign assets (current LCU)`) %>% drop_na()

ext_wealth2 <- ext_wealth %>% mutate(`external wealth` =( `Net foreign assets (current LCU)`- `Net domestic credit (current LCU)` ))
ext_wealth3 <- ext_wealth2 %>% mutate(date = seq(from = as.Date("2001-01-01"), to = as.Date("2019-01-01"), by = 'year')) %>% select(date,`Country Name`,`external wealth`)

plotly::ggplotly(f)

f <- ggplot(ext_wealth3, aes(date,`external wealth` ,group=1)) +
  geom_line(color="red") +
  geom_hline(yintercept=0)+
  geom_point()+
 
  labs(x = "Date", y = "external wealth", 
       title = "Germany external wealth")