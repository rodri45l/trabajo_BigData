library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)
library(lubridate)

# net domestic credit https://data.worldbank.org/indicator/FM.AST.DOMS.CN?locations=DE
#net foreign assets https://data.worldbank.org/indicator/FM.AST.NFRG.CN



options(scipen=999)

remove(list = ls())
for_assets0<- read_csv("./datos/for_ass5.csv")
for_assets<- for_assets0[-c(2:6), ]
gdp0<- read_csv("./datos/gdp.csv")
for_ass1 <- for_assets %>% pivot_longer(cols = 5:26,names_to = "year", values_to = "Net foreign assets (current LCU)")
for_ass2 <- for_ass1 %>% select ( `Country Name`,year,`Net foreign assets (current LCU)`) 
gdp<- gdp0[-c(2:6), ]
gdp1 <- gdp %>% pivot_longer(cols = 5:26,names_to = "year", values_to = "GDP (current LCU)")
gdp2 <- gdp1 %>% select (`Country Name`,year, `GDP (current LCU)`) 

finan_inc <- inner_join(gdp2,for_ass2,by = "year")
finan_inc2 <- finan_inc %>% select(-"Country Name.y")


finan_inc3 <- finan_inc2 %>% mutate(date = seq(from = as.Date("1999-01-01"), to = as.Date("2020-01-01"), by = 'year')) %>% select(-year)
finan_inc4 <- finan_inc3 %>% mutate(`Financial increase` =( `Net foreign assets (current LCU)`/`GDP (current LCU)` )* 100)


ggplot(finan_inc4, aes(date,`Financial increase` ,group=1)) +
  geom_line(color="red") +
  geom_ribbon(aes(ymin=0, ymax=pmax(finan_inc4$`Financial increase`,0)), fill="green", col="green", alpha=0.5)+
  
  labs(x = "Año",
       y = "%", 
      title = "Aumento en transacciones financieras",
      subtitle = "Alemania 2000-2019")


RPCS<- read_csv("./datos/Relativepcs.csv") %>% drop_na() %>% rename("2000" = `2000 [YR2000]`,
                                                                    "2001" = `2001 [YR2001]`,
                                                                    "2002" = `2002 [YR2002]`,
                                                                    "2003" = `2003 [YR2003]`,
                                                                    "2004" = `2004 [YR2004]`,
                                                                    "2005" = `2005 [YR2005]`,
                                                                    "2006" = `2006 [YR2006]`,
                                                                    "2007" = `2007 [YR2007]`,
                                                                    "2008" = `2008 [YR2008]`,
                                                                    "2009" = `2009 [YR2009]`,
                                                                    "2010" = `2010 [YR2010]`,
                                                                    "2011" = `2011 [YR2011]`,
                                                                    "2012" = `2012 [YR2012]`,
                                                                    "2013" = `2013 [YR2013]`,
                                                                    "2014" = `2014 [YR2014]`,
                                                                    "2015" = `2015 [YR2015]`,
                                                                    "2016" = `2016 [YR2016]`,
                                                                    "2017" = `2017 [YR2017]`,
                                                                    "2018" = `2018 [YR2018]`,
                                                                    "2019" = `2019 [YR2019]`)  %>% 
  pivot_longer(cols = 5:24, names_to = "year", values_to = "rpcs") %>% 
  select('Country Name', year, rpcs) %>%  mutate(year = as.numeric(year)) %>% pivot_wider(names_from = 'Country Name', values_from = "rpcs") %>% 
  mutate(rpcs = (Germany/`United States`)) %>% select(rpcs,year)



ger_exch12 <- ger_exch9 %>% filter(currency == "USD") %>% 
  mutate(year = year(Date)) %>% 
  mutate(month = month(Date)) %>% 
  mutate(day = day(Date)) %>% filter(day ==  2) %>% filter(month == 1) %>% select(year,exchange_rate)


d <- inner_join(ger_exch12,RPCS) 



ggplot(d)+ 

geom_line(aes(x= year,y = rpcs, colour = "Precios relativos")) +
  geom_line(aes(x= year,y = exchange_rate, colour = "Tipo de cambio")) +
  scale_colour_manual("", 
                      breaks = c("Precios relativos", "Tipo de cambio"),
                      values = c("red", "blue")) +
  geom_smooth(aes(year,rpcs),color = "red",method = "lm") +
  geom_smooth(aes(year,exchange_rate),color = "blue",method = "lm") +
  xlab(" ") +
  scale_y_continuous(" ", limits = c(0.9,2)) + 
  labs(title = "PPA RELATIVA",
       subtitle = "Alemania 2006-2020",
       x= "Año",
       y = " ") 



