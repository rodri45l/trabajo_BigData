library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)
#exchange rates https://ec.europa.eu/eurostat/data/database?p_p_id=NavTreeportletprod_WAR_NavTreeportletprod_INSTANCE_nPqeVbPXRmWQ&p_p_lifecycle=0&p_p_state=normal&p_p_mode=view&p_p_col_id=column-2&p_p_col_pos=2&p_p_col_count=3


exch_ger_csv<- read_csv("./datos/ert_bil_eur_m_1_Data.csv")



ger_ex_rate <- exch_ger_csv %>% filter(CURRENCY == "US dollar"|CURRENCY == "Chinese renminbi-yuan",STATINFO == "Value at the end of the period")
 

ger_exch_NA <- ger_ex_rate %>% mutate( value = na_if(Value,":"))


ger_exch <- ger_exch_NA %>% select(TIME, CURRENCY, value)


ger_exch3 <- pivot_wider(ger_exch, values_from = value, names_from = CURRENCY)
ger_exch4 <-ger_exch3 %>% mutate(date = seq(from = as.Date("1999-01-01"), to = as.Date("2020-10-01"), by = 'month'))

ger_exch5 <- ger_exch4 %>% pivot_longer(cols = 2:3, names_to = "CURRENCY", values_to = "values")
 ger_exch6 <- ger_exch5 %>% drop_na(values)

 ggplot(ger_exch6, aes(date, values,group = CURRENCY )) +
  geom_line(aes(color = CURRENCY)) +
   theme(axis.title.y=element_blank(),
         axis.text.y=element_blank(),
         axis.ticks.y=element_blank())+
   labs(x = "Date", y = "exchange rate", 
       title = "Tipo de cambio euro/dolar")
