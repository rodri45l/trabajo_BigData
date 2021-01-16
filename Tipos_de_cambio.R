
library(correlation)
library(tidyverse)


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Gráfico tipo de cambio EURO
rm(list = ls())

exch_ger_csv2<- read_csv("./datos/eurofxref.csv")
Inflation<- read_csv("./datos/inflation.csv")

ger_exch9 <- exch_ger_csv2 %>% select(Date, USD, CNY, GBP, JPY) %>% drop_na() %>% pivot_longer(cols = 2:5, names_to = "currency", values_to = "exchange_rate")

ger_exch10 <- exch_ger_csv2 %>% select(Date,USD) %>% drop_na() %>% pivot_longer(cols = 1:2, names_to = "currency", values_to = "exchange_rate")

ger_inf <- Inflation %>% drop_na() %>% rename("2000" = `2000 [YR2000]`,
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
                                              "2019" = `2019 [YR2019]`)  %>% pivot_longer(cols = 5:24, names_to = "year", values_to = "inflation")
ger_inf2 <- ger_inf %>% select('Country Name', year, inflation) %>%  mutate(year = as.numeric(year))

  ger_inf3 <- ger_inf2 %>% pivot_wider(names_from = "Country Name", values_from = inflation) %>%  rename("USA" = `United States`)
 ger_inf4 <- transform(ger_inf3, DIF = USA - Germany)
p <-ggplot(ger_exch9, aes(Date, exchange_rate,group = currency )) +
  geom_line(aes(color = currency)) +

  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  geom_smooth() +
  labs(x = "Año", y = "exchange rate", 
       title = "Tipo de cambio €") +
  facet_grid(rows = vars(currency),scales = "free") +
  theme(legend.title = element_blank())

plotly::ggplotly(p)

ger_exch11 <- transform(ger_exch10, inc = (USD - lag(USD)/ USD)*10)
o <- ggplot() +
  geom_line(aes(Date,inc), ger_exch11) 

  
    i<- ggplot() + geom_line(aes(year,DIF),ger_inf4)

#install.packages("ggpubr")
library(ggpubr)
figure <- ggarrange(o, i,
                    labels = c("A", "B"),
                    ncol = 1, nrow = 2)
figure



cor.test(exch_ger_csv2$GBP,exch_ger_csv2$JPY)




cor.test(exch_ger_csv2$CNY, exch_ger_csv2$USD)


corrplot(cor(ger_exch12),
         method = "number",
         type = "upper" # show only upper side
)
m <- cor(ger_exch12)
ger_exch12 <- exch_ger_csv2 %>% select(USD, GBP, JPY,CNY) %>% drop_na()

str(ger_exch12)