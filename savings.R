options ( scipen = 999)

library("tidyverse")
savger <- read_csv("./datos/savingsger.csv")
ger_sav <- savger %>% drop_na() %>% rename("2000" = `2000 [YR2000]`,
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
                                              "2019" = `2019 [YR2019]`)  %>% pivot_longer(cols = 5:24, names_to = "year", values_to = "savings")
ger_sav2<- ger_sav %>% select('Country Name', year, savings) %>%  mutate(year = as.numeric(year))




ggplot() +
  geom_line(aes(year,savings,group= 1),colour="blue",ger_sav2) +
  geom_smooth(aes(year,savings),colour = "green",ger_sav2) +
  labs(title="Ahorro",
       subtitle = "Alemania 2000-2019",
       x = "Año",
       y ="%PIB")

invest <- read_csv("./datos/investment.csv") 
invest2 <- invest %>% filter(`Indicator Id` == 345, `Country Name` == "Germany") %>%  pivot_longer(cols = 6:50, names_to = "year", values_to = "investment") %>% 
  select("year","investment") %>%  filter(year <= 2020,year >1999) 
invest3 <- transform(invest2,year = as.numeric(year))      
ger_sav3 <-  transform(ger_sav2,year = as.numeric(year)) 

ggplot(invest3) +
  geom_bar(aes(year,investment,fill= investment),stat="identity") +
  labs(title = "Inversión %PIB",
       subtitle = "Alemania 2000-2019",
       x= "Año",
       y = "%PIB") +
  geom_smooth(aes(year,investment, group= 1), color= "Turquoise")
 

  gersavinv<-inner_join(invest3,ger_sav3) %>% select(investment,savings, year)
 gersavinv2 <- inner_join(invest3,ger_sav3) %>% select(investment,savings, year)%>% rename("Ahorro" = savings,"Inversión" = investment) %>% pivot_longer(cols = 1:2,names_to = "tipo", values_to = "n" ) 

ggplot(gersavinv2,aes(year,n, group = tipo,color = tipo)) +
  geom_line(size = 2) +
  geom_smooth(se = FALSE, method = "lm",color = "black") +
  labs(y = "%PIB",
       x = "Año",
       subtitle =  "Alemania 2000-2019")

library(corrplot)
gersavinv<-inner_join(invest3,ger_sav3) %>% select(investment,savings, year)
m <-gersavinv%>% filter(year>2006) %>% select(investment,savings) 
n<-cor(m,method= "pearson")
corrplot(n,
                      method = "number"
)



#balanza por cuenta corriente alemania

bpcger <- read_csv("./datos/bpc.csv")

bpcger2 <- bpcger %>% drop_na() %>% rename("2000" = `2000 [YR2000]`,
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
                                    "2019" = `2019 [YR2019]`)  %>% pivot_longer(cols = 5:24, names_to = "year", values_to = "bpc") %>% 
  select(year,bpc)


ggplot(bpcger2) +
  geom_line(aes(year, bpc,group = 1),color= "turquoise") +
  geom_smooth(aes(year, bpc,group = 1),color= "turquoise")+
  #geom_smooth(aes(year, ctacap,group = 1),color= "blue")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(title = "Balanza por cuenta corriente",
       subtitle = "Alemania 2000-2019",
       y = "$",
       x = "Año")


debtger <- read_csv("./datos/sovdebt.csv") %>% select (TIME,Value)



"Gráfico deuda"<- ggplot(debtger) +
  geom_line(aes(TIME, Value,group = 1),color= "blue") +
  geom_smooth(aes(TIME, Value,group = 1), color = "white") +
  labs(title = "Deuda Soberana",  
       subtitle  =  "Alemania 2000-2019",
       caption  = "fuente: World Bank Data",
       x = "Año",
       y = "%PIB")


ctacap <- read_csv("./datos/cta-capital.csv") %>%  drop_na() %>% rename("2000" = `2000 [YR2000]`,
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
                                                                        "2019" = `2019 [YR2019]`)  %>% pivot_longer(cols = 5:24, names_to = "year", values_to = "ctacap") %>% 
  select(year,ctacap)




ggplot(ctacap) +
  geom_line(aes(year, ctacap,group = 1),color= "blue") +
  #geom_smooth(aes(year, ctacap,group = 1),color= "blue")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(title = "Cuenta de Capital",
       subtitle = "Alemania 2000-2019",
       y = "",
       x = "Año")
  



ctafin <- read_csv("./datos/cta-financiera.csv") %>%  drop_na() %>% rename("2000" = `2000 [YR2000]`,
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
                                                                        "2019" = `2019 [YR2019]`)  %>% pivot_longer(cols = 5:24, names_to = "year", values_to = "ctafin") %>% 
  select(year,ctafin)

ggplot(ctafin) +
  geom_line(aes(year, ctafin,group = 1),color= "purple") +
  #geom_smooth(aes(year, ctacap,group = 1),color= "blue")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(title = "Cuenta Financiera",
       subtitle = "Alemania 2000-2019",
       y = "",
       x = "Año")
ctares <- read_csv("./datos/cta-res.csv") %>%  drop_na() %>% rename("2000" = `2000 [YR2000]`,
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
                                                                           "2019" = `2019 [YR2019]`)  %>% pivot_longer(cols = 5:24, names_to = "year", values_to = "ctares") %>% 
  select(year,ctares)




ggplot(ctafin) +
  geom_line(aes(year, ctafin,group = 1),color= "blue")
ctaerr <- read_csv("./datos/cta-errores.csv") %>%  drop_na() %>% rename("2000" = `2000 [YR2000]`,
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
                                                                           "2019" = `2019 [YR2019]`)  %>% pivot_longer(cols = 5:24, names_to = "year", values_to = "ctaerr") %>% 
  select(year,ctaerr)



ctapay <- inner_join(bpcger2,ctacap)

ctapay2 <- inner_join(ctapay,ctaerr)

ctapay3 <- inner_join(ctapay2,ctafin)

ctapay4 <- inner_join(ctapay3,ctares)


ctapay5 <- ctapay4 %>%  mutate(bpc = (bpc*(-1))/10000000000 ) %>% mutate(ctafin = (ctafin/10000000000 ))%>% mutate(ctacap = (ctacap/1000000000 ))%>%  select(year,bpc,ctacap,ctafin) %>% rename("Balanza por Cuenta Corriente" = "bpc", 
                                                             "Cuenta Financiera" = "ctafin",
                                                             "Cuenta de Capital" = "ctacap") %>%  
  pivot_longer(cols = 2:4, names_to = "cta")  
                                                                                                                                            



g <- ggplot(ctapay5, aes(x=year, y=value, fill=cta,group=cta)) + 
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  ggtitle("Balanza de Pagos") +
  xlab("Año")   + theme(axis.title.y = element_blank()) +
  theme(legend.title = element_blank())
  

g


pib <- read_csv("./datos/PIB.csv") %>%  drop_na() %>% rename("2000" = `2000 [YR2000]`,
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
                                                                    "2019" = `2019 [YR2019]`)  %>% pivot_longer(cols = 5:24, names_to = "year", values_to = "PIB") %>% 
  select(year,PIB) %>% mutate(PIB = PIB/1000000000)


ggplot(pib,aes(year,PIB,group = 1)) +
  geom_smooth(color = "green", se= FALSE) +
  geom_line(color= "navyblue", size = 2) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(title = "PIB",
       subtitle = "Alemania 2000-2019",
       caption = "World Bank, en $ actuales",
       y = "mil. mill. $",
       x = "Año")
  

exportaciones <- read_csv("./datos/Exp_ger.csv") %>%  drop_na() %>% rename("2000" = `2000 [YR2000]`,
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
                                                             "2019" = `2019 [YR2019]`)  %>% pivot_longer(cols = 5:24, names_to = "año", values_to = "exportaciones") %>% 
  select(año,exportaciones)

importaciones <- read_csv("./datos/Imp_ger.csv") %>%  drop_na() %>% rename("2000" = `2000 [YR2000]`,
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
                                                                 "2019" = `2019 [YR2019]`)  %>% pivot_longer(cols = 5:24, names_to = "año", values_to = "importaciones") %>% 
  select(año,importaciones)


impexp<- inner_join(importaciones, exportaciones) %>% mutate(importaciones = importaciones * -1) %>%   mutate(dif = (exportaciones-(-1*importaciones))) %>% pivot_longer(cols= 2:4,names_to = "tipo",values_to = "n") 

ggplot(impexp) +
  geom_line(aes(año, n,color= tipo,group = tipo),size=2) +
  geom_hline(aes(yintercept=0)) +
  
  
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(title = "EXPORTACIONES, IMPORTACIONES",
       subtitle = "Alemania 2000-2019",
       caption = "World Bank, en $ actuales",
       x = "Año",
       y= " ")
molp <- read_csv("./datos/1993.csv") %>%  drop_na() %>% rename("2000" = `2000 [YR2000]`,
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
                                                                           "2019" = `2019 [YR2019]`,
                                                               "1999" = `1999 [YR1999]`,
                                                               "1998" = `1998 [YR1998]`,
                                                               "1997" = `1997 [YR1997]`,
                                                               "1996" = `1996 [YR1996]`,
                                                               "1995" = `1995 [YR1995]`,
                                                               "1994" = `1994 [YR1994]`,
                                                               "1993" = `1993 [YR1993]`,
                                                               "1992" = `1992 [YR1992]`,
                                                               "1991" = `1991 [YR1991]`,
                                                               "1990" = `1990 [YR1990]`,
                                                               "1989" = `1989 [YR1989]`,
                                                               "1988" = `1988 [YR1988]`,
                                                               "1987" = `1987 [YR1987]`,
                                                               "1986" = `1986 [YR1986]`,
                                                               "1985" = `1985 [YR1985]`,
                                                               "1984" = `1984 [YR1984]`,
                                                               "1983" = `1983 [YR1983]`
                                                               
                                                            )  %>% pivot_longer(cols = 5:41, names_to = "año", values_to = "exportaciones") %>% 
  select(año,exportaciones) %>%  mutate(exportaciones = exportaciones/100000000000)
  
annotation <- data.frame(
  x = c(11,17),
  y = c(5,7),
  label = c("Creación de la UE", "Moneda única")
)

o<-ggplot(molp,aes(año, exportaciones,group=1)) + 
  geom_smooth(se= FALSE, color = "green", method = "lm") +
  geom_line(color = "darkgreen",size = 1.6) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(title = "EXPORTACIONES",
       subtitle = "Alemania 1983-2019",
       caption = "World Bank, en 100 mill. de $ actuales",
       x = "Año",
       y= " ") +
  geom_text(data=annotation, aes( x=x, y=y, label=label),
            color="black", 
            size=4 , fontface="bold" ) +
   annotate("segment", x = 17, xend = 25, y = 5, yend = 15, colour = "red", size=2, alpha=0.4, arrow=arrow())
o

infeu <- read_csv("./datos/inflationeu.csv") %>%  drop_na() %>% rename("2000" = `2000 [YR2000]`,
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
                                                               "2019" = `2019 [YR2019]`,
                                                               "1999" = `1999 [YR1999]`,
                                                               "1998" = `1998 [YR1998]`,
                                                               "1997" = `1997 [YR1997]`,
                                                               "1996" = `1996 [YR1996]`,
                                                               "1995" = `1995 [YR1995]`,
                                                               "1994" = `1994 [YR1994]`,
                                                               "1993" = `1993 [YR1993]`,
                                                               "1992" = `1992 [YR1992]`,
                                                               "1991" = `1991 [YR1991]`,
                                                               "1990" = `1990 [YR1990]`,
                                                               "1989" = `1989 [YR1989]`,
                                                               "1988" = `1988 [YR1988]`,
                                                               "1987" = `1987 [YR1987]`,
                                                               "1986" = `1986 [YR1986]`,
                                                               "1985" = `1985 [YR1985]`,
                                                               "1984" = `1984 [YR1984]`,
                                                               "1983" = `1983 [YR1983]`
                                                               
)  %>% pivot_longer(cols = 5:41, names_to = "año", values_to = "inflacion") %>% 
  select(año,inflacion) 

annotation2 <- data.frame(
  x = c(8),
  y = c(2.1),
  label = c("Inflación Objetivo")
)

ggplot(infeu,aes(año,inflacion,group=1)) +
  geom_line(color = "black") +
  geom_smooth(se = FALSE, color= "green", method= "lm") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(title = "Inflación",
       subtitle = "Union Europea 1983-2019",
       caption = "World Bank",
       x = "Año",
       y= "%PIB") +
  geom_hline(aes(yintercept=2), color = "green") +
  geom_text(data=annotation2, aes( x=x, y=y, label=label),
            color="Green", 
            size=4 , fontface="bold" ) 
paro <- read_csv("./datos/paro.csv") %>%  drop_na() %>% rename("2000" = `2000 [YR2000]`,
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
                                                                       "2019" = `2019 [YR2019]`,
                                                                       "1999" = `1999 [YR1999]`,
                                                                       "1998" = `1998 [YR1998]`,
                                                                       "1997" = `1997 [YR1997]`,
                                                                       "1996" = `1996 [YR1996]`,
                                                                       "1995" = `1995 [YR1995]`,
                                                                       "1994" = `1994 [YR1994]`,
                                                                       "1993" = `1993 [YR1993]`,
                                                                       "1992" = `1992 [YR1992]`,
                                                                       "1991" = `1991 [YR1991]`,
                                                                       "1990" = `1990 [YR1990]`,
                                                               "2020" = `2020 [YR2020]`
                                                                       
) %>% select(!("1990")) %>% pivot_longer(cols = 5:34, names_to = "año", values_to = "paro") %>% 
  select(año,paro)
  
annotation3 <- data.frame(
  x = c(15),
  y = c(2.1),
  label = c("A partir de 2005 Alemania solo crea empleo")
)

ggplot(paro,aes(año,paro,group=1)) +
  geom_line(color = "red", size = 2) +
  geom_smooth(se = FALSE, color= "pink", method= "lm") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(title = "Tasa de desempleo",
       subtitle = "Alemania 1991-2020",
       caption = "World Bank",
       x = "Año",
       y= "%") +
  geom_vline(aes(xintercept=15), color = "grey") +
  geom_text(data=annotation3, aes( x=x, y=y, label=label),
            color="Black", 
            size=3.3 , fontface="bold" )  +
  annotate("segment", x = 15, xend = 30, y = 11.3, yend = 3, colour = "Green", size=1.5, alpha=0.8, arrow=arrow())


paroesp <- read_csv("./datos/paroesp.csv") %>%  drop_na() %>% rename("2000" = `2000 [YR2000]`,
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
                                                               "2019" = `2019 [YR2019]`,
                                                               "1999" = `1999 [YR1999]`,
                                                               "1998" = `1998 [YR1998]`,
                                                               "1997" = `1997 [YR1997]`,
                                                               "1996" = `1996 [YR1996]`,
                                                               "1995" = `1995 [YR1995]`,
                                                               "1994" = `1994 [YR1994]`,
                                                               "1993" = `1993 [YR1993]`,
                                                               "1992" = `1992 [YR1992]`,
                                                               "1991" = `1991 [YR1991]`,
                                                            
                                                               "2020" = `2020 [YR2020]`
                                                               
)  %>% pivot_longer(cols = 5:34, names_to = "año", values_to = "paro") %>% 
  select(año,paro)

annotation4 <- data.frame(
  x = c(19,23),
  y = c(2.1, 28),
  label = c("Crisis 2007","26%")
)

ggplot(paroesp,aes(año,paro,group=1)) +
  geom_line(color = "black", size = 2) +
  geom_smooth(se = FALSE, color= "Blue") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(title = "Tasa de desempleo",
       subtitle = "España 1991-2019",
       caption = "World Bank",
       x = "Año",
       y= "%") +
  geom_vline(aes(xintercept=17), color = "yellow") +
  geom_text(data=annotation4, aes( x=x, y=y, label=label),
            color="RED", 
            size=3.3 , fontface="bold" )  +
  annotate("segment", x = 17, xend = 23, y = 9, yend = 26, colour = "RED", size=1.5, alpha=0.5, arrow=arrow()) +
  
  annotate("segment", x = 23, xend = 30, y = 26, yend = 13, colour = "green", size=1.5, alpha=0.5, arrow=arrow())
