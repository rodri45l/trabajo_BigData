

install.packages("gghighlight")
library(gghighlight)
library(tidyverse)

bcc<- read_csv("./datos/bcc.csv")
bcc1<- bcc %>% select(LOCATION, TIME,Value) %>% pivot_wider(names_from = LOCATION, values_from = Value)
bcc2 <- bcc1 %>% select(TIME, USA, CHN, ESP, JPN,GBR,ARG,DEU)  %>% pivot_longer(cols = 2:8, names_to = "country", values_to = "bcc")%>% drop_na()

bcc3 <- bcc1 %>% select(TIME,DEU) %>% drop_na() %>% rename(year = TIME)


p <-ggplot(bcc3) +
  geom_line(aes(year, DEU )) +
  geom_point(aes(year, DEU ))+
  #gghighlight(max(bcc) > 7)+
  
  #theme(axis.title.y=element_blank(),
  #axis.text.y=element_blank(),
  #axis.ticks.y=element_blank())+
  
  labs(x = "Año", 
       title = "Balanza por cuenta corriente",
       y="%PIB"
       ,subtitle = "Alemania 2000-2019") 
#facet_grid(rows = vars(country),scales = "free") 
p




p <-ggplot(bcc2, aes(TIME, bcc,group = country,color = country )) +
  geom_line(aes(color = country)) +
  geom_point()+
  gghighlight(max(bcc) > 7)+
  
  #theme(axis.title.y=element_blank(),
        #axis.text.y=element_blank(),
        #axis.ticks.y=element_blank())+
   
  labs(x = "Año", 
       title = "Balanza por cuenta corriente",
       y="%PIB"
      ,subtitle = "Alemania 2000-2019") 
  #facet_grid(rows = vars(country),scales = "free") 
p
plotly::ggplotly(p)                                                 