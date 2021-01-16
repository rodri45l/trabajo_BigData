library(ggthemes) # Load
library(tidyverse)
library(rio)
library(plotly)
library(janitor)
library(gganimate)
library(viridis)
library("sf")
library(tidyverse)
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world_points<- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))

df_pwr<- read_csv("./datos/pib5.csv") %>% select(-c(`Series Name`,`Series Code`))  %>% rename(sovereignt = `Country Name`,
          `1990`=`1990 [YR1990]`,
          `1991`=`1991 [YR1991]`,
          `1992`=`1992 [YR1992]`,
          `1993`=`1993 [YR1993]`,
          `1994`=`1994 [YR1994]`,
          `1995`=`1995 [YR1995]`,
          `1996`=`1996 [YR1996]`,
          `1997`=`1997 [YR1997]`,
          `1998`=`1998 [YR1998]`,
          `1999`=`1999 [YR1999]`,
          `2000`=`2000 [YR2000]`,
          `2001`=`2001 [YR2001]`,
          `2002`=`2002 [YR2002]`,
          `2003`=`2003 [YR2003]`,
          `2004`=`2004 [YR2004]`,
          `2005`=`2005 [YR2005]`,
          `2006`=`2006 [YR2006]`,
          `2007`=`2007 [YR2007]`,
          `2008`=`2008 [YR2008]`,
          `2009`=`2009 [YR2009]`,
          `2010`=`2010 [YR2010]`,
          `2011`=`2011 [YR2011]`,
          `2012`=`2012 [YR2012]`,
          `2013`=`2013 [YR2013]`,
          `2014`=`2014 [YR2014]` ,
          `2015`=`2015 [YR2015]`,
          `2016`=`2016 [YR2016]`,
          `2017`=`2017 [YR2017]`,
          `2018`=`2018 [YR2018]`,
          `2019`=`2019 [YR2019]`

  )

df_pwr20 <- df_pwr %>%  pivot_longer(cols = 3:32, names_to = "year") %>% transform(year = as.numeric(year),
                                                                                  value = as.numeric(value)
                                                                                  )



df_pwr5 <- inner_join(world, df_pwr20, by= c("sovereignt" = "sovereignt")) %>% filter(continent == "Europe", sovereignt != "Bosnia and Herzegovina")

df_pwr9 <- df_pwr5 %>% select(sovereignt, year, value)
p <- ggplot() + geom_sf(data = df_pwr9, aes(fill = value)) +
  labs(title = "Evolucíon del PIB en Europa 1990 - 2019",
       caption = "Datos provenientes de World Bank") +
  scale_fill_viridis(direction = 1,option = "C") +
  transition_time(year) + labs(title = "Evolucíon del PIB en Europa 1990 - 2019", subtitle = "Año:{as.integer(frame_time)}"  ) +
  coord_sf(xlim = c(-17, 40), ylim = c(38, 70)) +
  theme(
    panel.background = element_rect(fill ='#272635' ,
                                    colour = '#272635',
                                    size = 0.5, linetype = "solid"),
    plot.background = element_rect(fill ='#272635' ,
                                   colour = '#272635',
                                   size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = '#CECECE'), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = '#CECECE'),
    axis.text.x =element_text(colour='#CECECE'),
    axis.text.y =element_text(colour='#CECECE'),
    plot.title = element_text(colour = '#CECECE'),
    axis.title.x = element_text(colour = "#CECECE"),
    axis.title.y = element_text(colour = "#CECECE"),
    # panel.border = element_rect(fill='#272635')
  )

anim1 <- animate(p)

anim_save("./imagenes/map2.gif", anim1)

anim <- animate(p)
magick::image_write(anim, path="./imagenes/map.gif")

#![](./imagenes/map.gif)