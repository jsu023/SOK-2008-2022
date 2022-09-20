install.packages("ggplot2")
install.packages("tidyverse")
install.packages("readr")
install.packages("plyr")
install.packages("dplyr")


library(readr)
library(ggplot2)
library(tidyverse)

union<- read_csv("union_unempl.csv")

union <- union %>%
  mutate(Excess_coverage = union$coverage - union$density)

mapdata <- map_data("world")

union$country <- gsub("United Kingdom", "UK", union$country)

names(union)[names(union) == "country"] <- "region"

mapdata <- right_join(mapdata, union, by = "region")

arbeidsledighet <- ggplot(mapdata, aes(x= long , y = lat, group = group))+
  geom_polygon(aes(fill = unempl), color = "black")+
  scale_fill_gradient(name = "% arbeidsledighetsrate", low = "mistyrose2", high = "hotpink3")+
  labs(title="Arbeidsledighetsrate\nEuropa 2019")+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank())
  
arbeidsledighet

fagforeningsdensitet <- ggplot(mapdata, aes(x= long , y = lat, group = group))+
  geom_polygon(aes(fill = density), color = "black")+
  scale_fill_gradient(name = "fagforeningsdensitet", low = "lightblue2", high = "tomato1")+
  labs(title="Fagforeningsdensitet\nEuropa 2019")+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank())
 
fagforeningsdensitet

#excess coverage



Excess_coverage <- ggplot(mapdata, aes(x= long , y = lat, group = group))+
  geom_polygon(aes(fill = Excess_coverage), color = "black")+
  scale_fill_gradient(name = "Excess coverage", low = "mintcream", high = "plum", na.value = "seashell1")+
  labs(title="Excess_coverage\nEuropa")+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank())
  
Excess_coverage


Koordinering_av_lønnsfastsettelse <- ggplot(mapdata, aes(x= long , y = lat, group = group))+
  geom_polygon(aes(fill = coord), color = "black")+
  scale_fill_manual(name = "Koordinering av lønnsfastsettelse", values = c("violet", "purple1", "powderblue", "rosybrown1", "yellow1"))+
  labs(title="Koordinering av lønnsfastsettelse\nEuropa 2019")+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank())
  

Koordinering_av_lønnsfastsettelse

