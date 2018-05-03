library(sp)
library(deldir)
library(raster)
library(rgeos)
library(tidyverse)

#setwd("C:/Users/garretrc/voronoi_dev")

source("geom_voronoi.R")
source("stat_voronoi.R")
source("StatVoronoi.R")
source("voronoi_polygon.R")

#start with some simulated data and drawing the path only
x=sample(1:100,50)
y=sample(1:100,50)

?voronoi_path
ggplot()+voronoi_path(x,y)

#now add a simple fill component and turn these into polygons!
?voronoi_polygon
ggplot()+voronoi_polygon(x=x,y=y,fill=1:50)

#sweet! of course we can get fancy with this since we're using ggplot2
#base fill on distance from the center
fill = sqrt((x-50)^2 + (y-50)^2)

ggplot()+
  voronoi_polygon(x,y,fill=fill)+
  scale_fill_gradient(low="white",high="red")+
  geom_point(aes(x,y),alpha=.2)

#and now finally we can restrict the area where the voronoi diagram is drawn with the outline argument

circle = data.frame(x = 50*(1+cos(seq(0, 2*pi, length.out = 1000))), 
                    y = 50*(1+sin(seq(0, 2*pi, length.out = 1000))))

ggplot()+
  voronoi_polygon(x,y,fill=fill,outline=circle)+
  scale_fill_gradient(low="white",high="red")


#Now for an actual use case, maps!

USarea = map_data("usa") %>% filter(region == "main")

#grab some quick data on us cities from https://simplemaps.com/data/us-cities

#takes ~10 seconds
cities = read.csv("https://simplemaps.com/static/data/us-cities/uscitiesv1.4.csv")

#grab only states in the continental US
cities = cities %>%
  filter(!(state_name %in% c("Alaska","Hawaii","Puerto Rico"))) %>%
  mutate(long=lng) %>% dplyr::select(-lng) %>% filter(lat>20)

#grab highest population sity in each state
pop = cities %>% group_by(state_id) %>% arrange(-population) %>% slice(1)

#plot the cities
ggplot()+
  geom_path(data = USarea, aes(x=long,y=lat))+
  geom_point(data = pop, aes(x=long,y=lat))+
  coord_map()

#Before we use the outline argument of voronoi_polygon, we need to make sure:
#First column is x/longitude
#Second column is y/latitude
#pieces of the map are denoted in the group column
names(USarea)

#now plot the voronoi regions with outline as the USarea dataframe
map = ggplot()+
  voronoi_polygon(x=pop$long, y=pop$lat, fill=log(pop$population),outline = USarea)+
  geom_path(data=USarea,aes(x=long,y=lat,group=group))+
  geom_point(data=pop,aes(x=long,y=lat))+
  scale_fill_gradient(low="white",high="darkgreen",guide=F)+
  coord_map()
map

#And if you have ggthemes installed you can theme_map it!
map+ggthemes::theme_map()

#Along with this, we can use voronoi_polygons to see the population of every us city at the same time!
#This makes voronoi diagrams a powerful tool for interpolation

#need to make sure to remove duplicate points!
all_cities = cities %>% filter(!is.na(population)) %>% distinct(long,lat,.keep_all = T) 

#caution: this might take a minute!
big.map = ggplot()+
  voronoi_polygon(x=all_cities$long,y=all_cities$lat,fill=log(all_cities$population),outline=USarea)+
  geom_path(data=USarea,aes(x=long,y=lat,group = group))+
  scale_fill_gradient(low="white",high="darkgreen",guide=F)+
  coord_quickmap()
big.map
