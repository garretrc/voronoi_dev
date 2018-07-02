library(ggplot2)
library(dplyr)
library(sp)
library(rgeos)
library(deldir)
library(raster)

#setwd("C:/Users/garretrc/voronoi_dev")

source('voronoi_polygon.R')
source('stat_voronoi.R')
source('StatVoronoi.R')
source('geom_voronoi.R')

####Circles####
#start with some simulated data and drawing the path only
x=sample(1:100,50)
y=sample(1:100,50)
fill = sqrt((x-50)^2 + (y-50)^2) #distance form the center
points = data.frame(x=x,y=y,fill=fill)

#We can draw only the paths
ggplot(points)+
  geom_voronoi(aes(x,y),fill=NA,color="black")+
  theme_minimal()

#or add in a fill variable!
ggplot(points)+
  geom_voronoi(aes(x,y,fill=fill))+
  theme_minimal()

#but most of the time we don't want a square, we want a predefined region.
#the outline argument can take any dataframe with the following structure:
#first column is x/longitude
#second column is y/latitude
#optional column "group"
#Or you can feed it any spatial polygons dataframe!

circle = data.frame(x = 50*(1+cos(seq(0, 2*pi, length.out = 1000))), 
                    y = 50*(1+sin(seq(0, 2*pi, length.out = 1000))),
                    group=rep(1,1000))

ggplot(data=points, aes(x=x, y=y, fill=fill)) + 
  geom_voronoi(outline = circle)+
  theme_minimal()

#And with more knowlege of ggplot we can add more:
ggplot(points)+
  geom_voronoi(aes(x=x,y=y,fill=fill),outline=circle)+
  scale_fill_gradient(low="white",high="darkred",guide=F)+
  geom_point(aes(x,y))+
  theme_minimal()
  
####North America Example####

#This example will be using multiple maps, and is a bit more complicated!
us_cont = map_data(map = "usa")
mexico = map_data("world", "mexico")

outlines = rbind(us_cont, mexico)
outlines = outlines %>% 
  mutate(group = paste(region, subregion, group, sep = '.')) %>% # Need 'group' variable to be a unique variable now, wasn't from rbinding multiple together
  filter(long < 100) # Just to ignore that little Alaskan island that is on other side of 180/-180 line

cities = world.cities %>% filter(country.etc %in% c('USA', 'Mexico') & pop > 100000)

ggplot() + 
  geom_voronoi(data=cities, 
               aes(long, lat, fill = log(pop)),
               outline = outlines)+
  scale_fill_gradient(high="darkgreen",low="gray90")+
  geom_path(data=outlines, 
            aes(x=long,y=lat,group=group))+
  theme_minimal()+
  coord_map(projection = "gilbert")


####To be finished####

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
