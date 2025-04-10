# Class 4/10
library(sf) # simple features (spatial vector data) st_read, st_transform
# library(broom) # part of tidyverse
library(tidyverse)
library(mapdata)  # map_data
library(marmap) # getNOAA.bathy()

# Carcass location data
carcass = read.csv('data/RW_carcasses_2017.csv')

#Read in US critical habitat shapefiles 
# https://www.greateratlantic.fisheries.noaa.gov/educational_resources/gis/data/index.html
USA_crit_hab = st_read(dsn = 'data/North_Atlantic_Right_Whale_Critical_Habitat/',layer = 'North_Atlantic_Right_Whale_Critical_Habitat') # reads in set of shapefiles
USA_crit_hab
USA_crit_hab_sf = st_transform(USA_crit_hab, crs=4326) #crs="+proj=longlat +datum=WGS84")

#Load in Canadian RW critical habitat coordinates http://www.dfo-mpo.gc.ca/species-especes/profiles-profils/rightwhaleNA-baleinenoireAN-eng.html
CAN_crit_hab = read.csv('data/NARW_canadian_critical_habitat_2017.csv')
head(CAN_crit_hab)

# Turn data frame into sf points, then sf polygon
CAN_crit_hab_sf = CAN_crit_hab %>% 
  st_as_sf(coords=c("lon","lat"), crs=4326) %>% # convert to sf
  dplyr::group_by(habitat, country) %>% 
  dplyr::summarize(do_union=FALSE) %>% # collapses data into multipoint; do_union=FALSE prevents reordering points; check out ?summarise.sf
  st_cast("POLYGON") # converts btwn spatial geometries
print(CAN_crit_hab_sf) # 2 simple features, with habitat and country attributes

# Simply USA_crit_hab data frame to match CAN_crit_hab
plot(USA_crit_hab_sf$geometry[1], axes=TRUE) # GOM habitat
plot(USA_crit_hab_sf$geometry[2], axes=TRUE) # FL / GA habitat
USA_crit_hab_sf$habitat=c("GOM", "SEUS")
USA_crit_hab_sf$country="USA"
USA_crit_hab_sf = USA_crit_hab_sf %>% 
  dplyr::select(country, habitat, geometry) # drops all other variables from shapefile

# Join the USA and Canada critical habitat sf objects
crit_hab = rbind(USA_crit_hab_sf, CAN_crit_hab_sf)

lat_bounds= c(39,53)
lon_bounds= c(-72, -54)
world_map= map_data("worldHires", ylim= lat_bounds, xlim= lon_bounds)

crit_map= ggplot()+
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill = "black")+
  geom_sf(data = crit_hab, aes(fill=country), alpha=0.5)+
  geom_point(data = carcass, aes(x=Longitude, y=Latitude,
                                 color= Carcass.condition.at.first.observation))+
  coord_sf(xlim = lon_bounds, ylim = lat_bounds)+
  theme_classic()
crit_map
ggsave(plot = crit_map, filename = "figures/crit_map.pdf")

### AIS Automatic Identification System 
library(lubridate)
ais_day= read.csv("data/processed_ais/ais_2017-01-25.csv")
head(ais_day)
dim(ais_day)

SE_lat_bounds= c(25,34)
SE_lon_bounds= c(-82, -76)

world_map= map_data("worldHires", ylim= SE_lat_bounds, xlim= SE_lon_bounds)
USA_crit_hab

ais_map_points= ggplot()+
  geom_polygon(data=world_map, aes(x=long, y= lat, group = group), fill= "darkgray")+
  geom_sf(data = USA_crit_hab, alpha= 0.5, fill= "pink")+
  geom_point(data = ais_day, aes(x=LON, y=LAT))+
  coord_sf(xlim = SE_lon_bounds, ylim= SE_lat_bounds)+
  theme_classic()

ais_map_points

#which points fall inside the polygon for the SEUS crit hab
Sys.time()
ships_RW_intersect= ais_day %>%
  st_as_sf(coords = c("LON", "LAT"), crs= 4269) %>%
  st_intersection(USA_crit_hab %>% dplyr::select(geometry))
Sys.time()

law_breakers= ships_RW_intersect %>%
  filter(Length > 20, SOG > 10)

head(law_breakers)
dim(law_breakers)
unique(law_breakers$VesselName)
length(unique(law_breakers$CallSign))

illegal_paths= law_breakers %>%
  mutate(date_time= lubridate::ymd_hms(BaseDateTime)) %>%
  arrange(date_time)%>%
  group_by(CallSign) %>%
  summarise(do_union = FALSE)%>%
  st_cast(to= "LINESTRING")%>%
  st_make_valid()
  
head(illegal_paths)  

law_breaking_map= ggplot()+
  geom_polygon(data = world_map, aes(x=long, y=lat, group=group), fill= "darkgreen")+
  geom_sf(data = USA_crit_hab, alpha= 0.5, fill="darkslategrey")+
  geom_sf(data=illegal_paths, aes(color= CallSign, fill=CallSign))+
  coord_sf(xlim = SE_lon_bounds, ylim = SE_lat_bounds)
law_breaking_map

illegal_path_lengths= illegal_paths %>%
  mutate(track_length_m= st_length(geometry))

head(illegal_path_lengths)
sum(illegal_path_lengths$track_length_m)
