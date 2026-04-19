library(leaflet)
library(tidygeocoder)
library(dplyr)
library(tmap)
library(sf)



## obtenir les latitudes et longitudes 
addr<-c("CHU GA La Tronche","2 rue de la république Grenoble")
num_adresse<-c("add1","add2")
table<-data.frame(addr,num_adresse)
table
geocode(table, address=addr, method='osm', lat=latitude, long=longitude)


# ajouter un unique pour ne pas refaire tourner dse. adresses plusieurs fois 


# create a dataframe with addresses
some_addresses <- tibble::tribble(
  ~name,                  ~addr,
  "White House",          "1600 Pennsylvania Ave NW, Washington, DC",
  "Transamerica Pyramid", "600 Montgomery St, San Francisco, CA 94111",     
  "Willis Tower",         "233 S Wacker Dr, Chicago, IL 60606",
  "Gare de Lyon Perrache","Gare de Lyon Perrache",
  "Domicile", "25 Rue Jean Prévost, 38000 Grenoble"
)
some_addresse_2 <- data.frame(name=c("White House","Transamerica Pyramid","Domicile"), addr=c("1600 Pennsylvania Ave NW, Washington, DC", "600 Montgomery St, San Francisco, CA 94111", "25 Rue Jean Prévost, 38000 Grenoble"))

## obtenir les latitudes et longitudes 
adresses<-read_excel("Resultats/sfgyne_isere_gpt3.xlsx")
adresses<-unite(adresses, addr, 2:4, sep=", ", remove=FALSE)
# geocode the addresses
lat_long_4<-geocode(adresses,addr, method = 'osm', lat = latitude , long = longitude)


# mettre sur une carte 1 point
# map <- leaflet()
# map <- addTiles(map)
# map <- addMarkers(map, lng = lat_long_4$longitude[1], lat = lat_long_4$latitude[1], popup = lat_long_4$structure_clean[1])
# print(map)
# mettre sur une carte toutes les adresses
map <- leaflet()
map <- addTiles(map)
for (i in seq_along(adresses$addr)){
  map <-addMarkers(map, lng = lat_long_4$longitude[i], lat = lat_long_4$latitude[i], popup = lat_long_4$structure_clean[i])
  print(map)
}
map <- addMarkers(map, lng = lat_long_4$longitude[1], lat = lat_long_4$latitude[1], popup = lat_long_4$structure_clean[1])
print(map)
#for i in seq_along(adresses){
 #point[i]=st_sfc(st_point(c(adresses$latitude[i], adresses$longitude)))
#}

#point=st_sfc(st_point(c(lat_long_4$latitude[1], lat_long_4$longitude[1])))




