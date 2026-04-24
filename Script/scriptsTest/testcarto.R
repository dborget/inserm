## obtenir les latitudes et longitudes 
addr<-c("CHU GA La Tronche","2 rue de la république Grenoble")
num_adresse<-c("add1","add2")
table<-data.frame(addr,num_adresse)
table
geocode(table, address=addr, method='osm', lat=latitude, long=longitude)

#create a dataframe with addresses

some_addresse_2 <- data.frame(name=c("White House","Transamerica Pyramid","Domicile"), addr=c("1600 Pennsylvania Ave NW, Washington, DC", "600 Montgomery St, San Francisco, CA 94111", "25 Rue Jean Prévost, 38000 Grenoble"))

# mettre sur une carte 1 point
# map <- leaflet()
# map <- addTiles(map)
# map <- addMarkers(map, lng = lat_long_4$longitude[1], lat = lat_long_4$latitude[1], popup = lat_long_4$structure_clean[1])
# print(map)
# mettre sur une carte toutes les adresses

# test donnees datagouov
testgeocode<-read_excel("Resultats/testgdatagouv.xlsx")
testgeocode<-mutate(testgeocode, latitude=as.numeric(latitude), longitude=as.numeric(longitude))
map2 <- leaflet()
map2 <- addTiles(map2)
for (i in seq_along(testgeocode$adresse)){
  map2 <-addMarkers(map2, lng = testgeocode$longitude[i], lat = testgeocode$latitude[i], popup = testgeocode$adresse[i])
}
print(map2)


# importer les regroupements pour l'isere 
adresses<-read_excel("Resultats/sfgyne_isere_gpt.xlsx")

# ajouter la colonne adresse de la table initiale pour les structures qui ressortent dans la table SfGoAdrsse_isere
# récupérer l'adresse la plus fréquemment renseignée pour une valeur "structure_clean" à partir de la table netoyée sageefemmeGyne_isere
table_comptage_adresse<-group_by(count(SagefemmeGyne_isere, structure_clean, adresse, sort = TRUE), structure_clean)
adresses_supp<-slice_max(table_comptage_adresse,n, with_ties = FALSE)
adresses_supp<-select(adresses_supp,-n)
# Regrouper avec la table adresses
adresses<-left_join(adresses,adresses_supp, by="structure_clean")
# unir les colonnes adresse et ville pour un meilleur geocodage
adresses<-unite(adresses, addr, c(5,2), sep=", ", remove=FALSE)
# geocoder les adresses
lat_long_isere<-geocode(adresses,addr, method = 'osm', lat = latitude , long = longitude)


# projeter sur une carte openstreetmap
map <- leaflet()
map <- addTiles(map)
for (i in seq_along(adresses$addr)){
  map <-addMarkers(map, lng = lat_long_isere$longitude[i], lat = lat_long_isere$latitude[i], popup = lat_long_isere$structure_clean[i])
}
print(map)

write_csv2(lat_long_isere, "Resultats/coordonnees_geo_isere.csv")