# Objectif : obtenir les latitudes et longitudes à partir des regroupements identifiés et les mettre sur une carte

# packages requis
library(leaflet) # affichage de la carto
library(tidygeocoder) # fonction geocode, récuperer les coordonnées géo
library(dplyr)
library(tmap)
library(sf)



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

# fonction projection sur une carte
# la table d'entrée est une table de groupements d'adresses obteenus à partir du code A2_sfgyne_main
projeter_adresses<-function (table_nettoyee_depart,table_adresses) {
  # récupérer l'adresse la plus fréquemment renseignée pour une valeur "structure_clean" à partir de la table netoyée sageefemmeGyne_isere
  table_comptage_adresse<-group_by(count(table_nettoyee_depart, structure_clean, adresse, sort = TRUE), structure_clean)
  # créer une table aveec la liste des valeurs de structures_clean associées à l'adreesse la plus fréquente
  adresses_supp<-slice_max(table_comptage_adresse,n, with_ties = FALSE)
  adresses_supp<-select(adresses_supp,-n)
  # Regrouper avec la table adresses
  table_adresses<-left_join(adresses,adresses_supp, by="structure_clean")
  # unir les colonnes adresse et ville pour un meilleur geocodage
  table_adresses<-unite(adresses, addr, c(5,2), sep=", ", remove=FALSE)
  # geocoder les adresses
  lat_long<-geocode(adresses,addr, method = 'osm', lat = latitude , long = longitude)
  # projeter sur une carte openstreetmap
  map <- leaflet()
  map <- addTiles(map)
  for (i in seq_along(adresses$addr)){
    map <-addMarkers(map, lng = lat_long$longitude[i], lat = lat_long$latitude[i], popup = lat_long$structure_clean[i])
  }
  print(map)
  #exporter les coordonnees
  write_csv2(lat_long, paste0("Resultats/coordonnees_geo_",table_adresses,".csv"))
}
test<-projeter_adresses(SagefemmeGyne_isere,SfGoAdresse_isere)

