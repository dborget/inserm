#### Script pour l'application des fichiers de sages femmes et gyne

#0. Prérequis -----
##0.1 Packages-----

library(readr) # lire des csv
library(readxl) # lire des fichiers excel
library(dplyr) # utiliser les fonctions mutate, filter, etc
library(writexl) # exporter fichier excel
library(tidyr) # organiser un dataframe
library(stringr) # travailler le texte
library(xlsx) # exporter datagrame dans excel
library(leaflet) # affichage de la carto
library(tidygeocoder) # fonction geocode, récuperer les coordonnées géo
library(htmlwidgets) #sauveegarder en html
library(sf) # charger fond de carte

##0.2 Fonctions -------

# lancer les fonctions requises pour ce programme
source("Script/A_groupement/A1_sfgyne_fonctions.R")

##1.Import----- 
# importer les adresses des sages femmes et gynécologues
SagefemmeGyne <- read.csv2("Data_raw/sagesfemmes_gyne_aura.csv", encoding = "UTF-8")
## vérification des données brutes
class(SagefemmeGyne)
dim(SagefemmeGyne)
# head(SagefemmeGyne)
# importer la liste des communes de l'isere avec leurs codes communes
code_commune<- read_csv("Data_raw/communes_aura_2025.csv")

#2. Cleaning -------
##2.1. formatage et correction typo----
# appliquer la fonction formater table ars. On supprime à cette étape les gyne obstetriciens
SagefemmeGyne<-formater_table_ars(SagefemmeGyne)
# appliquer la fonction nettoyer ecriture à toutes les colonnes étant des charactères
SagefemmeGyne<-mutate(SagefemmeGyne, across(where(is.character),nettoyer_adresse))
code_commune<-mutate(code_commune, across(where(is.character),nettoyer_adresse))

##2.2. isoler les départements d'intérêt : 38, 63, 69----
SagefemmeGyne_clean<-filter(SagefemmeGyne, str_detect(cp, "^(38|63|69)\\d*"))
code_commune_clean<-filter(code_commune, str_detect(dep_code, "^(38|63|69)\\d*"))


## 2.3. Ajout Reseau ----
SagefemmeGyne_clean<-joindre_reseau(SagefemmeGyne_clean, code_commune_clean)
SagefemmeGyne_clean<-completer_reseau(SagefemmeGyne_clean)
# réattribuer les valeurs de réseau à la table SagefemmeGyneFinal (en supprimant les colonnes utiles pour le join)
SagefemmeGyne_clean<-select(SagefemmeGyne_clean,-c("epci_nom","code_postal","codes_postaux","cp_dans_codes_postaux"))

##2.3 Enregistrement de la base sage-femme gyne et codes communnes après premier cleaning et ajout reseau
saveRDS(SagefemmeGyne_clean, "Data_inter/Sagefemmegyne_clean.csv")
saveRDS(code_commune_clean,"Data_inter/code_commune_clean.csv")

#3. Par réseau----
##3.1. ISERE ----
### Step 1: sélection du département ISERE
SagefemmeGyne_isere<-filter(SagefemmeGyne_clean, str_detect(cp, "^38"))
nrow(SagefemmeGyne_isere)
# attention: perte de 7 lignes par rapport au test viales script old 
### Step 2 : cleaning des adresses
# appliquer la fonction correction adresses isère à toutes les colonnes étant des charactères. Attention à appliquer après correction table et nettoyage ecriture
SagefemmeGyne_isere<-mutate(SagefemmeGyne_isere, adresse=corriger_adresse_isere(adresse))
# appliquer la fonction creation structure clean isere à la table complète 
SagefemmeGyne_isere<-creer_structure_clean_isere(SagefemmeGyne_isere)
### Step 3 : groupement des adresses
SfGoAdresse_isere<-grouper_prof(SagefemmeGyne_isere)
print(SfGoAdresse_isere)
write_xlsx(SfGoAdresse_isere,"Resultats/sfgyne_isere_gpt.xlsx")

##3.2. RHONE ----
### Step 1: sélection du département Rhone
SagefemmeGyne_rhone<-filter(SagefemmeGyne_clean, str_detect(cp, "^69"))
nrow(SagefemmeGyne_rhone)
### Step 2 : cleaning des adresses
# appliquer la fonction correction adresses isère à toutes les colonnes étant des charactères. Attention à appliquer après correction table et nettoyage ecriture
SagefemmeGyne_rhone<-mutate(SagefemmeGyne_rhone, adresse=corriger_adresse_rhone(adresse))
# appliquer la fonction creation structure clean isere à la table complète 
SagefemmeGyne_rhone<-creer_structure_clean_rhone(SagefemmeGyne_rhone)
### Step 3 : groupement des adresses
SfGoAdresse_rhone<-grouper_prof(SagefemmeGyne_rhone)
print(SfGoAdresse_rhone)
write_xlsx(SfGoAdresse_rhone,"Resultats/sfgyne_rhone_gpt.xlsx")


##3.3. PUY DE DOME ----
### Step 1: sélection du département Rhone
SagefemmeGyne_dome<-filter(SagefemmeGyne_clean, str_detect(cp, "^63"))
nrow(SagefemmeGyne_dome)

### Step 2 : cleaning des adresses
# appliquer la fonction correction adresses isère à toutes les colonnes étant des charactères. Attention à appliquer après correction table et nettoyage ecriture
SagefemmeGyne_dome<-mutate(SagefemmeGyne_dome, adresse=corriger_adresse_dome(adresse))
# appliquer la fonction creation structure clean isere à la table complète 
SagefemmeGyne_dome<-creer_structure_clean_dome(SagefemmeGyne_dome)
### Step 3 : groupement des adresses
SfGoAdresse_dome<-grouper_prof(SagefemmeGyne_dome)
print(SfGoAdresse_dome)
#write_xlsx(SfGoAdresse_rhone,"Resultats/sfgyne_gpt.xlsx")


#4. MAP-----

map_isere<-projeter_adresses(SagefemmeGyne_isere,SfGoAdresse_isere,"isere")
saveWidget(map_isere, file = "Resultats/carto_isere.html", selfcontained = TRUE)

map_rhone<-projeter_adresses(SagefemmeGyne_rhone,SfGoAdresse_rhone,"rhone")
saveWidget(map_rhone, file = "Resultats/carto_rhone.html", selfcontained = TRUE)

#map_dome<-projeter_adresses(SagefemmeGyne_dome,SfGoAdresse_dome,"dome")