#### Script pour l'application des fichiers de sages femmes et gyne

#0. Packages-----

library(rmarkdown) # ecrire sur rmd
library(markdown) # ecrire sur rmd
library(knitr) # lancer un html
library(readr) # lire des csv
library(readxl) # lire des fichiers excel
library(dplyr) # utiliser les fonctions mutate, filter, etc
library(writexl) # exporter fichier excel
library(tidyr)
library(stringr)
library("xlsx") # exporter datagrame dans excel
#1. Import-----

# importer les adresses des sages femmes et gynécologues
SagefemmeGyne <- read.csv2("Data_raw/sagesfemmes_gyne_isere.csv", encoding = "UTF-8")
## vérification des données brutes
class(SagefemmeGyne)
dim(SagefemmeGyne)
# head(SagefemmeGyne)
# importer la liste des communes de l'AURA avec leurs codes communes
code_commune<- read_csv("Data_raw/communes_isere_2025.csv")

#2. Cleaning -------
##2.1. formatage et correction typo
# appliquer la fonction formater table ars
SagefemmeGyne<-formater_table_ars(SagefemmeGyne)
# appliquer la fonction nettoyer ecriture à toutes les colonnes étant des charactères
SagefemmeGyne<-mutate(SagefemmeGyne, across(where(is.character),nettoyer_adresse))

#########################################################
######### APPEL FONCTIONS SPECIFIQUES ISERE NETTOYAGE
#########################################################
# appliquer la fonction correction adresses isère à toutes les colonnes étant des charactères. Attention à appliquer après correction table et nettoyage ecriture
SagefemmeGyne<-mutate(SagefemmeGyne, adresse=corriger_adresse_isere(adresse))
# appliquer la fonction creation structure clean isere à la table complète 
SagefemmeGyne<-creer_structure_clean_isere(SagefemmeGyne)
# dernière version avec nettoyage
SagefemmeGyneFinal<-SagefemmeGyne

#########################################################
######### APPEL FONCTION COMMUNE RESEAU
#########################################################

# appliquer la fonction jointure_reseau
SagefemmeGyne_commune<-joindre_reseau(SagefemmeGyneFinal, code_commune)

#########################################################
######### APPEL FONCTION ISERE RESEAU
#########################################################
# appliquer la fonction completion_reseau_isere
SagefemmeGyne_commune<-completer_reseau_isere(SagefemmeGyne_commune)
# réattribuer les valeurs de réseau à la table SagefemmeGyneFinal (en supprimant les colonnes utiles pour le join)
SagefemmeGyneFinal<-select(SagefemmeGyne_commune,-c("epci_nom","code_postal","codes_postaux","cp_dans_codes_postaux"))


head(SagefemmeGyneFinal)

#########################################################
######### APPEL FONCTION GROUPEMENT PROFESSIONNELS 
#########################################################
SfGoAdresse<-grouper_prof(SagefemmeGyneFinal)
print(SfGoAdresse)
write.xlsx(SfGoAdresse,"Data/sfgyne_gpt.xlsx")
