# Script d'application de la fonction projection, département par département

#0. Prérequis -----
##0.1 Packages-----

library(readr) # lire des csv
library(readxl) # lire des fichiers excel
library(dplyr) # utiliser les fonctions mutate, filter, etc
library(writexl) # exporter fichier excel
library(tidyr) # organiser un dataframe
library(stringr) # travailler le texte
library("xlsx") # exporter datagrame dans excel


##0.2 Fonctions -------

# lancer les fonctions requises pour ce programme
source("Script/B_projection/B1_projection_fonction.R")

#1.Variables d'entrée ----
# variable mois : modifiable pour faire sur plusieurs années : changer la length.out
mois<-seq(as.Date("2027-01-01"), by="month", length.out=12) 
# inf : % de femmes informées sur la possiblité de rejoindre Filomene
inf<-c(25/100, 30/100, 35/100)
# inc: % de femmes acceptant de participer à Filomene
inc<-c(10/100,15/100,20/100)

#2. Lancement des scenario par réseau ----

## 2.1 ISERE RPAI ----


###2.1.1 variables d'entrée ISERE RPAI -----
# Import Data frame entree : maternités et nombre d'accouchements annuel par maternité pour le RPAI
# le nombre total d'accouchement est modulé par le nombre de mois. 
table_entree_rpai <- read.csv2("Data_raw/mater_acc_IsereRPAI.csv", header=TRUE)
table_entree_rpai <-mutate(table_entree_rpai, accouchement=accouchement*length(mois)/12)
maternites_rpai<-table_entree_rpai$maternite
table_entree_rpai <- mutate(table_entree_rpai, maternite=recode(maternite, "HCE"="hce","GHM de Grenoble"="ghm","CH Voiron"="voi", "Clinique  Belledone "="bel","Clinique des Cedres"="ced"))

### 2.1.2. scenario ----
# appel de la fonction creer scenario
scenario_rpai<-lancer_scenario(table_entree_rpai)
# scenario_rpai[[1]]

### variables d'entrées et appel de la fonction annualiser scenario 
# utiiliser les prefixes des colonnes, pour récupérer l'informations selon si colonnes acc, inf, inc
prefixes_rpai <- c("hce_", "ghm_", "voi_", "bel_", "ced_") 
scenario_rpai_annuel<-annualiser_scenario(scenario_rpai,maternites_rpai,prefixes_rpai)
print(scenario_rpai_annuel)

###2.1.3 Sorties -----
# excel avec un scenario par onglet, titre de l'excel avec la date du jour
date<-Sys.Date()
#write_xlsx(scenario_rpai, paste0("Resultats/scenario_rpai",date,".xlsx"),col_names = TRUE)
write_xlsx(scenario_rpai_annuel, paste0("Resultats/scenario_rpai_annuel_",date,".xlsx"),col_names = TRUE)



## 2.2 ISERE AURORE -----


###2.2.1 variables d'entrée ISERE AURORE -----
# Imprt Data frame entree : maternités et le nombre d'accouchements par maternite
# le nombre total d'accouchement est modulé par le nombre de mois.  
table_entree_aurore_i <- read.csv2("Data_raw/mater_acc_IsereAurore.csv", header=TRUE)
# le nombre d'accouchement est modulé par le nombre de mois. 
table_entree_aurore_i <-mutate(table_entree_aurore_i, accouchement=accouchement*length(mois)/12)
# conserver le nom des materenités pour l'annualisation finale
maternites_aurore_i<-table_entree_aurore_i$maternite
# renommer les maternites de manière succinct pour faire tourner les fonctions et utiliser plus facilement les préfixes des colonnes
table_entree_aurore_i <- mutate(table_entree_aurore_i, maternite=recode(maternite, "CH PIERRE OUDOT BOURGOIN JALLIEU"="oud", "CH LUCIEN HUSSEL DE VIENNE"="luc","CLINIQUE SAINT VINCENT DE PAUL"="stv"))

### 2.2.2. scenario ----
# appel de la fonction creer scenario
scenario_aurore_i<-lancer_scenario(table_entree_aurore_i
print(scenario_aurore_i)

### variables d'entrées et appel de la fonction annualiser scenario 
# utiiliser les prefixes des colonnes, pour récupérer l'informations selon si colonnes acc, inf, inc
prefixes_aurore_i <- c("oud_", "luc_", "stv_") 
scenario_aurore_i_annuel<-annualiser_scenario(scenario_aurore_i,maternites_aurore_i,prefixes_aurore_i)
print(scenario_aurore_i_annuel)

### 2.2.3. sortie -----
# excel avec un scenario par onglet, titre de l'excel avec la date du jour
date<-Sys.Date()
#write_xlsx(scenario_aurore_i, paste0("Resultats/scenario_aurore_i",date,".xlsx"),col_names = TRUE)
write_xlsx(scenario_aurore_i_annuel, paste0("Resultats/scenario_aurore_i_annuel_",date,".xlsx"),col_names = TRUE)

## 2.3 RHONE AURORE -----


###2.3.1 variables d'entrée RHONE AURORE -----

# Import Data frame entree : maternités et le nombre d'accouchements par maternite
# La table d'entrée dépend du réseau. Il s'agit du total d'accouchement par an. Il est modulé par le nombre de mois. 
table_entree_aurore_r <- read.csv2("Data_raw/mater_acc_RhoneAurore.csv", header=TRUE)
table_entree_aurore_r <-mutate(table_entree_aurore_r, accouchement=accouchement*length(mois)/12)
maternites_aurore_r <-table_entree_aurore_r$maternite
# changer le nom des maternites pour des noms plus simples et courts
table_entree_aurore_r <- mutate(table_entree_aurore_r, maternite=recode(maternite, "CH montgelas"="mon", "Ch de ste FOY les LYON"="foy", "Hp femme mere enfant HCL"="hfme", "Hopital mere enfant natecia"="nat","medipole hp mutualiste" ="med" ,"clinique du val d'ouest vendome"="ven","groupement fp mutualiste les portes du sud"="ghm", "hp lyon sud HCL "="lsud", "hp croix rousse hcl" ="croix", "ch st joseph st luc" ="jos","ch nord ouest villefranche"="vil","polyclinique du beaujolais"="poly" ))
table_entree_aurore_r$maternite

### 2.3.2. scenario ----
# appel de la fonction creer scenario
scenario_aurore_r<-lancer_scenario(table_entree_aurore_r)
# tous les resultats sont dans la liste resultat scenario
print(scenario_aurore_r)

# variables d'entrées et appel de la fonction annualiser scenario 
# utiiliser les prefixes des colonnes, pour récupérer l'informations selon si colonnes acc, inf, inc
prefixes_aurore_r <- c("mon_","foy_","hfme_","nat_","med_", "ven_", "ghm_","lsud_","croix_","jos_","vil_","poly_")
scenario_aurore_r_annuel<-annualiser_scenario(scenario_aurore_r,maternites_aurore_r,prefixes_aurore_r)
print(scenario_aurore_r_annuel)

### 2.3.3. sortie -----
# excel avec un scenario par onglet, titre de l'excel avec la date du jour
date<-Sys.Date()
#write.xlsx(scenario_aurore_r, paste0("Resultats/scenario_aurore_r",date,".xlsx"),col_names = TRUE)

write_xlsx(scenario_aurore_r_annuel, paste0("Data_inter/scenario_annuel_aurore_r_annuel_",date,".xlsx"),col_names = TRUE)


## 2.4 Puy de Dome RSPA -----


###2.4.1 variables d'entrée PUY DE DOME RSPA -----

# Imprt Data frame entree : maternités et le nombre d'accouchements par maternité pour le RSPA
# La table d'entrée dépend du réseau. Il s'agit du total d'accouchement par an. Il est modulé par le nombre de mois. 
table_entree_rspa_p <- read.csv2("Data_raw/mater_acc_PddRSPA.csv", header=TRUE)
table_entree_rspa_p <-mutate(table_entree_rspa_p, accouchement=accouchement*length(mois)/12)
maternites_rspa_p<-table_entree_rspa_p$maternite
table_entree_rspa_p <- mutate(table_entree_rspa_p, maternite=recode(maternite,"HP Estaing CHU"="est","Clinique Chataigneraie "="cha","CH de Tiers"="tie"))

### 2.4.2. scenario ----
# appel de la fonction creer scenario
scenario_rspa_p<-lancer_scenario(table_entree_rspa_p)
#scenario_rspa_p[[1]]

### variables d'entrées et appel de la fonction annualiser scenario 
# utiiliser les prefixes des colonnes, pour récupérer l'informations selon si colonnes acc, inf, inc
prefixes_rspa_p <- c("est_","cha_","tie_") 
scenario_rspa_p_annuel<-annualiser_scenario(scenario_rspa_p,maternites_rspa_p,prefixes_rspa_p)
print(scenario_rspa_p_annuel)

### 2.4.3. sortie -----

date<-Sys.Date()
#write_xlsx(scenario_rspa_p, paste0("Resultats/scenario_rspa_p",date,".xlsx"),col_names = TRUE)
write_xlsx(scenario_rspa_p_annuel, paste0("Data_inter/scenario_rspa_p_annuel_",date,".xlsx"),col_names = TRUE)


#3.Fusion des scenarios des différents réseaux----
# créer une liste avec toutes les listes des différents réseaux
scenarios_a_fusion<-list(scenario_rpai_annuel, scenario_aurore_i_annuel,scenario_aurore_r_annuel,scenario_rspa_p_annuel)
# initialiser une nouvelle liste vide qui comprendra les résultats fusionnées
scenario_fusionnes_annuel<-list()
# dans chaque onglet correspondant aux différentes hypothèses inf et inc testées (identiques pour chaque scenario), récupérer les données issues de chaque scenario réseau
for(onglet in names(scenario_rpai_annuel)){
  data_onglet<-data.frame()
    for (scenario in scenarios_a_fusion){
      # scenario est une liste qui contient la liste des scenario des différents réseaux. scenario[[onglet]] est un dataframe
      # rrécupérer les données de l'onglet correspondant aux bonnes hypothèses pour chaque scenario de la liste
      data_scenario<-scenario[[onglet]]
      # ajouter à un dataframe l'ensemble des données de chaque scenario
      data_onglet<-rbind(data_onglet,data_scenario)
    }
  scenario_fusionnes_annuel[[onglet]]<-data_onglet  
}
print(scenario_fusionnes_annuel)
# enrigistrer la sortie finale
write_xlsx(scenario_fusionnes_annuel, paste0("Resultats/scenario_fusionnes_annuel_",date,".xlsx"),col_names = TRUE)

