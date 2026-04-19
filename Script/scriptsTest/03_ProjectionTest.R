####### Script pour la création de la table de projection

################################
#### LIBRAIRIES
################################
library(dplyr)

################################
### VARIABLES D'ENTREES
################################

# Data frame entree : maternités et le nombre d'accouchements par maternité pour le RPAI
# La table d'entrée dépend du réseau. Il s'agit du total d'accouchement par an. Il est modulé par le nombre de mois. 
maternite<-c("hce", "ghm","voi", "bel", "ced")
accouchement<-c(2483,1333,1266,1465,759)*length(mois)/12
statut<-c("public","prive non lucratif", "public", "prive", "prive")
table_entree_rpai <-data.frame(maternite,accouchement,statut)
table_entree_rpai

# variable mois : modifiable pour faire sur plusieurs années : changer la length.out
mois<-seq(as.Date("2027-01-01"), by="month", length.out=12) 
# inf : % de femmes informées sur la possiblité de rejoindre Filomene
inf<-c(25/100, 30/100, 35/100)
# inc: % de femmes acceptant de participer à Filomene
# à qui on a présenté la cohorte ? ou pas forcément ?
inc<-c(10/100,15/100,20/100)

#########################################
### FONCTION CREATION DE TABLE PROJECTION
#########################################

#fonction qui avec une table en entrée et les % de la projection, sortira une table en sortie
# Attention, la table d'entree doit contenir une colonne maternite et une colonne accouchement

scenario<- function(table, var_inf, var_inc, mois) {
  
  # Récupère les noms uniques des maternités -> colonne 1 de la table 
  maternite <- unique(table$maternite)
  # Remettre la liste à vide
  acc_lisse<-list()
  #boucle pour récupérer le nombre d'accouchement pour chaque maternite de la table d'entrée. Le nombre d'accouchement est lissé selon le nombre de mois.
  for (i in seq_along(maternite)) {
    nom_mater<-paste0(maternite[i],"_acc")
    valeur_acc<-round((table[table$maternite==maternite[i], "accouchement"] / length(mois)), digits=0)
    acc_lisse[[nom_mater]]<-rep(valeur_acc,times=length(mois))
  }
  #table_accouchement_lisse : chaque élément de la liste devient une colonne dont le nom est le nom des éléments de la liste
  table_acc_lisse<-as.data.frame(acc_lisse)
  #projection : table pour les projections
  projection<-as.data.frame(acc_lisse,mois)
  #ajouter les colonnes informées et incluses calculant le nombre de potentielles inclusions arrondies à partir de la table_acc_lisse
  proj_inf<-round(table_acc_lisse*var_inf, digits=0)
  proj_inc<-round(table_acc_lisse*var_inf*var_inc, digits=0)
  projection<-cbind(projection,proj_inf,proj_inc)
  
  #les colonnes de chaque maternité ont le même nom, make.unique ajoute un suffixe pour les distinguer.
  colnames(projection) <- make.unique(colnames(projection))
  #fonction remplacer_num : renommer les colonnes créées
  remplacer_num <- function(nom_colonne) {
    nom_colonne<-str_replace_all(nom_colonne, "acc\\.1", "inf")
    nom_colonne<-str_replace_all(nom_colonne,"acc\\.2", "inc")
    return(nom_colonne)
  }
  #appliquer la fonction
  projection<-rename_with(projection, remplacer_num, .cols = matches("\\.1|\\.2"))
  
  return(projection)
}
# test pour 1 scenario pour le reseau rpai 
inf_test<-40/100
inc_test<-10/100
scenario(table_entree_rpai, inf_test, inc_test, mois)

##############################################
######### BOUCLE POUR PLUSIEURS SCENARIO
##############################################

# initialiser liste, qui se remplira des scenario
resultat_scenario<-list()
# boucle avec toutes les hypothèses de inf et inc 
for (i in seq_along(inf)){
  for (j in seq_along(inc)){
    nom_scenario<-paste0("inf=", inf[i],"_inc=",inc[j])
    resultat_scenario[[nom_scenario]]<-scenario(table_entree_rpai, inf[i], inc[j], mois)
  }
}
# tous les resultats sont dans la liste resultat scenario
print(resultat_scenario)
#appeler un resultat
resultat_scenario[["inf=0.25_inc=0.1"]]
resultat_scenario[2]  

###############################################
############# SORTIES DANS UN FICHIER ????
###############################################

# exporter les scneario dans un cvs : ou dans plusieurs csv (1 par scenario) ? 
# write.csv2(resultat_scenario, file="resultat_scenario.csv", fileEncoding="UTF-8")
# for (i in names(resultat_scenario){
  #write.csv2(resultats[[i]], file=paste0("resultat_scenario",i,".csv"))
# excel en onglet : 
library(writexl)
date<-Sys.Date()
write_xlsx(resultat_scenario, paste0("Data/resultat_scenario_",date,".xlsx"),col_names = TRUE)


###############################################
############# ANNUALISER LES RESULTATS
###############################################
# test 1 à la main  
scenario_rpai_annuel<- list()
scenario_rpai_annuel[[scenario]] <- data.frame(
  Établissement = c("HCE", "GHM de Grenoble", "CH Voiron", "Clinique Belledone", "Clinique des Cèdres"),
  Accouchement= c(
    sum(scenario_rpai[[scenario]]$hce_acc), sum(scenario_rpai[[scenario]]$ghm_acc), sum(scenario_rpai[[scenario]]$voi_acc),
    sum(scenario_rpai[[scenario]]$bel_acc), sum(scenario_rpai[[scenario]]$ced_acc)
  ),
  Informees = c(
    sum(scenario_rpai[[scenario]]$hce_inf), sum(scenario_rpai[[scenario]]$ghm_inf), sum(scenario_rpai[[scenario]]$voi_inf),
    sum(scenario_rpai[[scenario]]$bel_inf), sum(scenario_rpai[[scenario]]$ced_inf)
  ),
  Incluses = c(
    sum(scenario_rpai[[scenario]]$hce_inc), sum(scenario_rpai[[scenario]]$ghm_inc), sum(scenario_rpai[[scenario]]$voi_inc),
    sum(scenario_rpai[[scenario]]$bel_inc), sum(scenario_rpai[[scenario]]$ced_inc)
  )
)
}
print(scenario_rpai_annuel)
# test 2 : faire une boucle
# pour le rpai
maternites <- c("HCE", "GHM de Grenoble", "CH Voiron", "Clinique Belledone", "Clinique des Cèdres")
# utiiliser les prefixes des colonnes, pour récupérer l'informations selon si colonnes acc, inf, inc
prefixes <- c("hce_", "ghm_", "voi_", "bel_", "ced_")  
scenario_rpai_annuel<- list()

for (scenario in names(scenario_rpai)) {
  #innitialisation du dataframe 
  etablissements<-maternites
  accouchements<-numeric()
  informees<-numeric()
  incluses<-numeric()
  # boucle pour sommer 
  for (i in seq_along(maternites)){
    accouchements[i]<-sum(scenario_rpai[[scenario]][[paste0(prefixes[i], "acc")]])
    informees[i]<-sum(scenario_rpai[[scenario]][[paste0(prefixes[i], "inf")]])
    incluses[i]<-sum(scenario_rpai[[scenario]][[paste0(prefixes[i], "inc")]])
  }
  # remplir le dataframe 
  scenario_rpai_annuel[[scenario]] <- data.frame(etablissements, accouchements, informees, incluses)
}
print(scenario_rpai_annuel[["inf=0.25_inc=0.1"]])

# test fonction


annualiser_scenario<-function(table_scenario){
  scenario_annuel<- list()
  for (scenario in names(table_scenario)) {
    scenario_annuel[[scenario]] <- data.frame(
      Établissement = c("HCE", "GHM de Grenoble", "CH Voiron", "Clinique Belledone", "Clinique des Cèdres"),
      Accouchement= c(
        sum(table_scenario[[scenario]]$hce_acc), sum(table_scenario[[scenario]]$ghm_acc), sum(table_scenario[[scenario]]$voi_acc),
        sum(table_scenario[[scenario]]$bel_acc), sum(table_scenario[[scenario]]$ced_acc)
      ),
      Informees = c(
        sum(table_scenario[[scenario]]$hce_inf), sum(table_scenario[[scenario]]$ghm_inf), sum(table_scenario[[scenario]]$voi_inf),
        sum(table_scenario[[scenario]]$bel_inf), sum(table_scenario[[scenario]]$ced_inf)
      ),
      Incluses = c(
        sum(table_scenario[[scenario]]$hce_inc), sum(table_scenario[[scenario]]$ghm_inc), sum(table_scenario[[scenario]]$voi_inc),
        sum(table_scenario[[scenario]]$bel_inc), sum(table_scenario[[scenario]]$ced_inc)
      )
    )
  }
  return(scenario_annuel)
}

scenario_rpai_annuel<- list()

for (scenario in names(scenario_rpai)) {
  scenario_rpai_annuel[[scenario]] <- data.frame(
    Établissement = c("HCE", "GHM de Grenoble", "CH Voiron", "Clinique Belledone", "Clinique des Cèdres"),
    Accouchement= c(
      sum(scenario_rpai[[scenario]]$hce_acc), sum(scenario_rpai[[scenario]]$ghm_acc), sum(scenario_rpai[[scenario]]$voi_acc),
      sum(scenario_rpai[[scenario]]$bel_acc), sum(scenario_rpai[[scenario]]$ced_acc)
    ),
    Informees = c(
      sum(scenario_rpai[[scenario]]$hce_inf), sum(scenario_rpai[[scenario]]$ghm_inf), sum(scenario_rpai[[scenario]]$voi_inf),
      sum(scenario_rpai[[scenario]]$bel_inf), sum(scenario_rpai[[scenario]]$ced_inf)
    ),
    Incluses = c(
      sum(scenario_rpai[[scenario]]$hce_inc), sum(scenario_rpai[[scenario]]$ghm_inc), sum(scenario_rpai[[scenario]]$voi_inc),
      sum(scenario_rpai[[scenario]]$bel_inc), sum(scenario_rpai[[scenario]]$ced_inc)
    )
  )
}
print(scenario_rpai_annuel)