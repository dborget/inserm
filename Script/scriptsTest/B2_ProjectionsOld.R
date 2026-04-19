# Script d'application de la fonction projection, département par département


# variable mois : modifiable pour faire sur plusieurs années : changer la length.out
mois<-seq(as.Date("2027-01-01"), by="month", length.out=12) 
# inf : % de femmes informées sur la possiblité de rejoindre Filomene
inf<-c(25/100, 30/100, 35/100)
# inc: % de femmes acceptant de participer à Filomene
inc<-c(10/100,15/100,20/100)

###############################
### ISERE RPAI
################################

### variables d'entrée ISERE RPAI
# Imprt Data frame entree : maternités et le nombre d'accouchements par maternité pour le RPAI
# La table d'entrée dépend du réseau. Il s'agit du total d'accouchement par an. Il est modulé par le nombre de mois. 
table_entree_rpai <- read.csv2("Data_raw/mater_acc_IsereRPAI.csv", header=TRUE)
table_entree_rpai <-mutate(table_entree_rpai, accouchement=accouchement*length(mois)/12)
table_entree_rpai <- mutate(table_entree_rpai, maternite=recode(maternite, "HCE"="hce","GHM de Grenoble"="ghm","CH Voiron"="voi", "Clinique  Belledone "="bel","Clinique des Cedres"="ced"))

### appel de la fonction creer scenario
scenario_rpai_i<-lancer_scenario(table_entree_rpai)

### SORTIES DANS UN FICHIER 
# excel avec un scenario par onglet, titre de l'excel avec la date du jour
date<-Sys.Date()
write_xlsx(scenario_rpai_i, paste0("Data/scenario_rpai",date,".xlsx"),col_names = TRUE)


###############################
### ISERE AURORE
################################


### variables d'entrée ISERE AURORE
# Imprt Data frame entree : maternités et le nombre d'accouchements par maternite
# La table d'entrée dépend du réseau. Il s'agit du total d'accouchement par an. Il est modulé par le nombre de mois. 
table_entree_aurore_i <- read.csv2("Data_raw/mater_acc_IsereAurore.csv", header=TRUE)
table_entree_aurore_i <-mutate(table_entree_aurorei, accouchement=accouchement*length(mois)/12)
table_entree_aurore_i <- mutate(table_entree_aurore_i, maternite=recode(maternite, "CH PIERRE OUDOT BOURGOIN JALLIEU"="oud", "CH LUCIEN HUSSEL DE VIENNE"="luc","CLINIQUE SAINT VINCENT DE PAUL"="stv"))

### appel de la fonction creer scenario
scenario_aurore_i<-lancer_scenario(table_entree_aurore_i)
# tous les resultats sont dans la liste resultat scenario
print(scenario_aurore_i)

### SORTIES DANS UN FICHIER 
# excel avec un scenario par onglet, titre de l'excel avec la date du jour
date<-Sys.Date()
write_xlsx(scenario_aurore_i, paste0("Data/scenario_aurore_i",date,".xlsx"),col_names = TRUE)



###############################
### RHONE AURORE 
################################

### variables d'entrée RHONE AURORE
# Import Data frame entree : maternités et le nombre d'accouchements par maternite
# La table d'entrée dépend du réseau. Il s'agit du total d'accouchement par an. Il est modulé par le nombre de mois. 
table_entree_aurore_r <- read.csv2("Data_raw/mater_acc_RhoneAurore.csv", header=TRUE)
table_entree_aurore_r <-mutate(table_entree_aurorer, accouchement=accouchement*length(mois)/12)
# changer le nom des maternites pour des noms plus simples et courts
table_entree_aurore_r <- mutate(table_entree_aurore_r, maternite=recode(maternite, "CH montgelas"="mon", "Ch de ste FOY les LYON"="foy", "Hp femme mere enfant HCL"="hfme", "Hopital mere enfant natecia"="nat","medipole hp mutualiste" ="med" ,"clinique du val d'ouest vendome"="ven","groupement fp mutualiste les portes du sud"="ghm", "hp lyon sud HCL "="lsud", "hp croix rousse hcl" ="croix", "ch st joseph st luc" ="jos","ch nord ouest villefranche"="vil","polyclinique du beaujolais"="poly" ))

### appel de la fonction creer scenario
scenario_aurore_r<-lancer_scenario(table_entree_aurore_r)
# tous les resultats sont dans la liste resultat scenario
print(scenario_aurore_r)

### SORTIES DANS UN FICHIER 
# excel avec un scenario par onglet, titre de l'excel avec la date du jour
date<-Sys.Date()
write.xlsx(scenario_aurore_r, paste0("Data/scenario_aurore_r",date,".xlsx"),col_names = TRUE)


