####### Script pour la création de la table de projection


#1. CREATION DE TABLE DE PROJECTION -----

# La fonction creer scenario créee des scenario du nombre de femmes à inclure à partir de variables d'entrée. 
# la fonction prend pour argument une table d'entrée avec le noms des maternités et le nb d'accouchement, deux veecteurs de % pour les femmes informés et incluses, la variable mois. 
# La sortie est une liste projection pour les scenario

creer_scenario<- function(table, var_inf, var_inc, mois) {
  
  # Récupérer les noms uniques des maternités -> de la colonne 1 de la table 
  maternite <- unique(table$maternite)
  # Remettre la liste à vide
  acc_lisse<-list()
  #boucle pour récupérer le nombre d'accouchement pour chaque maternite de la table d'entrée. Le nombre d'accouchement est lissé selon le nombre de mois.
  for (i in seq_along(maternite)) {
    nom_mater<-paste0(maternite[i],"_acc")
    valeur_acc<-round((table[table$maternite==maternite[i], "accouchement"] / length(mois)), digits=0)
    acc_lisse[[nom_mater]]<-rep(valeur_acc,times=length(mois))
  }
  # table_accouchement_lisse : chaque élément de la liste acc_lisse devient une colonne 
  table_acc_lisse<-as.data.frame(acc_lisse)
  # créer un data frame avec une colonne mois et le nombre d'accouochement associé 
  projection<-as.data.frame(acc_lisse,mois)
  # calculer le nombre d'informées et incluses, arrondi à l'entier à partir de la table_acc_lisse
  proj_inf<-round(table_acc_lisse*var_inf, digits=0)
  proj_inc<-round(table_acc_lisse*var_inf*var_inc, digits=0)
  projection<-cbind(projection,proj_inf,proj_inc)
  
  # les colonnes de chaque maternité ont le même nom, make.unique ajoute un suffixe pour les distinguer.
  colnames(projection) <- make.unique(colnames(projection))
  # fonction remplacer_num : renommer les colonnes créées avec les suffixes inf (pour informéees) et inc(pour incluses)
  remplacer_num <- function(nom_colonne) {
    nom_colonne<-str_replace_all(nom_colonne, "acc\\.1", "inf")
    nom_colonne<-str_replace_all(nom_colonne,"acc\\.2", "inc")
    return(nom_colonne)
  }
  # appliquer la fonction à remplacer num des colonnes par les préfixes inf et inc
  projection<-rename_with(projection, remplacer_num, .cols = matches("\\.1|\\.2"))
  
  return(projection)
}

# La fonction lancer scenario créee les scenario pour une table d'entree donnée
# la fonction prend pour argument une table d'eentrée pour un réseau d'un département 
# La sortie est une liste de scenario
lancer_scenario<-function(table_entree){
  scenario<-list()
  # boucle qui applique la fonction creeer scenario pour chaque couple de valeur inf et inc. le resultat est stockée dans la liste 
  for (i in seq_along(inf)){
    for (j in seq_along(inc)){
      nom_scenario<-paste0("inf=", inf[i],"_inc=",inc[j])
      scenario[[nom_scenario]]<-creer_scenario(table_entree, inf[i], inc[j], mois)
    }
  }
  return(scenario)
}

#2. ANNUALISER LA PROJECTION ------
# La fonction annualiser scenario agrege les données de la table scenario d'un réseau pour donner par mateernités du nombre annuel d'accouchement, d'informees, d'incluses
# la fonction prend pour arguments une table scenario, la liste des maternites et les prefixes (à savoir les maternités en abrégées permeettant d'identifier les colonnes à utiliser pour les sommes)
# La sortie est une liste de scenario avec des totaux annualisés
annualiser_scenario<-function(table_scenario, maternites, prefixes){
scenario_annuel<- list()
for (scenario in names(table_scenario)) {
#innitialisation du dataframe
  etablissements<-maternites
  accouchements<-numeric()
  informees<-numeric()
  incluses<-numeric()
# boucle pour sommer les valeurs de chaque colonne (sur 12 mois)
  for (i in seq_along(maternites)){
  accouchements[i]<-sum(table_scenario[[scenario]][[paste0(prefixes[i], "acc")]])
  informees[i]<-sum(table_scenario[[scenario]][[paste0(prefixes[i], "inf")]])
  incluses[i]<-sum(table_scenario[[scenario]][[paste0(prefixes[i], "inc")]])
}
# remplir la liste. chaque élément de la liste est un dataframe
scenario_annuel[[scenario]] <- data.frame(etablissements, accouchements, informees, incluses)
}
return(scenario_annuel)
}



