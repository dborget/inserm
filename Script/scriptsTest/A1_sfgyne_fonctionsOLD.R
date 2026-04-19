## Script de nettoyage du data frame SageFemmeGyne----


#1. Cleaning table----



## la fonction correction_table_entree re formate la table d'entree en réalisant des modifications, suppressions, ajout de colonne. 
# La fonction prend pour argument une table obtenue à partir de la fusion de deux fichiers de l'ARS AURA (cf import des données).
# La sortie de la fonction est une table. 
formater_table_ars<-function(table){
  # renommer la colonne adresse
  table<-rename(table, "Adresse"="Adresse.de.la.structure.d.activité")
  # fusionner les colonnes noms et prénoms
  table<- unite(table, "Nom Prenom",c("nom","prenom"),remove=TRUE, sep=" ")
  # renommer les colonnes
  table<-rename(table,"nom_prenom"="Nom Prenom", "mode_exercice"="Mode.d.exercice","adresse"="Adresse", "profession"="Profession", "ville"="Ville", "structure"="Structure")
  # séparer l'adresse de la structure de son cp et de la ville
  table <- extract(table, "adresse", into=c("adresse", "CP2", "Ville2"), regex="(.*?)(\\d{5})(.*)")
  # supprimer les numeros de # rue/avenue dans les adresses
  table <- mutate(table, adresse = str_remove_all(adresse, "[^A-Za-z ]"))
  # quand CP2 est vide, donner valeur de CP
  table<-mutate(table, CP2=ifelse(CP2==is.na(CP2), CP, CP2))
  # suppression de la colonne ville2 et CP respectivement moins bien renseignées que ville et CP2. 
  table<- select(table, -c("Ville2" ,"CP"))
  # la colonne CP2 est renommée cp (code postal)
  table<-rename(table, "cp"="CP2")
  #ajouter une colonne reseau
  table<-mutate(table,reseau=NA)
  #supprimer les professionnels ayant pour profession gynecologie medicale
  table<-filter(table, profession!="Gynécologie médicale")
  #remplacer l'adresse par la valeur de la structure si il n'y a pas de valeur ou un vide dans la colonne adresse
  table<-mutate(table, adresse=ifelse(adresse==""|is.na(adresse),structure,adresse))
  return(table)
}



# la fonction nettoyage_ecriture supprime les accents, les tirets, les majuscules, les espaces en début et fin de ligne,les lettres seules en fin de lignes, écrit les mots entiers boulevard, avenue, saint.
# la fonction prend pour argument une chaine de characteres
# la fonction donne en sortie une chaine de charactères
# cette fonction peut etre utilisée sans personnalisation 
nettoyer_adresse <- function(character){
  character<-tolower(character)
  character<-str_replace_all(character, "-", " ")
  character<-str_replace_all(character, "é", "e")
  character<-str_replace_all(character, "â", "a")
  character<-str_replace_all(character, "ê", "e")
  character<-str_replace_all(character, "è", "e")
  character<-str_replace_all(character, "ç", "c")
  character<-str_replace_all(character, "à", "a")
  character<-str_replace_all(character, "bld", "boulevard")
  character<-str_replace_all(character, "bd", "boulevard")
  character<-str_replace_all(character, "\\bav\\b", "avenue")
  character<-str_replace_all(character, "\\bmte\\b", "montee")
  character<-str_replace_all(character,"\\st\\b", "saint")
  character<-str_replace(character," [a-z]$", "")
  character<-trimws(character)
  return(character)
}

#2. Nettoyage par département-----

### 2.1 Fonctions isere----
# La fonction corriger adresse permet de corriger certaines adresses qui ont des erreurs d'écriture, autoriser le comptage même si une lettre différe entre 2 adresses
# la fonction prend pour argument une chaine de characteres
# la fonction donne en sortie une chaine de charactères
corriger_adresse_isere<-function(adresse) {
  adresse<-str_replace_all(adresse, "mdiple", "medipole") 
  adresse<-str_replace_all(adresse,"flemming","fleming")
  adresse<-str_replace_all(adresse,"grand rue", "grande rue")
  adresse<-str_replace_all(adresse,"rue de la rpublique", "rue de la republique")
  adresse<-str_replace_all(adresse, "rue belgrade","rue de belgrade")
  adresse<-str_replace_all(adresse,"montee biarde", "montee de la biarde")
  return(adresse)
}

# la fonction creer structue_clean isere ajoute une colonne structure clean dans laquelle seront réalisées des modifications manuelles 
creer_structure_clean_isere<-function(table) {
  table<-mutate(table, structure_clean = adresse)
  # supprimer les doubles espaces et bp qui suit l'adresse puis supprimer double espaces et ce qu'il y avant 
  # attention suppression via les doubles espaces, ne pas déplacer l'ordre
  table$structure_clean<-str_remove(table$structure_clean, "  bp$")
  table$structure_clean<-str_replace(table$structure_clean, "du  mai", "du mai")
  table$structure_clean<-str_replace(table$structure_clean, "  mars", " mars")
  table$structure_clean<-str_replace(table$structure_clean, "  cs$", "")
  table$structure_clean<-str_replace(table$structure_clean, "^.*?\\s{2,}","")
  table$mode_exercice<-str_replace(table$mode_exercice, "^.*?\\s{2,}","")
  # renommer les adresses qui ressortent parmi les principales adresses (SfGoAdresse) // spécifique à ce dataframe
  table <- mutate(table, structure_clean = ifelse(structure_clean == "service departemental de pmi isere", "pmi", structure_clean))
  table <- mutate(table, structure_clean = ifelse(structure_clean == "boulevard de la chantourne", "chu ga", structure_clean))
  table <- mutate(table, structure_clean = ifelse(structure_clean == "avenue du medipole", "ch pierre oudot", structure_clean))
  table <- mutate(table, structure_clean = ifelse(structure_clean == "rue docteur calmette"|structure_clean =="rue du docteur calmette", "ghm gre", structure_clean))
  table <- mutate(table, structure_clean = ifelse(structure_clean == "avenue gabriel peri", "cl belledone", structure_clean))
  table <- mutate(table, structure_clean = ifelse(structure_clean == "montee du docteur chapuis", "ch lucien hussel de vienne", structure_clean))
  table <- mutate(table, structure_clean = ifelse(structure_clean == "avenue jacques chirac", "hp voiron", structure_clean))
  table <- mutate(table, structure_clean = ifelse(structure_clean == "rue albert londres", "cl des cedres", structure_clean))
  table <- mutate(table, structure_clean = ifelse(structure_clean == "avenue de grugliasco", "medicedres", structure_clean))
  table <- mutate(table, structure_clean = ifelse(structure_clean == "avenue du marquis de gresivaudan" |structure_clean == "avenue maquis du gresivaudan", "chu ga", structure_clean))
  table <- mutate(table, structure_clean = ifelse(structure_clean == "chu38", "chu ga", structure_clean))
  table <- mutate(table, ville = ifelse(adresse== "avenue jean jaures"& nom_prenom== "defrance mylene", "rives", ville))
  table <- mutate(table,  cp=ifelse(ville=="cour et buis",  "38122",cp))
  table <- mutate(table,  cp=ifelse(cp=="10217",  "38000",cp))
  table <- mutate(table, ville=ifelse(ville=="badinieres",  "eclose badinieres",ville))
  return(table)
}

#3. Ajout reseau périnnatal-----


# Reseau prend valeur différente selon la communauté de communne auquel le code commune ou code insee ou commune corrrespond.

# fonction jointure reseau isere réalise une jointure à partir de la liste des communes d'un département sur les noms de ville
# La fonction pour arguments 1 table d'entrée sage femme et gyne et 1 liste des communes du même département
# La fonction donne en sortie une table sage femme et gyne avec une colonne reseau remplie
joindre_reseau<-function(table_nettoyee, liste_commune) {
  liste_commune<-mutate(liste_commune, across(where(is.character),nettoyer_adresse))
  liste_commune_ssgpe<-liste_commune[,c("nom_standard","epci_nom","code_postal","codes_postaux")]
  # left join sur liste des communes 
  jointure_commune <-left_join(table_nettoyee, liste_commune_ssgpe, by=c("ville"="nom_standard"))
  jointure_commune <-mutate(jointure_commune, cp_dans_codes_postaux = str_detect(codes_postaux, cp, negate=FALSE))
  print(summary(jointure_commune$cp_dans_codes_postaux))
  return(jointure_commune)
}

# fonction à personnaliser pour associer le réseau périnatal selon le nom des communautés de communes
completer_reseau_isere<-function(jointure_commune){
    jointure_commune$reseau<- case_when(
    jointure_commune$epci_nom=="ca vienne condrieu"|jointure_commune$epci_nom=="cc collines isere nord communaute"|jointure_commune$epci_nom=="ca porte de l'isere (capi)"|jointure_commune$epci_nom=="cc les balcons du dauphine"|jointure_commune$epci_nom== "cc lyon saint exupery en dauphine"~ "aurore", 
    TRUE ~ "rpai"
  )
  return(jointure_commune)
}

#4. Groupement professionnels----
# La fonction groupement prof permet de faire ressortir les lieux d'exercice avec plus de 3 professionnels (sages femmes ou gynécologues).
# la fonction a pour argument la table nettoyee contenant la liste des sages femmees et gyne d'un département. 
# la fonction a pour sortie une table avec les adresses présentes au moins 3 fois. 

gpmt_prof<-function(table_finale){
  adresse_groupe<-group_by(table_finale, structure_clean, ville, reseau) 
  SfGoAdresse<-summarise(adresse_groupe, nb_professionnel=n_distinct(nom_prenom),.groups="keep")
  SfGoAdresse<-filter(SfGoAdresse, nb_professionnel>3)
  SfGoAdresse<-arrange(SfGoAdresse, desc(nb_professionnel))
  print(paste0("il y a ", nrow(SfGoAdresse),  " adresses "))
  return(SfGoAdresse)
}


