### Script de nettoyage du data frame SageFemmeGyne

# import des librairies utiles pour le nettoyage de la table
library(tidyr)
library(dplyr)
library(stringr)

# renommer la colonne adresse
SagefemmeGyne<-rename(SagefemmeGyne, "Adresse"="Adresse.de.la.structure.d.activité")
# fusionner les colonnes noms et prénoms
SagefemmeGyne<- unite(SagefemmeGyne, "Nom Prenom",c("nom","prenom"),remove=TRUE, sep=" ")
names(SagefemmeGyne)
# renommer les colonnes
SagefemmeGyne<-rename(SagefemmeGyne,"nom_prenom"="Nom Prenom", "mode_exercice"="Mode.d.exercice","adresse"="Adresse", "profession"="Profession", "ville"="Ville", "structure"="Structure")
# séparer l'adresse de la structure de son cp et de la ville
SagefemmeGyne <- extract(SagefemmeGyne, "adresse", into=c("adresse", "CP2", "Ville2"), regex="(.*?)(\\d{5})(.*)")
summary(SagefemmeGyne$CP=="NA")

# supprimer les numeros de # rue/avenue dans les adresses
SagefemmeGyne <- mutate(SagefemmeGyne, adresse = str_remove_all(adresse, "[^A-Za-z ]"))
#supprimer les espaces au début et fin des colonnes?

#verif les 2 colonnes CP et CP2 des SF 
SagefemmeGyne$CP2<-as.numeric(SagefemmeGyne$CP2)
summary(SagefemmeGyne$profession == "Sage-Femme" & SagefemmeGyne$CP2 != SagefemmeGyne$CP)
summary(SagefemmeGyne$ville=="NA")
# il y a 31 lignes avec des valeurs d'adresse manquantes.Laisser ces lignes pour conserver les noms des professionnels associés. Traitement des adresses plus bas. 
# quand CP2 est vide, donner valeur de CP
class(SagefemmeGyne$CP2)
SagefemmeGyne<-mutate(SagefemmeGyne, CP2=ifelse(is.na(CP2), CP, CP2))
summary(SagefemmeGyne$CP2=="NA")

# suppression de la colonne ville2 qui est moins bien renseignée que ville. 
# suppresion de la colonne CP qui contient parfois uniquement le numéro de département 38 et pas le code postal. suppresion de la colonne CP. 
SagefemmeGyne<- select(SagefemmeGyne, -c("Ville2" ,"CP"))
# la colonne CP2 est renommée cp
SagefemmeGyne<-rename(SagefemmeGyne, "cp"="CP2")
# est-ce utile de mettre le CP en numeric (pour réaliser des group by par la suite) : SagefemmeGyne4$CP<-as.numeric(SagefemmeGyne4$CP)

# la fonction nettoyage_ecriture supprime les accents, les tirets, les majuscules, les espaces en début et fin de ligne, écrit les mots entiers boulevard, avenue
nettoyage_adresse <- function(character){
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
  #character<-iconv(character, from="UTF-8", to = "Latin1")
  return(character)
}
sapply(SagefemmeGyne, class)
#appliquer la fonction nettoyage ecriture 
SagefemmeGyne<-mutate(SagefemmeGyne, across(where(is.character),nettoyage_adresse))

#La fonction suivante permet de corriger certaines adresses qui ont des erreurs d'écriture, autoriser le comptage même si une lettre différe entre 2 adresses
#remplacement à la main
correction_adresse<-function(adresse) {
  adresse<-str_replace_all(adresse, "mdiple", "medipole") 
  adresse<-str_replace_all(adresse,"flemming","fleming")
  adresse<-str_replace_all(adresse,"grand rue", "grande rue")
  adresse<-str_replace_all(adresse,"rue de la rpublique", "rue de la republique")
  adresse<-str_replace_all(adresse, "rue belgrade","rue de belgrade")
  adresse<-str_replace_all(adresse,"montee biarde", "montee de la biarde")
  return(adresse)
}
SagefemmeGyne<-mutate(SagefemmeGyne, adresse=correction_adresse(adresse))
#remplacer l'adresse par la valeur de la structure si il n'y a pas de valeur dans l'adresse
SagefemmeGyne<-mutate(SagefemmeGyne, adresse=ifelse(adresse=="",structure,adresse))
SagefemmeGyne<-mutate(SagefemmeGyne, adresse=ifelse(is.na(adresse),structure,adresse))

#créer une colonne structure_clean à partir des adresses de SFGyne5. 
SagefemmeGyne<-mutate(SagefemmeGyne, structure_clean = adresse)
filter(SagefemmeGyne,is.na(adresse))

#nettoyer la colonne structure_clean, spécifique à ce dataframe (reproductibe sur autres département?) 
#afficher les lignes concernées par les doubles espaces 
pull(filter(SagefemmeGyne,str_detect(adresse,"  ")),adresse)
pull(filter(SagefemmeGyne,str_detect(structure_clean,"  ")),structure_clean)

# supprimer les doubles espaces et bp qui suit l'adresse puis supprimer double espaces + ce qu'il y avant 
# attention suppression via les doubles espaces, ne pas déplacer l'ordre
SagefemmeGyne$structure_clean<-str_remove(SagefemmeGyne$structure_clean, "  bp$")
SagefemmeGyne$structure_clean<-str_replace(SagefemmeGyne$structure_clean, "du  mai", "du mai")
SagefemmeGyne$structure_clean<-str_replace(SagefemmeGyne$structure_clean, "  mars", " mars")
SagefemmeGyne$structure_clean<-str_replace(SagefemmeGyne$structure_clean, "  cs$", "")
SagefemmeGyne$structure_clean<-str_replace(SagefemmeGyne$structure_clean, "^.*?\\s{2,}","")
#supprimer les doubles espaces de la colonne Mode d'exercice
SagefemmeGyne$mode_exercice<-str_replace(SagefemmeGyne$structure_clean, "^.*?\\s{2,}","")

#supprimer les professionnels ayant pour profession gynecologie medicale
SagefemmeGyne<-filter(SagefemmeGyne, profession!="gynecologie medicale")

# renommer les adresses qui ressortent parmi les principales adresses (SfGoAdresse) // spécifique à ce dataframe
#SagefemmeGyne8<-SagefemmeGyne7[SagefemmeGyne8$Structure_clean == "service departemental de pmi isere"] <- "pmi"
SagefemmeGyne <- mutate(SagefemmeGyne, structure_clean = ifelse(structure_clean == "service departemental de pmi isere", "pmi", structure_clean))
SagefemmeGyne <- mutate(SagefemmeGyne, structure_clean = ifelse(structure_clean == "boulevard de la chantourne", "chu ga", structure_clean))
SagefemmeGyne <- mutate(SagefemmeGyne, structure_clean = ifelse(structure_clean == "avenue du medipole", "ch pierre oudot", structure_clean))
SagefemmeGyne <- mutate(SagefemmeGyne, structure_clean = ifelse(structure_clean == "rue docteur calmette"|structure_clean =="rue du docteur calmette", "ghm gre", structure_clean))
SagefemmeGyne <- mutate(SagefemmeGyne, structure_clean = ifelse(structure_clean == "avenue gabriel peri", "cl belledone", structure_clean))
SagefemmeGyne <- mutate(SagefemmeGyne, structure_clean = ifelse(structure_clean == "montee du docteur chapuis", "ch lucien hussel de vienne", structure_clean))
SagefemmeGyne <- mutate(SagefemmeGyne, structure_clean = ifelse(structure_clean == "avenue jacques chirac", "hp voiron", structure_clean))
SagefemmeGyne <- mutate(SagefemmeGyne, structure_clean = ifelse(structure_clean == "rue albert londres", "cl des cedres", structure_clean))
SagefemmeGyne <- mutate(SagefemmeGyne, structure_clean = ifelse(structure_clean == "avenue de grugliasco", "medicedres", structure_clean))
SagefemmeGyne <- mutate(SagefemmeGyne, structure_clean = ifelse(structure_clean == "avenue du marquis de gresivaudan" |structure_clean == "avenue maquis de gresivaudan", "chu ga", structure_clean))
SagefemmeGyne <- mutate(SagefemmeGyne, structure_clean = ifelse(structure_clean == "chu38", "chu ga", structure_clean))

# autres corrections
SagefemmeGyne <- mutate(SagefemmeGyne, ville = ifelse(adresse== "avenue jean jaures"& nom_prenom== "defrance mylene", "rives", ville))
SagefemmeGyne<-mutate(SagefemmeGyne,  cp=ifelse(ville=="cour et buis",  "38122",cp))
SagefemmeGyne<-mutate(SagefemmeGyne,  cp=ifelse(cp=="10217",  "38000",cp))
# le vrai nom de ville de badinieres est Eclose Badinieres. (Badinieres n'est pas dans la table initale des communes de l'Isère).
SagefemmeGyne<-mutate(SagefemmeGyne, ville=ifelse(ville=="badinieres",  "eclose badinieres",ville))


#dernière version avec nettoyage
SagefemmeGyneFinal<-SagefemmeGyne

summary(SagefemmeGyneFinal$adresse=="NA")
summary(SagefemmeGyneFinal$structure_clean=="NA")
#write.csv2(SagefemmeGyneFinal, file="Data/SagefemmeGyneFinal.csv")


################################################
################### AJOUT COLONNE RESEAU
################################################
#Reseau prend valeur différente selon la communauté de communne auquel le code commune ou code insee ou commune corrrespond.
#convertir le cp en double ?
#SagefemmeGyne$cp<-as.numeric(SagefemmeGyne$cp)
#left join car on veut garder toute la table SagefemmeGyneFinal même si pas de correspondance. Si pas de correspondance, il y aura des NA.
SagefemmeGyneFinal<-mutate(SagefemmeGyneFinal,reseau=NA)
code_commune<-mutate(code_commune, across(where(is.character),nettoyage_adresse))

paste0(("il y a "), nrow(code_commune), " lignes")
code_commune_ssgpe<-code_commune[,c("nom_standard","epci_nom","code_postal","codes_postaux")]

# 1er test : joindre sur code insee 
# jointure_codeinsee<-left_join(SagefemmeGyneFinal, code_commune_ssgpe, by=c("cp"="code_insee"))
# head(jointure_codeinsee)
# compter combien de similarité et de différence
nb_communes_identiques_insee <- sum(trimws(jointure_codeinsee$ville) == trimws(jointure_codeinsee$nom_standard), na.rm=TRUE)
print(nb_communes_identiques_insee)
summary(jointure_codeinsee$ville=="NA")
summary(jointure_codeinsee$nom_standard=="NA")
nb_communes_diff_insee <- sum(jointure_codeinsee$ville != jointure_codeinsee$nom_standard, na.rm=TRUE)
print(nb_communes_diff_insee) 

# 2 eme test : joindre sur les codes postaux. mais codes postaux dans meme colonne donc les séparer

# codes postaux en charactere pour ensuite séparer les lignes
#code_commune$codes_postaux<-as.character(code_commune$codes_postaux)
#code_commune<-separate_rows(code_commune, codes_postaux, sep=",")
#code_commune_ssgpe$codes_postaux<-as.numeric(code_commune_ssgpe$codes_postaux)
#code_commune_ssgpe$codes_postal<-as.numeric(code_commune_ssgpe$code_postal)
#jointure_codepostal<-left_join(SagefemmeGyneFinal, code_commune_ssgpe, by=c("cp"="code_postal"))
#verif
nb_communes_identiques_cp <- sum(trimws(jointure_codepostal$ville) == trimws(jointure_codepostal$nom_standard), na.rm=TRUE)
print(nb_communes_identiques_cp)
summary(jointure_codepostal$ville=="NA")
summary(jointure_codepostal$nom_standard=="NA")
nb_communes_diff_cp <- sum(jointure_codepostal$ville != jointure_codepostal$nom_standard, na.rm=TRUE)
print(nb_communes_diff_cp)


# 3 eme test : joindre sur les noms des villes : test retenu
# vérification qu'il y. a une seule ligne par nom_standard (ville)
unique(code_commune_ssgpe["nom_standard"])


#code_commune_ssgpe$code_postal<-as.character(code_commune_ssgpe$code_postal)
#code_commune_ssgpe$codes_postaux<-as.character(code_commune_ssgpe$codes_postaux)
#SagefemmeGyneFinal$cp<-as.character(SagefemmeGyneFinal$cp)
jointure_commune<-left_join(SagefemmeGyneFinal, code_commune_ssgpe, by=c("ville"="nom_standard"))

#compter quand les cp sont identiques (sans tenir compte des NA, sinon le résultat renvoie NA)
nb_communes_identiques_com <- sum(trimws(jointure_commune$cp) == trimws(jointure_commune$code_postal), na.rm=TRUE)
paste("nb de communes identiques avec le join sur la communne :", nb_communes_identiques_com)

nb_communes_diff_com <- sum(trimws(jointure_commune$cp) != trimws(jointure_commune$code_postal), na.rm=TRUE)
paste("nb de communes différente avec le join sur la commune :", nb_communes_diff_com)
# Une ville peut avoir différents codes postaux. Plutôt vérifier entre la colonne cp et codes postaux. 
# La colonne vérifier que le cp de la table SageFemmeGynefinal soit compris dans la liste des codes postaux de la table communes isère. 
# cp doit etre de la meme classe que codes postaux, ici en charactère 
jointure_commune <-mutate(jointure_commune, cp_dans_codes_postaux = str_detect(codes_postaux, cp, negate=FALSE))
#vérification des cas où le cp n'est pas dans codes postaux. 
summary(jointure_commune$cp_dans_codes_postaux)
# 1 cas 
# filter(jointure_commune,is.na(jointure_commune$cp_dans_codes_postaux))
# badinieres n'est pas dans la table initale des communes de l'Isère. Il s'agit d'Eclose badinieres, changer le nom dans cleandig
head(jointure_commune)
unique(jointure_commune$epci_nom)
# associer le réseau selon la com-com.reseau aurore si com-com est l'une des com-com suivantes, sinon reseau prend la valeur RPAI
jointure_commune$reseau<- case_when(
  jointure_commune$epci_nom=="ca vienne condrieu"|jointure_commune$epci_nom=="cc collines isere nord communaute"|jointure_commune$epci_nom=="ca porte de l'isere (capi)"|jointure_commune$epci_nom=="cc les balcons du dauphine"|jointure_commune$epci_nom== "cc lyon saint exupery en dauphine"~ "aurore", 
  TRUE ~ "rpai"
)
# réattribuer les valeurs de réseau à la table SagefemmeGyneFinal (en supprimant lse colonnes utiles pour le join)
SagefemmeGyneFinal<-select(jointure_commune,-c("epci_nom","code_postal","codes_postaux","cp_dans_codes_postaux"))
head(SagefemmeGyneFinal)
