#### Script de traitement de l'entree SagefemmeGyneFinal




######################################################
############## GROUPEMENTS DE PROFESSIONNELS
######################################################

#faire ressortir les adresses qui ressortent plus de 3 fois. summarise compte combien de regroupements par meme adresse.comme une ligne est un professionnel, n correspond au nombre de professionnel
adresse_groupe<-group_by(SagefemmeGyneFinal, structure_clean, ville, reseau)
#SfGoAdresse<-summarise(adresse_groupe, nb_professionnel=n(),.groups="keep")
SfGoAdresse<-summarise(adresse_groupe, nb_professionnel=n_distinct(nom_prenom),.groups="keep")
SfGoAdresse<-filter(SfGoAdresse, nb_professionnel>3)
SfGoAdresse<-arrange(SfGoAdresse, desc(nb_professionnel))
print(SfGoAdresse)

#Test : SfGoAdresse sans condition de nb_prof>3
SfGoAdresse<-summarise(adresse_groupe, nb_professionnel=n_distinct(nom_prenom),.groups="keep")
SfGoAdresse<-arrange(SfGoAdresse, desc(nb_professionnel))
nrow(SfGoAdresse)

# parmi les adresses qui ressortent, corriger les structure_clean vide ou NA 
filter(SagefemmeGyneFinal, structure=="service departemental de pmi isere")
filter(SagefemmeGyneFinal, structure_clean=NA)


#####################################################
############ description des données 
#####################################################

# connaître les professionnels d'un établissement 
distinct(select(filter(SagefemmeGyneFinal, structure_clean=="boulevard de la chantourne"),nom_prenom))
select(filter(SagefemmeGyneFinal, structure_clean=="route des gorges"), nom_prenom)
#nombre de professionnels par profession
count(SagefemmeGyneFinal, profession)
#nombre de villes différentes
summarise(SagefemmeGyneFinal, nb_ville=n_distinct(ville)) 
n_distinct(SagefemmeGyneFinal$ville)

# tests supplémentaires 
print(filter(SagefemmeGyneFinal, SagefemmeGyneFinal$adresse=="rue de la rpublique")) 
