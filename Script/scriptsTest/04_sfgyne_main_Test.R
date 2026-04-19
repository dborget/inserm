# Script pour l'appel des fonctions appliquées à chaque département 

#########################################################
#APPEL FONCTIONS COMMUNES NETTOYAGE-------
#########################################################

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
SfGoAdresse<-gpmt_prof(SagefemmeGyneFinal)
print(SfGoAdresse)
write.xlsx(SfGoAdresse,"Data/sfgyne_gpt.xlsx")

#1. errgvtfgv------
#1.1 ferfgew-------
