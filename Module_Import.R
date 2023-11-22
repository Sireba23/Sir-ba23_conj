

#********* Modélisation : Projet ConjontureR************************************************
#*******************************************************************************************



           #**************1.Importation  Base de donnée Historique**********************
           #**************1.Importation Base de données historique ******************

rm(list = ls())

#getwd()
#setwd("C:/Users/sire.ba/Documents/Travail_collectif_git")


DH <- "L:/04- Conjoncture/06- ConjonctureR/3.Historique_des_Séries"
Donnees_Histo <- list()
fichiers_histo <- list.files(path = DH, pattern = ".xlsx", full.names = TRUE)

for (fichier in fichiers_histo) {
  if (!grepl("~$", basename(fichier))) {
    tryCatch({
      donnees <- read_xlsx(fichier)
      Donnees_Histo[[basename(fichier)]] <- donnees
    }, error = function(e) {
      cat("Erreur lors de l'importation du fichier:", basename(fichier), "\n")
      cat("Message d'erreur:", conditionMessage(e), "\n")
    })
  }
}


                   #********** Fusion des données historiques ********
                   #********** Fusion des données historiques ********
                   

Base_1 <- Donnees_Histo[[1]]
for (i in 2:length(Donnees_Histo)) {
  Base_1 <- merge(Base_1, Donnees_Histo[[i]], by = "...1", all = TRUE)
}


BDD_Histo_Final<-Base_1

BDD_Histo_Final$...1 <- as.Date(paste0(BDD_Histo_Final$...1, "-01"), format = "%Y-%m-%d")
BDD_Histo_Final <- BDD_Histo_Final[order(BDD_Histo_Final$...1, decreasing = TRUE), ]


          #***** Sauvegarde de la base de donnés*****
          #**** Format de stokage ?????******************

#write_xlsx(BDD_Histo_Final, "BDD_energies.xlsx")
#write_xlsx(BDD_Histo_Final, "C:/Users/sire.ba/Desktop/Document_SB/Données/BDD_energies.xlsx")


      
    #******** 2. Module Import : Importation des séries***************************
    #******** 2. Module Import : Importation des séries élémentaires***************


# néttoyage des variables existante de l'environnement R



annee<-2018

#*************** API de Rstudio *****************************************************************
path_i<-dirname(rstudioapi::getActiveDocumentContext()$path)

path <- "L:/04- Conjoncture/06- ConjonctureR/1.Importation/Charbon/Toutes_Series"


file_paths <- list.files(path, full.names = TRUE, pattern = ".xls", recursive = TRUE)


#****%%%%%%%***Méthode1 :Méthode générale ********************************
#*****%%%%%%%***** Importer des données dans differents sous dossiers ,classeurs ,feuilles****
#****: Cette Méthode vise à importer des fichiers dans des dossiers differents , *********
#**====== des classeurs differentes et des feuilles à spécifier ou choisir ***********************************

#*** Temps d'executiondu programmme)
#profvis({
#Mesure_temps <- function() {

path <- "L:/04- Conjoncture/06- ConjonctureR/1.Importation"


#******** Les differents  sous-dossiers ***************************************************
#*****Les sous dosssiers :Charbon , Electricité, Pétrole, Gaz, `Prix et facture et `Prix a la consommation **********
#*$$$$$*******Dans chaque sous dosiers on précise la les classeurs et feuilles à choisir puis les fusionner**********
#*********Extension xls et xlsx***********************************************************


specs <- list(
  Charbon = list(
    "Charbon_series_CVS_et_CJO_a_importer" = c("Importation"),
    "Fichier import Conjoncture Charbon" = c("Importation1","Importation2"),
    "Importation siderurgie" =c("Importation")
  ),
  Electricite = list(
    "Elec_pourPegase" = c("Importation1","SImportation2")
  ),
  Gaz = list(
    "Conjoncture Gaz 2023" = c("Importation1", "SImportation2")
  ),
  Petrole = list(
    "IMPORPETRJEDOX août 2023 hors DROM1" = c("Importation1","Importation2"),
    "IMPORPETRJEDOX août 2023 version DROM" = c("Importation"),
    "Petrole_series_CVS_et_CJO_a_importer" = c("Importation1"),
    "importation petrochimie" = c("Importation")
  ),
  `Prix a la consommation` = list(
    "import_enquete_tp_jedox_2023_10_06" = c("Importation"),
    "import_Jedox_granulés_T2_2023" = c("Importation"),
    "import_Jedox_produits_pétroliers_2023-08" = c("Importation1")
  ),
  `Prix et facture` = list(
    "SERIES_POUR_JEDOX_2023_09_EXP" = c("Importation"),
    "SERIES_POUR_JEDOX_2023_09_IMP" = c("Importation"),
    "SERIES_POUR_JEDOX_2023_09_PRIX_ET_CLIMAT" = c("Importation")
  )
)


data_frames_list <- list()

#********** Parcourir les sous-dossiers ****************************************
#******************************************************************************


for (sous_D in names(specs)) {
  
  sous_D_path <- file.path(path, sous_D)   #***** Chemin du sous dossier ***********
  
  if (dir.exists(sous_D_path)) {
    spec_sous_D <- specs[[sous_D]]
    
    #******* Parcourir les classeurs spécifiés pour ce sous-dossier****************
    
    for (classeur in names(spec_sous_D)) {
      # classeur_path <- file.path(sous_D_path, paste0(classeur, ".xls"))
      extension <- ifelse(sous_D %in% c("Prix a la consommation", "Prix et facture"), "xlsx", "xls")
      classeur_path <- file.path(sous_D_path, paste0(classeur, ".", extension))
      
      if (file.exists(classeur_path)) {
        feuilles_spec <- spec_sous_D[[classeur]]
        noms_feuilles <- excel_sheets(classeur_path)  #***** Utilisation .xlsx **********
        
        feuilles_selectionnees <- noms_feuilles[noms_feuilles %in% feuilles_spec]
        données_xls <- lapply(feuilles_selectionnees, function(feuille) {
          if (extension == "xlsx") {
            read_xlsx(classeur_path, sheet = feuille)  #*** Utilisation read_xlsx pour .xlsx *****
          } else {
            read_excel(classeur_path, sheet = feuille)
          }
        })
        
        data_frames_list <- c(data_frames_list, données_xls)
      }
    }
  }
}

#*******Fusion des données en ajoutant les colonnes *************************
#****************************************************************************

tot <- bind_rows(data_frames_list)
tot2<-tot
tot$...1  <- as.Date(paste0(tot$...1 , "-01"), format = "%Y-%m-%d")
tot<-tot %>% rename(Annees_mois=  ...1)


tot_arrage <- tot %>% arrange(desc(Annees_mois))
tot_arrage <-tot_arrage %>% select(sort(colnames(.)))


#}
#mb_result <- microbenchmark(Mesure_temps())
#print(mb_result)

#})



#*******$$$$ : 2-module Import *******************************
#******$$$$  : 2-module Import *******************************
#*********$$$$$$$$ Méthode2 :Regroupement dans un classeurs *****************


#**** Cette méthode regroupe toutes les feuilles de données des series dans un seul classeur $************
#***** puis les importer d'un seul cout****************************************************************
#*******************************************************************************************************


#Mesure_temps <- function() {  

list_importation<- import_list("L:/04- Conjoncture/06- ConjonctureR/1.Importation/Importation_toutes_series.xlsx")


#*****//////sortir les data frames de la liste pour les stocker***********//////****************
#********** dans des objets individuels, avec un nom identique au nom du fichier****//////////********


for(i in 1: length(list_importation)) { 
  assign(names(list_importation)[i], list_importation[[i]])
} 

Base_1_import <- list_importation[[1]]

for (i in 2:length(list_importation)) {
  Base_1_import <- merge(Base_1_import, list_importation[[i]], by = "...1", all = TRUE)
}

BDD_S_elm<-Base_1_import


#******* Vérification**********************************************************


colonnes_uniques <- !duplicated(BDD_S_elm)

if (any(!colonnes_uniques)) {
  print("Les colonnes suivantes ne sont pas uniques :")
  print(names(BDD_S_elm)[!colonnes_uniques])
} else {
  print("Toutes les colonnes de la base de données sont uniques.")
}
#*************************Fin de la vérification*****************************



cl_modif <- grep("\\.x$|\\.y$", names(BDD_S_elm), value = TRUE)
noms_modif <- sub("\\.x$", "", cl_modif)
noms_modif <- sub("\\.y$", "", noms_modif)
names(BDD_S_elm)[names(BDD_S_elm) %in% cl_modif] <- noms_modif


BDD_S_elm$...1 <- as.Date(paste0(BDD_S_elm$...1, "-01"), format = "%Y-%m-%d")
BDD_S_elm <- BDD_S_elm[order(BDD_S_elm$...1, decreasing = TRUE), ]

                 #**** Exportation base de données séries élementaires *******
                 #**** Exportation base de données séries élementaires *******
                 

#write_xlsx(BDD_S_elm, "BDD_energies.xlsx")
#write_xlsx(BDD_S_elm, "C:/Users/sire.ba/Desktop/Document_SB/Données/BDD_energies.xlsx")



    #***** 3 Comparaison *******************************************************************
    #****  3. Compareraison séries importées et séries enregistrées dans base historique*****

#*** j'ai importé 784 séries élementaires que je dois faire une comparaison avec les séries qui sont dans ma 
#* base de données historiques qui contient 2556 séries***************



         #************ 4-Métadonnées *****************************
         #****$$$$$$$$ 4-Métadonnées  $$$$$$**********************
         #*** !!!! Base de données des métadonnées$$$$$***********  


#***** Importer plusieurs feuilles Excel dans des data frame distincts *************


F_list<- import_list("L:/04- Conjoncture/06- ConjonctureR/2.Metadonnées/Metadonnées_Initiales_Series_et_Prix.xlsx")


#*****//////sortir les data frames de la liste pour les stocker***********//////****************
#********** dans des objets individuels, avec un nom identique au nom du fichier****//////////********

for(i in 1: length(F_list)) { 
  assign(names(F_list)[i], F_list[[i]])
} 


#***** Fusion des formules********

path <- "L:/04- Conjoncture/06- ConjonctureR/2.Metadonnées/Metadonnées_Initiales_Series_et_Prix.xlsx"

BDD_Formules <- path %>%
  excel_sheets() %>% 
  map_df(read_excel,
         path = path)

class(BDD_Formules)
Formules<-BDD_Formules
Formules<-as.data.frame(Formules)
Formules<-Formules[,-c(54,55)]
colnames(Formules)


Metadonnees<-Formules[,-c(2,3,5,7,8,9,10,11,13,17,20,22,23,24,27,29,30,34,35,36,37,38,40,42,44,45,46,47,48)]


#******$$$$$$$ *Pas besoin la répétion de la ligne suivante: **********

#****** $$$$$$$ **** "GROUPE toutes series petrole" ********************
Metadonnees_1 <- Metadonnees[c(-1, -106, -314, -469, -708, -912, -1265, -1519,-1689,-1723,-1751, -2036), ]

Metadonnees_1<-Metadonnees_1 %>% rename(Groupes_Series = '...1')


             #*******Ajout des Séries dans Dido ************
            #*****$$$$!!!!*** Ajout des series dans Dido ****

Series_Dido<-read_excel("L:/04- Conjoncture/06- ConjonctureR/docs Jedox et dido/Séries Dido.xlsx")
Series_Dido<-Series_Dido %>% rename(Groupes_Series = nom_Jedox)

#******* fusion : recuperation des colonnes dans dido et rajoutons les dans les métadonnées *******
#*$$$$$$**fusion : recuperation des colonnes dans dido et rajoutons les dans les métadonnées******

Metadonnees_final <- merge(Metadonnees_1, Series_Dido, by = "Groupes_Series", all.x = TRUE, all.y = FALSE)



        #********* 4.2Fusion métadonnée et base de donnés historique********
        #********* 4.2Fusion métadonnées et base de données historique******



#********* Utilisons  pivot_longer pour transformer BDD_Histo_Final( Series élementaires) afin de recuperer: ******
#*******les dates ,les series et leurs valeurs  en longueur (verticalement)**********


#********************** Début***********************************************************************


data2_long <- BDD_Histo_Final %>%
  pivot_longer(cols = -...1, names_to = "Groupes_Series", values_to = "Value")

data2_long <- data2_long %>% rename(Annees_mois = `...1`)

BDD_H_Meta<- merge(data2_long,Metadonnees_1, by = "Groupes_Series", all.x = TRUE) 

BDD_H_Meta <- BDD_H_Meta[order(BDD_H_Meta$Annees_mois, decreasing = TRUE), ]

                 
BDD_H_Meta2 <- BDD_H_Meta %>%
  pivot_wider(names_from = Annees_mois, values_from = Value)

#**********************  fin******************************************************************
#**********************  fin******************************************************************





 











































