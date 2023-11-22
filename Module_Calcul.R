

    
     #********* *** MODULE CALCUL DE SERIES *********************************
     #***********************************************************************



                    #****3- Calcul de séries  ********************
                     #****3- Calcul à l'aide des formules **********


   #$@**** OBJECTIF : Calculer les séries à travers les diferentes formules et les diferentes colonnes des métadonnées
    # des métadonnes et en les attribuant leurs noms correspondantes

    #************* Conserver la base  et renommons les noms des colonnes *******
    #**********remplissage des valeurs de la série calculée***********************************
    #**** attribuer les valeurs de la colonne(série calculée) au nom correspondante de la formule(série calculée) **
    #***************************************************************************************************************


tot<-BDD_S_elm    #*** calculer dans le module import***

colnames(tot) <- gsub("-", "_", colnames(tot))


BDD_SF <- Metadonnees_final[, c(1,8)]    # Total OBSERVATIONS = 2147
lgn_select<- !grepl("AV",BDD_SF$`Formule calcul`) 
List_supp<- BDD_SF[lgn_select, ]


List_General<-List_supp$`Formule calcul`
L_nom_General<-List_supp$Groupes_Series


#******** Cas des caracteres*************************************************
#******* clarifier les caracteres  sur la liste des noms et listes formules*****


List_F_General<- lapply(List_General, function(x) gsub("(-)(?=[A-Z])", "_", perl = TRUE,x))
List_F_General<- lapply(List_F_General, function(x) gsub(",", ".", x))
L_noms_FGeneral<- lapply(L_nom_General, function(x) gsub("-", "_", x))


#**********$$$$ Maintenant identifions les formulescas  par cas ********$$$$*
#**********$$$$ pour chaque cas : creeons une liste*********************$$$$*


#liste_AV<- list()
liste_NA<-list()
liste_Lag <- list()
liste_Cumul<-list()
liste_Autres<- list()
liste_cond <- list()


#****/////// Création des listes par cas****//////////**************** 
#***///////Boucle à travers la liste générale et******//////////*******
#***////// filtrez les éléments dans les listes appropriées***////******


for (i in 1:length(List_F_General)) {
  if (is.na(List_F_General[[i]])) {
    liste_NA <- c(liste_NA, List_F_General[[i]])
  } else if (grepl("Lag", List_F_General[[i]])) {
    liste_Lag <- c(liste_Lag, List_F_General[[i]])
  } else if (grepl("Cumul",List_F_General[[i]])) {
    liste_Cumul<-c(liste_Cumul,List_F_General[[i]])
  } else if (grepl("si",List_F_General[[i]])) {
    liste_cond<-c(liste_cond,List_F_General[[i]])
  } else {
    liste_Autres<- c(liste_Autres, List_F_General[[i]])
  }
}


#***** $$$$$ Recupération des noms dans la liste_Autres ***$$$$$ *************
#***$$$ Ce qui nous permettra par la suite d'attribuér la formule au nom correspondant ***
#****$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$**


BDD_noms_Autres<-BDD_SF
BDD_noms_Autres<-na.omit(BDD_noms_Autres)

BDD_noms_Autres2 <- BDD_noms_Autres %>%
  filter(!grepl("Cumul|AV|Lag|si", `Formule calcul`))


BDD_noms_Autres2$`Formule calcul`
BDD_noms_Autres2$Groupes_Series
lis_Series_D_Autres<-BDD_noms_Autres2$Groupes_Series
lis_Series_D_Autres<- lapply(lis_Series_D_Autres, function(x) gsub("-", "_", x))


BDD_noms_Condit_Si <- BDD_noms_Autres %>%
  filter(grepl("si", `Formule calcul`))


#liste_cond<-BDD_noms_Condit_Si$`Formule calcul`
for (i in 1:length(liste_cond)) {
  liste_cond[i] <- gsub("-", "_", liste_cond[i])
}


liste_noms_Condit_Si<-BDD_noms_Condit_Si$Groupes_Series
for (i in 1:length(liste_noms_Condit_Si)) {
  liste_noms_Condit_Si[i] <- gsub("-", "_", liste_noms_Condit_Si[i])
}


#***** transformation des caracteres pour bien attriber les noms ************
#******* verification la conformité de l'ecriture des noms et des formules **********


replace_in_string <- function(string) {
  return(gsub("_EPT", "-EPT", string))
}
liste_Autres<- lapply(liste_Autres, function(x) gsub("-", "_", x))
liste_Autres <- lapply(liste_Autres, replace_in_string)



# Fonction pour effectuer le remplacement

R_underscore <- function(formule) {
  if (!grepl("- ", formule)) {
    # Remplacez "_ " par "- "
    formule <- gsub("_ ", "- ", formule)
  }
  return(formule)
}

# Appliquez la fonction de remplacement à toutes les formules
liste_Autres <- lapply(liste_Autres, R_underscore)


R_efface <- function(formule) {
  formule <- gsub("\\r", "", formule)
  return(formule)
}
liste_Autres <- lapply(liste_Autres, R_efface )


#************$$$$$$$$$$$*******$$$$$$$$$$$****$$$$$$$$$*********$$$$$$$******$$$$$******
#**********remplissage des valeurs de la série calculée***********************************
#**** attribuer les valeurs de la colonne(série calculée) au nom correspondante de la formule(série calculée) **
#***************************************************************************************************************


liste_resultats <- list()
liste_formules_non_calculees <- c() 

for (i in 1:length(List_F_General)) {
  formule <- List_F_General[[i]]
  nom_serie <- L_noms_FGeneral[[i]]
  
  if (formule %in% liste_NA) {  
    resultat <- NULL # Ne rien faire
    
    
  } else if (formule %in% liste_Autres) {
    if (i <= length(lis_Series_D_Autres)) { # Vérification de la validité de l'indice i
      nom_serie <- lis_Series_D_Autres[[i]] # Obtenir le nom de la série correspondante
      resultat <- numeric(nrow(tot))
      
      for (k in 1:nrow(tot)) {
        resultat_calcul <- 0
        
        tryCatch({
          resultat_calcul <- eval(parse(text = formule), envir = tot[k, ])
        }, error = function(e) {
          resultat_calcul <- NA
        })
        
        col_name <- nom_serie 
        tot[[col_name]][k] <- resultat_calcul
        
        resultat[k] <- resultat_calcul
      }
      
      liste_resultats[[nom_serie]] <- resultat
    }
    
    
  } else if (formule %in% liste_Lag) {
    variable <- sub("Lag_(\\d+)_mois\\((.*)\\)", "\\2", formule)
    lag_months <- as.integer(sub("Lag_(\\d+)_mois\\((.*)\\)", "\\1", formule))
    
    if (!is.null(tot[[variable]])) {
      col_name <- if (lag_months == 1) {
        paste0("Lag_1","_", variable)
      } else {
        paste0("Lag_12", "_", variable)
      }
      tot[[col_name]] <- ifelse(1:nrow(tot) <= lag_months, NA, lag(tot[[variable]], n = lag_months))
      resultat <-tot[[col_name]]
    } else {
      resultat <- NULL
    }
    
    
  } else if (formule %in% liste_Cumul) { 
    variable <- sub("Cumul_12_mois\\((.*)\\)", "\\1", formule)  
    cumul <- rollapply(tot[[variable]], width = 12, FUN = sum, align = "left", fill = NA)
    
    if (length(cumul) < nrow(tot)) {
      cumul <- c(rep(NA, nrow(tot) - length(cumul)), cumul)
    }
    
    col_name <- gsub("_BM$", "_BN", variable) 
    tot[[col_name]] <- cumul
    resultat <- cumul 
  } else if (formule %in% liste_cond) {
    if (i <= length(liste_cond)) {
      formule_cond <- liste_cond[[i]]
      
    
      
      resultat <- ifelse(
        !is.na(tot[[liste_noms_Condit_Si[i]]]),  
        eval(parse(text = formule_cond), envir = tot),  
        tot[[liste_noms_Condit_Si[i]]]  
      )
      
      # Stockez le résultat dans la liste des résultats
      liste_resultats[[liste_noms_Condit_Si[i]]] <- resultat
    }
  } else {
    cat("Indice i hors limites : ", i, "\n")
  }
}

if (length(liste_formules_non_calculees) > 0) {
  cat("Les formules suivantes n'ont pas généré de colonnes :\n")
  cat(liste_formules_non_calculees, sep = "\n")
} else {
  cat("Toutes les formules ont généré des colonnes.\n")
}
resultats_df <- as.data.frame(do.call(cbind, liste_resultats))
tot <- cbind(tot, resultats_df)
  


#write_xlsx(tot, "BDD_energies.xlsx")
#write_xlsx(tot, "C:/Users/sire.ba/Desktop/Document_SB/Données/BDD_energies.xlsx")

#**** Travail éffectué sur R:

#***Les remarques et suggestion sur le code**
#** Est ce que je creer des fonctions ou je peux continuer sur la méthode que j'ai éfféctuer ???*** 


          


