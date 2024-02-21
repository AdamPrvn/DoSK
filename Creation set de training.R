#clean console, environment & plots 
rm(list = ls())
cat("\014")
# Clear all plots
try(dev.off(dev.list()["RStudioGD"]),silent=TRUE)
try(dev.off(),silent=TRUE)

setwd("C:/Primatologie/TRAVAUX/Degré de savoir social - C.Garcia, J.Duboscq & S.Ballesta/R/DoSK")
library(readxl)
library(arrangements)
library(tidyr)
library(openxlsx) # pour exporter un tableau dans R en tableau excel
data <- read_excel("D:/Stage M2 CdP/Kinship/Tableau Paires.xlsx", 
                             sheet = "Feuil2")
View(data)

#######################################################

# Sélectionner 3 paires non kin
n<-3
kinship_0 <- subset(data, kinship == 0)
sample_kinship_0 <- kinship_0[sample(nrow(kinship_0), n), ]
kinship_1 <- subset(data, kinship == 1)
individuals_kinship_0 <- sort(unique(c(sample_kinship_0$id1, sample_kinship_0$id2)))

# Sélectionner toutes les paires où kinship est égal à 1 et où les individus de kinship 0 sont présents dans id1 ou id2
matching_pairs_kinship_1 <- subset(kinship_1, id1 %in% individuals_kinship_0 & id2 %in% individuals_kinship_0)
individuals_kinship_1 <- sort(unique(c(matching_pairs_kinship_1$id1, matching_pairs_kinship_1$id2)))

# Tant que le nombre de lignes de matching_pairs_kinship_1 est inférieur à 3
while(nrow(matching_pairs_kinship_1) < n || !identical(individuals_kinship_0, individuals_kinship_1)) {
  # Sélectionner aléatoirement 3 paires où kinship est égal à 0
  sample_kinship_0 <- kinship_0[sample(nrow(kinship_0), n), ]
  individuals_kinship_0 <- sort(unique(c(sample_kinship_0$id1, sample_kinship_0$id2)))
  matching_pairs_kinship_1 <- subset(kinship_1, id1 %in% individuals_kinship_0 & id2 %in% individuals_kinship_0)
  individuals_kinship_1 <- sort(unique(c(matching_pairs_kinship_1$id1, matching_pairs_kinship_1$id2)))
  # Si le nombre de lignes de matching_pairs_kinship_1 est toujours inférieur à 3, répéter le processus
}
individuals_kinship_0 <- sort(unique(c(sample_kinship_0$id1, sample_kinship_0$id2)))
individuals_kinship_1 <- sort(unique(c(matching_pairs_kinship_1$id1, matching_pairs_kinship_1$id2)))

tot_individuals_kinship_1 <- list(individuals=c(matching_pairs_kinship_1$id1, matching_pairs_kinship_1$id2))
# Compter le nombre d'occurrences de chaque individu dans la liste tot_individuals_kinship_1
ind_counts <- table(tot_individuals_kinship_1$individuals)
# Marquer "option" dans matching_pairs_kinship_1$possibility lorsque id1 et id2 apparaissent au moins deux fois dans la liste tot_individuals_kinship_1
matching_pairs_kinship_1$possibility <- ifelse(ind_counts[matching_pairs_kinship_1$id1] >= 2 & ind_counts[matching_pairs_kinship_1$id2] >= 2, "option", "no option")
print(matching_pairs_kinship_1)

(m<-nrow(matching_pairs_kinship_1))
(k<-m-n) # nombre de lignes à retirer

no_option_kinship_1 <- subset(matching_pairs_kinship_1, possibility == "no option")
option_kinship_1 <- subset(matching_pairs_kinship_1, possibility == "option")
(l<-nrow(option_kinship_1))
sample_option_kinship_1 <- option_kinship_1[sample(nrow(option_kinship_1), l-k), ]
sample_kinship_1 <- rbind(no_option_kinship_1, sample_option_kinship_1)

individuals_sample_kinship_1 <- sort(unique(c(sample_kinship_1$id1, sample_kinship_1$id2)))

# Tant que les individus dans kin et nonkin ne sont pas identiques
while(!identical(individuals_kinship_0, individuals_sample_kinship_1)) {
  sample_option_kinship_1 <- option_kinship_1[sample(nrow(option_kinship_1), l-k), ]
  sample_kinship_1 <- rbind(no_option_kinship_1, sample_option_kinship_1)
  individuals_sample_kinship_1 <- sort(unique(c(sample_kinship_1$id1, sample_kinship_1$id2)))
  # Si les individus dans kin et nonkin sont toujours différents, répéter le processus
}

sample_kinship_0$possibility<-"no option"
training_set <- rbind(sample_kinship_0, sample_kinship_1) # set de training
View(training_set)


#######################################################

# Sélectionner 3 paires kin
n<-3
kinship_0 <- subset(data, kinship == 0)
kinship_1 <- subset(data, kinship == 1)
sample_kinship_1 <- kinship_1[sample(nrow(kinship_1), n), ] # tirage de n paires de kin
individuals_kinship_1 <- unique(c(sample_kinship_1$id1, sample_kinship_1$id2))

# Sélectionner toutes les paires où kinship est égal à 1 et où les individus de kinship 0 sont présents dans id1 ou id2
matching_pairs_kinship_0 <- subset(kinship_0, id1 %in% individuals_kinship_1 & id2 %in% individuals_kinship_1)
individuals_kinship_0 <- unique(c(matching_pairs_kinship_0$id1, matching_pairs_kinship_0$id2))

# Tant que le nombre de lignes de matching_pairs_kinship_0 est inférieur à 3 et que la liste des individus de kinship 0 et 1 ne sont pas identiques
while(nrow(matching_pairs_kinship_0) < n || !identical(individuals_kinship_1, individuals_kinship_0)) {
  sample_kinship_1 <- kinship_1[sample(nrow(kinship_1), n), ] # tirage de n paires de kin
  individuals_kinship_1 <- unique(c(sample_kinship_1$id1, sample_kinship_1$id2))
  matching_pairs_kinship_0 <- subset(kinship_0, id1 %in% individuals_kinship_1 & id2 %in% individuals_kinship_1)
  individuals_kinship_0 <- unique(c(matching_pairs_kinship_0$id1, matching_pairs_kinship_0$id2))
  # Si le nombre de lignes de matching_pairs_kinship_0 est toujours inférieur à 3, répéter le processus
}

tot_individuals_kinship_0 <- list(individuals=c(matching_pairs_kinship_0$id1, matching_pairs_kinship_0$id2))
# Compter le nombre d'occurrences de chaque individu dans la liste tot_individuals_kinship_1
ind_counts <- table(tot_individuals_kinship_0$individuals)
# Marquer "option" dans matching_pairs_kinship_1$possibility lorsque id1 et id2 apparaissent au moins deux fois dans la liste tot_individuals_kinship_1
matching_pairs_kinship_0$possibility <- ifelse(ind_counts[matching_pairs_kinship_0$id1] >= 2 & ind_counts[matching_pairs_kinship_0$id2] >= 2, "option", "no option")
print(matching_pairs_kinship_0)

(m<-nrow(matching_pairs_kinship_0)) # donc il faut retirer m-n lignes pour avoir un total de n lignes
(k<-m-n) # nombre de lignes à retirer

no_option_kinship_0 <- subset(matching_pairs_kinship_0, possibility == "no option")
option_kinship_0 <- subset(matching_pairs_kinship_0, possibility == "option")
(l<-nrow(option_kinship_0))

sample_option_kinship_0 <- option_kinship_0[sample(nrow(option_kinship_0), l-k), ]
sample_kinship_0 <- rbind(no_option_kinship_0, sample_option_kinship_0)
individuals_sample_kinship_0 <- unique(c(sample_kinship_0$id1, sample_kinship_0$id2))

# Tant que les individus dans kin et nonkin ne sont pas identiques
while(!identical(individuals_kinship_1, individuals_sample_kinship_0)) {
  sample_option_kinship_0 <- option_kinship_0[sample(nrow(option_kinship_0), l-k), ]
  sample_kinship_0 <- rbind(no_option_kinship_0, sample_option_kinship_0)
  individuals_sample_kinship_0 <- unique(c(sample_kinship_0$id1, sample_kinship_0$id2))
  # Si les individus de individuals_sample_kinship_0 sont toujours différents, répéter le processus
}

sample_kinship_1$possibility<-"no option"
training_set <- rbind(sample_kinship_1, sample_kinship_0) # set de training
View(training_set)

############################################################

write.xlsx(training_set, "D:/Stage M2 CdP/Kinship/Training sets/Training_Set_test2.xlsx") # exporte le training set qui vient d'être généré

training_set <- read_excel("D:/Stage M2 CdP/Kinship/Training sets/Training_Set_test2.xlsx")
View(training_set)

#### Travail sur les individus du training set
tot_ind_kinship_1 <- list(id=c(sample_kinship_1$id1, sample_kinship_1$id2))
tot_ind_kinship_0 <- list(id=c(sample_kinship_0$id1, sample_kinship_0$id2))
ind_counts_kinship_0 <- table(tot_ind_kinship_0$id)
ind_counts_kinship_0 <- as.data.frame(ind_counts_kinship_0)
colnames(ind_counts_kinship_0) <- c("id","score_nonkin")
ind_counts_kinship_1 <- table(tot_ind_kinship_1$id)
ind_counts_kinship_1 <- as.data.frame(ind_counts_kinship_1) 
colnames(ind_counts_kinship_1) <- c("id","score_kin")

ind_training_set<-merge(ind_counts_kinship_0,ind_counts_kinship_1,by="id") 
(nid_training_set<-length(ind_training_set$id)) # nombre d'individus présents dans le training set

ind_training_set$score_id <- ind_training_set$score_kin-ind_training_set$score_nonkin
####

(score_nid<-(length(ind_training_set$id)/n)) # nombre d'individus / n -> plus c'est petit, mieux c'est
(quality<-sum(abs(ind_training_set$score_id)))

tab_training_set <- data.frame(training_set_id="test1",score_nid,quality) 
tab_training_set

tab_tot_training_sets <- read_excel("D:/Stage M2 CdP/Kinship/tab_tot_training_sets.xlsx")
View(tab_tot_training_sets)

write.xlsx(tab_training_sets, "D:/Stage M2 CdP/Kinship/tab_training_sets.xlsx") # exporte le tableau des training sets

###############################################################################
# Là, j'essaye de trouver un moyen pour que R me sorte tous les training sets possibles

data <- read_excel("D:/Stage M2 CdP/Kinship/Tableau Paires.xlsx", 
                   sheet = "Feuil2")

### 1. Cleaning et vérifications
n=3 # on veut n paire de kin VS n paires de nk
data$possibility<-NA # création de la variable possibility qui servira plus tard
data$pair_id <- apply(data[1:2], 1, function(x) paste(x, collapse = ";")) # création de la variable pair_id
kinship_1 <- subset(data, kinship == 1)
kinship_0 <- subset(data, kinship == 0) # séparation du dataset en un set regroupant toutes les paires nk et un autre avec toutes les paires kin
kinship_0$possibility<-"no option" # dans le subset nk, on remplit la var possibility avec "no option" car c'est à partir des nk qu'on regarde les set de kin qui matchent
# pair_kinship_0<-as.vector(kinship_0$pair_id) # donne la liste de toutes les paires de nk
# pair_kinship_1<-as.vector(kinship_1$pair_id) # donne la liste de toutes les paires de kin

## on vérifie que tous les individus présents dans nk le sont aussi dans kin
individuals_kinship_0 <- sort(unique(c(kinship_0$id1, kinship_0$id2)))
individuals_kinship_1 <- sort(unique(c(kinship_1$id1, kinship_1$id2)))
identical(individuals_kinship_0, individuals_kinship_1)


### 2. Création de tous les dataframes de combinaisons de paires de nk possibles

## on fait tous les tirages de n paires de nonkin possibles
# ça nous permet ensuite de créer les training sets autour de ces tirages
n_combinaisons_kinship_0 <- combinations(kinship_0$pair_id, k=n)
names<-paste0("pair",1:n)
colnames(n_combinaisons_kinship_0) <- c(names)
View(n_combinaisons_kinship_0)

## on crée autant de dataframes que de tirages de n paires de nonkin
# Création d'une fonction pour transformer une ligne en un dataframe (type id1, id2, kinship, kindegree, pair_id)
transformer_en_dataframe <- function(row) {
  df<-data.frame(pair_id = row, stringsAsFactors = FALSE)
  df$id1<-NA
  df$id2<-NA
  df$kinship<-NA  
  df$kindegree<-NA
  df$possibility<-NA
  return(df)
}
# Application de la fonction à chaque combinaisons de paires de nk
# prend bcp de temps
nouveaux_dataframes <- lapply(1:nrow(n_combinaisons_kinship_0), function(i) transformer_en_dataframe(n_combinaisons_kinship_0[i, ]))
View(nouveaux_dataframes)

# Remplissage des tableaux de combinaisons de paires de nonkin (type id1, id2, kinship, kindegree, pair_id)
# prend un peu de temps
for(i in 1:length(nouveaux_dataframes)) {
  for(x in 1:nrow(nouveaux_dataframes[[i]])) {
    pair <- nouveaux_dataframes[[i]]$pair_id[x]
    loc <- which(nouveaux_dataframes[[i]]$pair_id[x]==kinship_0$pair_id)# donne la ligne dans kinship_0 où la pair_id est la même
    if(pair == kinship_0$pair_id[loc]) {
      kinship <- kinship_0$kinship[kinship_0$pair_id == pair]
      kindegree <- kinship_0$kindegree[kinship_0$pair_id == pair]
      possibility <- kinship_0$possibility[kinship_0$pair_id == pair]
      id1 <- kinship_0$id1[kinship_0$pair_id == pair]
      id2 <- kinship_0$id2[kinship_0$pair_id == pair]
      nouveaux_dataframes[[i]]$kinship[x] <- kinship
      nouveaux_dataframes[[i]]$kindegree[x] <- kindegree
      nouveaux_dataframes[[i]]$possibility[x] <- possibility
      nouveaux_dataframes[[i]]$id1[x] <- id1
      nouveaux_dataframes[[i]]$id2[x] <- id2
    }
  }
}

### 3. Création de tous les training sets utilisables

## 3.1 Sélection de toutes les paires de kin compatibles
# contrainte 1: les individus présents dans les paires nk doivent être présentes dans id1 et id2 des paires de kin 
# contrainte 2: tous les individus kin doivent être présent dans nonkin
for(i in 1:length(nouveaux_dataframes)) {
  individuals_kinship_0 <- sort(unique(c(nouveaux_dataframes[[i]]$id1, nouveaux_dataframes[[i]]$id2)))
  matching_pairs_kinship_1 <- subset(kinship_1, id1 %in% individuals_kinship_0 & id2 %in% individuals_kinship_0)
  individuals_kinship_1 <- sort(unique(c(matching_pairs_kinship_1$id1, matching_pairs_kinship_1$id2)))
  if(nrow(matching_pairs_kinship_1) >= n & identical(individuals_kinship_0, individuals_kinship_1)) {
    nouveaux_dataframes[[i]] <- rbind(nouveaux_dataframes[[i]], matching_pairs_kinship_1)
  } # on a relié les paires de kin correspondantes que si il y a au moins n paires de kin correspondantes
}   # et si tous les individus dans nk sont présents dans kin
# Dans 'nouveaux_dataframes' on obtient les tableaux des nk et les paires de kin combinés (si les conditions sont remplies)
# !!! attention : 'nouveaux_dataframes' contient encore les tableaux de nk où les paires de kin ne correspondaient pas aux contraintes, mais les deux tableaux n'ont pas été reliés (donc ils font n lignes exactement)
# donc on a des dataframes de n lignes et des dataframes de >= 2n lignes

## 3.2 Sélection des df utilisables pour créer les training sets
# contrainte 1 : on ne veut que les df avec au moins 2n lignes
# contrainte 2 : tous les individus dans nk doivent être présents dans kin

# pour compter et voir tous les dataframes dont le nombre de lignes est = n, donc inutilisables
count_dataframes_n <- sum(sapply(nouveaux_dataframes, function(df) nrow(df) == n))
print(count_dataframes_n)
for(i in 1:length(nouveaux_dataframes)) {
  if(nrow(nouveaux_dataframes[[i]]) == n) {
    print(i)
  }
} 
# pour compter et voir tous les dataframes dont le nombre de lignes est >= 2n, donc utilisables
count_dataframes_2n <- sum(sapply(nouveaux_dataframes, function(df) nrow(df) >= 2*n))
print(count_dataframes_2n)
for(i in 1:length(nouveaux_dataframes)) {
  if(nrow(nouveaux_dataframes[[i]]) >= 2*n) {
    print(i)
  }
} 
# Extraction de tous les dataframes utilisables, donc avec un nombre de lignes >= 2*n
dataframes_possibles_pile <- Filter(function(df) nrow(df) == 2*n, nouveaux_dataframes)
View(dataframes_possibles_pile) # = 2n
dataframes_possibles_plus <- Filter(function(df) nrow(df) > 2*n, nouveaux_dataframes)
View(dataframes_possibles_plus) # > 2n

# Pour tous les dataframes = 2n, les paires de kin ne sont pas optionnelles
for(i in 1:length(dataframes_possibles_pile)) {
  dataframes_possibles_pile[[i]]$possibility<-"no option"
}
# pour ces df, on vérifie que les ind kin et nk sont identiques
# on demande la liste des dataframes qui seront retirés car les individus de k0 et k1 ne sont pas identiques
indices_a_retirer <- c()
for(i in 1:length(dataframes_possibles_pile)) {
  k0 <- subset(dataframes_possibles_pile[[i]], kinship == "0")
  k1 <- subset(dataframes_possibles_pile[[i]], kinship == "1")
  ind_k0 <- sort(unique(c(k0$id1, k0$id2)))
  ind_k1 <- sort(unique(c(k1$id1, k1$id2)))   
  if (!identical(ind_k0, ind_k1)) {
    indices_a_retirer <- c(indices_a_retirer, i)
  }
}
# Affiche les indices des dataframes à retirer
print(indices_a_retirer) # si la réponse est "NULL" alors tous les df sont ok


## 3.3 Création des trainings sets pour les df > 2n
# Pour les df > 2n, on veut toutes les possibilités de training sets
# donc pour chaque df, on veut avoir un training set pour les différentes combinaisons de kin possibles

# On complète la colonne "possibility" pour les kin pour savoir quelle paire doit toujours être présente dans les training set
for(i in 1:length(dataframes_possibles_plus)) {
  ind <- c()
  df_kinship_0 <- subset(dataframes_possibles_plus[[i]], kinship == 0)
  df_kinship_1 <- subset(dataframes_possibles_plus[[i]], kinship == 1) # on va travailler que sur les kin
  for(x in 1:nrow(df_kinship_1)) {
    ind <- c(ind, df_kinship_1$id1[x],df_kinship_1$id2[x])
  }
  ind_list <- list(individuals=ind)
  ind_counts <- table(ind_list$individuals)
  df_ind_counts <- as.data.frame(ind_counts)
  colnames(df_ind_counts) <- c("ID", "Freq")
  for(y in 1:nrow(df_kinship_1)){
    loc1 <- which(df_kinship_1$id1[y]==df_ind_counts$ID)# donne la ligne dans df_kinship_1 où id1 correspond à ID
    loc2 <- which(df_kinship_1$id2[y]==df_ind_counts$ID)# donne la ligne dans df_kinship_1 où id2 correspond à ID
    df_kinship_1$possibility[y] <- ifelse(df_ind_counts$Freq[loc1] > 1 & df_ind_counts$Freq[loc2] > 1, "option", "no option")
  } # possibility = no option, si id1 et id2 ne sont présents qu'une fois dans la liste des kin
  dataframes_possibles_plus[[i]] <- rbind(df_kinship_0, df_kinship_1) # on relie pour que la colonne "possibility" soit complète
}

# tous les dataframes ne contenant aucune option sont inutilisables (car > 2n)
# pour obtenir tous les dataframes ne contenant aucune option
indices <- c()
for (i in seq_along(dataframes_possibles_plus)) {
  count_option <- sum(dataframes_possibles_plus[[i]]$possibility == "option")
  if (count_option == 0) {
    indices <- c(indices, i)
  }
}
list_df_0options <- list(indices) # liste des df inutilisables
dataframes_0options <- dataframes_possibles_plus[indices] # donne l'ensemble des df inutilisables 

# pour obtenir tous les dataframes contenant au moins 1 option (donc utilisables)
indices <- c()
for (i in seq_along(dataframes_possibles_plus)) {
  count_option <- sum(dataframes_possibles_plus[[i]]$possibility == "option")
  if (count_option >= 1) {
    indices <- c(indices, i)
  }
}
list_df_1options <- list(indices) # liste des df utilisables
dataframes_1options <- dataframes_possibles_plus[indices] # c'est cette liste qu'on va conserver

# 
linked_all <- list()
for(y in 1:length(dataframes_1options)) {
  dataframes_1options_o <- subset(dataframes_1options[[y]], possibility == "option")
  dataframes_1options_no <- subset(dataframes_1options[[y]], possibility == "no option") 
  # on va travailler que sur les "option", pcq on garde tous les "no option"
  (m<-nrow(dataframes_1options[[y]])) # nombre de lignes du df
  (k<-m-(2*n)) # nombre de lignes à retirer pour avoir 2n lignes
  (l<-nrow(dataframes_1options_o)) # nombre de paires optionnelles
  (q<-l-k) # nombre de paires optionnelles à conserver
  # on fait toutes les combinaisons de q paires de kin optionnelles possibles
  combinaisons_options <- combinations(dataframes_1options_o$pair_id, k=q)
  #names<-paste0("pair",1:q)
  #colnames(n_combinaisons_kinship_0) <- c(names)
  # transformation en tableau
  new_df_o <- lapply(1:nrow(combinaisons_options), function(i) transformer_en_dataframe(combinaisons_options[i, ]))
  # on remplit tous les tableaux (type id1, id2, kinship, kindegree, pair_id)
  for(i in 1:length(new_df_o)) {
    for(x in 1:nrow(new_df_o[[i]])) {
      pair <- new_df_o[[i]]$pair_id[x]
      loc <- which(new_df_o[[i]]$pair_id[x]==dataframes_1options_o$pair_id)# donne la ligne dans dataframes_1options_o où la pair_id est la même
      if(pair == dataframes_1options_o$pair_id[loc]) {
        kinship <- dataframes_1options_o$kinship[dataframes_1options_o$pair_id == pair]
        kindegree <- dataframes_1options_o$kindegree[dataframes_1options_o$pair_id == pair]
        possibility <- dataframes_1options_o$possibility[dataframes_1options_o$pair_id == pair]
        id1 <- dataframes_1options_o$id1[dataframes_1options_o$pair_id == pair]
        id2 <- dataframes_1options_o$id2[dataframes_1options_o$pair_id == pair]
        new_df_o[[i]]$kinship[x] <- kinship
        new_df_o[[i]]$kindegree[x] <- kindegree
        new_df_o[[i]]$possibility[x] <- possibility
        new_df_o[[i]]$id1[x] <- id1
        new_df_o[[i]]$id2[x] <- id2
        }
      }
    }
  # on lie le tableau "new_df_o[[i]]" avec le tableau qui ne contenait plus que les "no option"
  linked_dataframes <- list()
  for (df in new_df_o) {
    linked_df <- rbind(dataframes_1options_no, df)
    linked_dataframes <- c(linked_dataframes, list(linked_df))
    }
  linked_all <- c(linked_all, list(linked_dataframes))
} # on obtient une liste "linked_all" regroupant les listes "linked_dataframes" des dataframes
# !!! attention : toutes les combinaisons des kin optionnels ayant été fait, dans certains cas, certains individus nk ne sont plus présents dans les kin.
# donc il faut re-sélectionner uniquement les df où les nk sont présents dans les kin

# Liste les dataframes à retirér car les individus de k0 et k1 ne sont pas identiques
indices_a_retirer <- list()
for(i in 1:length(linked_all)) {
  for(df in linked_all[[i]]){
    k0 <- subset(df, kinship == "0")
    k1 <- subset(df, kinship == "1")
    ind_k0 <- sort(unique(c(k0$id1, k0$id2)))
    ind_k1 <- sort(unique(c(k1$id1, k1$id2)))   
    if (!identical(ind_k0, ind_k1)) {
      indices_a_retirer[[length(indices_a_retirer) + 1]] <- i
      break  # Sortir de la boucle interne dès qu'un dataframe ne respecte pas la condition
    }
  }
}
print(indices_a_retirer)
# on retire les df concernés

# indices_a_retirer <- unlist(indices_a_retirer)
# linked_all_filtre <- linked_all[-indices_a_retirer] # là ça retire déjà les dataframes à retirer
# View(linked_all_filtre)

## OU 
linked_all <- lapply(linked_all, function(df_list) {
  Filter(function(df) {
    k0 <- subset(df, kinship == "0")
    k1 <- subset(df, kinship == "1")
    ind_k0 <- sort(unique(c(k0$id1, k0$id2)))
    ind_k1 <- sort(unique(c(k1$id1, k1$id2)))
    identical(ind_k0, ind_k1)
  }, df_list)
})
linked_all <- linked_all[sapply(linked_all, length) > 0] # Retirer les listes vides (si nécessaire)


## 3.4 Regroupement de tous les training set utilisables

# on veut maintenant regrouper dataframes_possibles_pile avec linked_all
# donc on doit d'abord dé-lister linked_all
tous_dataframes <- list()
for (i in 1:length(linked_all)) {
  tous_dataframes <- c(tous_dataframes, linked_all[[i]])
}
View(tous_dataframes)

# on regroupe tous les training sets utilisables
all_training_sets <- c(dataframes_possibles_pile,tous_dataframes) # ce sont tous les training sets utilisables

# on exporte tous les training sets
chemin <- "D:/Stage M2 CdP/Kinship/Training sets/"
for (i in 1:length(all_training_sets)) {
  nom_fichier <- paste0("training_set_", i, ".xlsx")
  chemin_fichier <- file.path(chemin, nom_fichier)
  write.xlsx(all_training_sets[[i]], file = chemin_fichier)
}














