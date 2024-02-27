#clean console, environment & plots
rm(list = ls())
cat("\014")
# Clear all plots
try(dev.off(dev.list()["RStudioGD"]),silent=TRUE)
try(dev.off(),silent=TRUE)

install.packages("readxl")
install.packages("arrangements")
install.packages("tidyr")
install.packages("openxlsx")

setwd("D:/Stage M2 CdP/Kinship")
library(readxl)
library(arrangements)
library(tidyr)
library(openxlsx) # pour exporter un tableau dans R en tableau excel

# Fannie
data <- read_excel("C:/Users/Fannie Beurrier/Documents/Stage M2 CdP/Kinship/Tableau Paires.xlsx", 
                             sheet = "Feuil2")
# Adam
data <- read_excel("C:/Primatologie/TRAVAUX/Degré de savoir social - C.Garcia, J.Duboscq & S.Ballesta/R/DoSK", 
                   sheet = "Feuil2")
View(data)

#######################################################
# Ce code est pour l'instant divisé en 4 parties :
# 1ere partie : Création d'un seul training set à partir d'une sélection de 3 paires de non kin
# 2eme partie : Création d'un seul training set à partir d'une sélection de 3 paires de kin
# 3eme partie : Tentative d'évaluation de la qualité du training set, mais loin d'être aboutit
# 4eme partie : Création de tous les training sets possibles et utilisables

#######################################################
##### Partie 1 : Création d'un seul training set à partir d'une sélection de 3 paires de non kin

## Attention, selon les tirages fait, le code peut parfois ne pas aboutir donc il faut le relancer pour tomber sur un bon tirage (comme au tarot ^^)

n<-3 # car on veut n paires de kin VS n paires de nk
kinship_0 <- subset(data, kinship == 0)
kinship_1 <- subset(data, kinship == 1)

## Etape 1 : Tirer 3 paires non kin au hasard
sample_kinship_0 <- kinship_0[sample(nrow(kinship_0), n), ] # tirage de n paires de nk au hasard
individuals_kinship_0 <- sort(unique(c(sample_kinship_0$id1, sample_kinship_0$id2))) # liste unique des individus nk tirés au hasard
# Sélectionner toutes les paires de kin (matching pairs) (avec kinship = 1) et où les individus de kinship 0 sont présents dans id1 ou id2
matching_pairs_kinship_1 <- subset(kinship_1, id1 %in% individuals_kinship_0 & id2 %in% individuals_kinship_0)
individuals_kinship_1 <- sort(unique(c(matching_pairs_kinship_1$id1, matching_pairs_kinship_1$id2))) # liste unique des individus des matching pairs de kin

# Tant que le nombre de lignes de matching_pairs_kinship_1 est inférieur à n, on retire les paires de nk (on refait l'étape 1)
while(nrow(matching_pairs_kinship_1) < n || !identical(individuals_kinship_0, individuals_kinship_1)) {
  # Tirer aléatoirement n paires de nk (avec kinship = 0)
  sample_kinship_0 <- kinship_0[sample(nrow(kinship_0), n), ]
  individuals_kinship_0 <- sort(unique(c(sample_kinship_0$id1, sample_kinship_0$id2)))
  matching_pairs_kinship_1 <- subset(kinship_1, id1 %in% individuals_kinship_0 & id2 %in% individuals_kinship_0) # Sélectionner toutes les paires de kin (matching pairs) 
  individuals_kinship_1 <- sort(unique(c(matching_pairs_kinship_1$id1, matching_pairs_kinship_1$id2))) # liste unique des individus des matching pairs de kin
  # Si le nombre de lignes de matching_pairs_kinship_1 est toujours inférieur à 3, répéter le processus
}
individuals_kinship_0 <- sort(unique(c(sample_kinship_0$id1, sample_kinship_0$id2))) # liste unique des individus des matching pairs de kin
individuals_kinship_1 <- sort(unique(c(matching_pairs_kinship_1$id1, matching_pairs_kinship_1$id2))) # liste unique des individus des matching pairs de kin

# quand le nombre d'individus de kin (des matching pairs) est < 2n (si c'est = 2n, on doit garder toutes les paires)
tot_individuals_kinship_1 <- list(individuals=c(matching_pairs_kinship_1$id1, matching_pairs_kinship_1$id2)) # liste unique des individus des matching pairs de kin (sous forme de liste vraiment)
# Compter le nombre d'occurrences de chaque individu dans la liste tot_individuals_kinship_1
ind_counts <- table(tot_individuals_kinship_1$individuals)
# Marquer "option" dans matching_pairs_kinship_1$possibility lorsque id1 et id2 apparaissent au moins deux fois dans la liste tot_individuals_kinship_1
matching_pairs_kinship_1$possibility <- ifelse(ind_counts[matching_pairs_kinship_1$id1] >= 2 & ind_counts[matching_pairs_kinship_1$id2] >= 2, "option", "no option")
print(matching_pairs_kinship_1)

(m<-nrow(matching_pairs_kinship_1))
(k<-m-n) # nombre de lignes à retirer

no_option_kinship_1 <- subset(matching_pairs_kinship_1, possibility == "no option")
option_kinship_1 <- subset(matching_pairs_kinship_1, possibility == "option")
(l<-nrow(option_kinship_1)) # nombre de matching pairs optionnelles

## Etape 2 : Choisir les matching pairs de kin
sample_option_kinship_1 <- option_kinship_1[sample(nrow(option_kinship_1), l-k), ] # parmis les paires optionnelles, on tire l-k paires au hasard
# l-k correspond au nombre de paires optionnelles à conserver pour avoir un total de n paires de matching pairs (donc c'est le nombre de matching pairs optionnelles moins le nombre de paires à retirer)
sample_kinship_1 <- rbind(no_option_kinship_1, sample_option_kinship_1) # liaison des (l-k) matching pairs optionnelles tirées avec les matching pairs obligatoires (le nombre de lignes doit être égal à n)
# on obtient donc l'ensemble des matching pairs qu'on conserve

individuals_sample_kinship_1 <- sort(unique(c(sample_kinship_1$id1, sample_kinship_1$id2))) # liste unique des individus des matching pairs retenues

# Tant que les individus dans kin et nonkin ne sont pas identiques, on refait l'étape 2
while(!identical(individuals_kinship_0, individuals_sample_kinship_1)) {
  sample_option_kinship_1 <- option_kinship_1[sample(nrow(option_kinship_1), l-k), ]
  sample_kinship_1 <- rbind(no_option_kinship_1, sample_option_kinship_1)
  individuals_sample_kinship_1 <- sort(unique(c(sample_kinship_1$id1, sample_kinship_1$id2)))
  # Si les individus dans kin et nonkin sont toujours différents, répéter le processus
}

sample_kinship_0$possibility<-"no option"
training_set <- rbind(sample_kinship_0, sample_kinship_1) # on combine les échantillons des nk et des matching pairs de kin retenus
# on obtient un set de training
View(training_set)

write.xlsx(training_set, "D:/Stage M2 CdP/Kinship/Training sets/Training_Set_test2.xlsx") # exporte le training set qui vient d'être généré


#######################################################
##### Partie 2 : Création d'un seul training set à partir d'une sélection de 3 paires de kin

## C'est le même code que précédemment mais en inversant les kin et nk

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

write.xlsx(training_set, "D:/Stage M2 CdP/Kinship/Training sets/Training_Set_test2.xlsx") # exporte le training set qui vient d'être généré

############################################################
##### Partie 3 : Tentative d'évaluation de la qualité du training set, mais loin d'être aboutit

### Travail sur les individus du training set généré
## NON ABOUTI

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
##### Partie 4 : Création de tous les training sets possibles et utilisables

# Fannie
data <- read_excel("C:/Users/Fannie Beurrier/Documents/Stage M2 CdP/Kinship/Tableau Paires.xlsx", 
                   sheet = "Feuil2")
View(data)

### 1. Cleaning et vérifications
n=3 # on veut n paires de kin VS n paires de non kin (abrégé nk)
data$possibility<-NA # création de la variable possibility qui servira plus tard à déterminer les paires de kin qui seront nécessairement à conserver dans le training set
data$pair_id <- apply(data[1:2], 1, function(x) paste(x, collapse = ";")) # création de la variable pair_id pour attribuer une identité à la paire
kinship_1 <- subset(data, kinship == 1)
kinship_0 <- subset(data, kinship == 0) # séparation du dataset en un set regroupant toutes les paires nk et un autre avec toutes les paires kin
kinship_0$possibility<-"no option" # dans le subset nk, on remplit la var possibility avec "no option" car c'est à partir des paires de nk qu'on regarde les paires de kin qui matchent
# pair_kinship_0<-as.vector(kinship_0$pair_id) # donne la liste de toutes les paires de nk
# pair_kinship_1<-as.vector(kinship_1$pair_id) # donne la liste de toutes les paires de kin

## on vérifie que tous les individus présents dans nk le sont aussi dans kin
individuals_kinship_0 <- sort(unique(c(kinship_0$id1, kinship_0$id2)))
individuals_kinship_1 <- sort(unique(c(kinship_1$id1, kinship_1$id2)))
identical(individuals_kinship_0, individuals_kinship_1)


### 2. Création de tous les dataframes de combinaisons de paires de nk possibles

## on fait toutes les combinaisons de n paires de nonkin possibles
# ça nous permettra ensuite de créer les training sets autour de ces combinaisons de nk
n_combinaisons_kinship_0 <- combinations(kinship_0$pair_id, k=n) 
names<-paste0("pair",1:n)
colnames(n_combinaisons_kinship_0) <- c(names)
View(n_combinaisons_kinship_0)

## on crée autant de dataframes que de combinaisons de n paires de nonkin
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

# Remplissage des tableaux de combinaisons de paires de nonkin (type id1, id2, kinship, kindegree, pair_id) à partir des données initiales
# prend un peu de temps
for(i in 1:length(nouveaux_dataframes)) {
  for(x in 1:nrow(nouveaux_dataframes[[i]])) {
    pair <- nouveaux_dataframes[[i]]$pair_id[x]
    loc <- which(nouveaux_dataframes[[i]]$pair_id[x]==kinship_0$pair_id)# donne la ligne dans kinship_0 où la pair_id est la même que la ligne x du nouveaux_dataframes i
    if(pair == kinship_0$pair_id[loc]) {
      kinship <- kinship_0$kinship[kinship_0$pair_id == pair] # pour chaque variable, attribution de la valeur de la variable à un objet (ici appelé "kinship")
      kindegree <- kinship_0$kindegree[kinship_0$pair_id == pair]
      possibility <- kinship_0$possibility[kinship_0$pair_id == pair]
      id1 <- kinship_0$id1[kinship_0$pair_id == pair]
      id2 <- kinship_0$id2[kinship_0$pair_id == pair]
      nouveaux_dataframes[[i]]$kinship[x] <- kinship # pour chaque variable, restitution des valeurs des objets définis dans le nouveaux_dataframes i à la ligne x
      nouveaux_dataframes[[i]]$kindegree[x] <- kindegree
      nouveaux_dataframes[[i]]$possibility[x] <- possibility
      nouveaux_dataframes[[i]]$id1[x] <- id1
      nouveaux_dataframes[[i]]$id2[x] <- id2
    }
  }
}

### 3. Création de tous les training sets utilisables

## 3.1 Sélection de toutes les paires de kin compatibles
# contrainte 1 : à l'échelle de la paire de kin, les individus (id1 ET id2) des paires de kin doivent chacun être présents dans les individus nk du dataframe
# contrainte 2 : à l'échelle de l'ensemble des combinaisons de paires de kin, tous les individus kin doivent être présent dans nonkin, autrement dit, la liste des nk doit être identique à celle des kin
# prend beaucoup de temps
for(i in 1:length(nouveaux_dataframes)) {
  individuals_kinship_0 <- sort(unique(c(nouveaux_dataframes[[i]]$id1, nouveaux_dataframes[[i]]$id2))) # pour le nouveaux_dataframes i, c'est la liste des individus nk présents
  matching_pairs_kinship_1 <- subset(kinship_1, id1 %in% individuals_kinship_0 & id2 %in% individuals_kinship_0) # donne toutes les combinaisons des paires de kin où les deux individus (id1 ET id2) sont présents dans la liste de nk définie au-dessus
  individuals_kinship_1 <- sort(unique(c(matching_pairs_kinship_1$id1, matching_pairs_kinship_1$id2))) # pour les paires de kin matchant, c'est la liste des individus kin présents
  if(nrow(matching_pairs_kinship_1) >= n & identical(individuals_kinship_0, individuals_kinship_1)) { # si la liste des kin et des nk est identique ET s'il y a au moins n paires de kin qui matchent, on combine le dataframe des paires de nk avec celui des paires de kin qui matchent
    nouveaux_dataframes[[i]] <- rbind(nouveaux_dataframes[[i]], matching_pairs_kinship_1)
  } 
} 
# Dans 'nouveaux_dataframes' on obtient les tableaux des nk et les paires de kin combinés (pour les dataframes qui remplissent les conditions)
# !!! attention : 'nouveaux_dataframes' contient encore les tableaux de nk où les paires de kin ne correspondaient pas aux contraintes, mais les deux tableaux n'ont pas été combinés (donc ils font n lignes exactement)
# donc on a des dataframes de n lignes (inutilisables) et des dataframes de >= 2n lignes (potentiellement utilisables)


## 3.2 Sélection des df utilisables pour créer les training sets
# contrainte 1 : on ne veut que les df avec au moins 2n lignes
# contrainte 2 : tous les individus dans nk doivent être présents dans kin (normalement c'est déjà vérifié juste au-dessus)

# pour compter et voir tous les dataframes dont le nombre de lignes est = n, donc inutilisables
# OSEF, c'est juste indicatif
count_dataframes_n <- sum(sapply(nouveaux_dataframes, function(df) nrow(df) == n))
print(count_dataframes_n)
for(i in 1:length(nouveaux_dataframes)) { # donne la liste de tous les df avec un nombre de lignes == n
  if(nrow(nouveaux_dataframes[[i]]) == n) {
    print(i)
  }
} 
# pour compter et voir tous les dataframes dont le nombre de lignes est >= 2n, donc utilisables
# OSEF, c'est juste indicatif
count_dataframes_2n <- sum(sapply(nouveaux_dataframes, function(df) nrow(df) >= 2*n))
print(count_dataframes_2n)
for(i in 1:length(nouveaux_dataframes)) { # donne la liste de tous les df avec un nombre de lignes >= 2n
  if(nrow(nouveaux_dataframes[[i]]) >= 2*n) {
    print(i)
  }
} 
# Extraction de tous les dataframes utilisables, donc avec un nombre de lignes >= 2*n
dataframes_possibles_pile <- Filter(function(df) nrow(df) == 2*n, nouveaux_dataframes)
View(dataframes_possibles_pile) # = 2n
dataframes_possibles_plus <- Filter(function(df) nrow(df) > 2*n, nouveaux_dataframes)
View(dataframes_possibles_plus) # > 2n

# Pour tous les dataframes = 2n, les paires de kin ne sont pas optionnelles car on veut n paires de kin VS n paires de nk
# Donc on remplit la colonne "possibility" avec "no option"
for(i in 1:length(dataframes_possibles_pile)) {
  dataframes_possibles_pile[[i]]$possibility<-"no option"
}

# Par précaution (pas obligatoire car on l'a déjà fait avant), pour les df 'pile', on vérifie que les ind kin et nk sont bien identiques
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
print(indices_a_retirer) # Affiche les indices des dataframes à retirer : si la réponse est "NULL" alors tous les df sont ok


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
  ind_counts <- table(ind_list$individuals) # on compte le nombre de fois que chaque individu est présent en tant que kin
  df_ind_counts <- as.data.frame(ind_counts)
  colnames(df_ind_counts) <- c("ID", "Freq")
  for(y in 1:nrow(df_kinship_1)){
    loc1 <- which(df_kinship_1$id1[y]==df_ind_counts$ID)# donne la ligne dans df_kinship_1 où id1 correspond à ID dans df_ind_counts
    loc2 <- which(df_kinship_1$id2[y]==df_ind_counts$ID)# donne la ligne dans df_kinship_1 où id2 correspond à ID dans df_ind_counts
    df_kinship_1$possibility[y] <- ifelse(df_ind_counts$Freq[loc1] > 1 & df_ind_counts$Freq[loc2] > 1, "option", "no option")
  } # possibility = "no option", si id1 ou id2 ne sont présents qu'une fois dans la liste des kin
  dataframes_possibles_plus[[i]] <- rbind(df_kinship_0, df_kinship_1) # on relie pour que la colonne "possibility" soit complète
}

# tous les dataframes contenant plus de 2n "no option" sont inutilisables (car > 2n)

# pour obtenir tous les dataframes contenant pile 2n "no option" (donc utilisables)
indices <- c()
for (i in seq_along(dataframes_possibles_plus)) {
  count_no <- sum(dataframes_possibles_plus[[i]]$possibility == "no option")
  if (count_no == 2*n) {
    indices <- c(indices, i)
  }
}
list_df_2n_no <- list(indices) # liste des df >2n contenant 2n "no option"
df_2n_no_pile <- dataframes_possibles_plus[indices] # convertis en dataframes
# il faut supprimer les lignes "option" pour chacun de ces dataframes
for(i in seq_along(df_2n_no_pile)) {
  df_2n_no_pile[[i]] <- subset(df_2n_no_pile[[i]], possibility != "option")
}
View(df_2n_no_pile)

# pour obtenir tous les dataframes contenant moins de 2n "no option" (donc utilisables)
indices <- c()
for (i in seq_along(dataframes_possibles_plus)) {
  count_no <- sum(dataframes_possibles_plus[[i]]$possibility == "no option")
  if (count_no < 2*n) {
    indices <- c(indices, i)
  }
}
list_df_2n_no_moins <- list(indices) # liste des df >2n contenant moins de 2n "no option"
df_2n_no_moins <- dataframes_possibles_plus[indices] # convertis en dataframes
View(df_2n_no_moins)

## Pour chaque df_2n_no_moins, on fait toutes les combinaisons de n paires de kin possibles 
linked_all <- list()
for(y in 1:length(df_2n_no_moins)) {
  df_2n_no_moins_option <- subset(df_2n_no_moins[[y]], possibility == "option")
  df_2n_no_moins_nooption <- subset(df_2n_no_moins[[y]], possibility == "no option") 
  # on va travailler que sur les "option", pcq on garde tous les "no option"
  (m<-nrow(df_2n_no_moins[[y]])) # nombre de lignes du df
  (k<-m-(2*n)) # nombre de lignes à retirer pour avoir 2n lignes
  (l<-nrow(df_2n_no_moins_option)) # nombre de paires optionnelles
  (q<-l-k) # nombre de paires optionnelles à conserver
  # on fait toutes les combinaisons de q paires de kin optionnelles possibles
  combinaisons_options <- combinations(df_2n_no_moins_option$pair_id, k=q)
  # transformation en tableau
  new_df_o <- lapply(1:nrow(combinaisons_options), function(i) transformer_en_dataframe(combinaisons_options[i, ]))
  # on remplit tous les tableaux (type id1, id2, kinship, kindegree, pair_id)
  for(i in 1:length(new_df_o)) {
    for(x in 1:nrow(new_df_o[[i]])) {
      pair <- new_df_o[[i]]$pair_id[x]
      loc <- which(new_df_o[[i]]$pair_id[x]==df_2n_no_moins_option$pair_id)# donne la ligne dans df_2n_no_moins_option où la pair_id est la même
      if(pair == df_2n_no_moins_option$pair_id[loc]) {
        kinship <- df_2n_no_moins_option$kinship[df_2n_no_moins_option$pair_id == pair]
        kindegree <- df_2n_no_moins_option$kindegree[df_2n_no_moins_option$pair_id == pair]
        possibility <- df_2n_no_moins_option$possibility[df_2n_no_moins_option$pair_id == pair]
        id1 <- df_2n_no_moins_option$id1[df_2n_no_moins_option$pair_id == pair]
        id2 <- df_2n_no_moins_option$id2[df_2n_no_moins_option$pair_id == pair]
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
    linked_df <- rbind(df_2n_no_moins_nooption, df)
    linked_dataframes <- c(linked_dataframes, list(linked_df))
    }
  linked_all <- c(linked_all, list(linked_dataframes))
} # on obtient une liste "linked_all" regroupant les listes "linked_dataframes" des dataframes
# !!! attention : toutes les combinaisons des kin optionnels ayant été fait, dans certains cas, certains individus nk ne sont plus présents dans les kin.
# donc il faut re-sélectionner uniquement les df où les nk sont présents dans les kin

# on commence par dé-lister linked_all
linked_all_simple <- list()
for (i in 1:length(linked_all)) {
  linked_all_simple <- c(linked_all_simple, linked_all[[i]])
}
View(linked_all_simple)

## Retirer les df où la liste des individus kin est différente de celle des nk
# Création d'une fonction pour retirer le df quand les deux listes ne sont pas identiques
filter_and_check<-function(df) {
  k0 <- subset(df, kinship == "0")
  k1 <- subset(df, kinship == "1")
  ind_k0 <- sort(unique(c(k0$id1, k0$id2)))
  ind_k1 <- sort(unique(c(k1$id1, k1$id2)))
  identical(ind_k0, ind_k1)
}
# Application de la fonction
filtered_linked_all_simple <- lapply(linked_all_simple, function(df) {
  if(filter_and_check(df)) {
    return(df)
  }
})
filtered_linked_all_simple <- filtered_linked_all_simple[!sapply(filtered_linked_all_simple, is.null)] # Supprimer les éléments vides de la liste
# je ne sais pas comment vérifier s'il a bien fait ça


## 3.4 Regroupement de tous les training set utilisables

# on regroupe tous les training sets utilisables 
# Donc: les df qui étaient à 2n pile, les df >2n qui avaient 2n "no option" et les df >2n avec moins de 2n "no option" après modification et sélection
all_training_sets <- c(dataframes_possibles_pile, df_2n_no_pile, filtered_linked_all_simple) # ce sont tous les training sets utilisables

# on exporte tous les training sets
chemin <- "D:/Stage M2 CdP/Kinship/Training sets/"
for (i in 1:length(all_training_sets)) {
  nom_fichier <- paste0("training_set_", i, ".xlsx")
  chemin_fichier <- file.path(chemin, nom_fichier)
  write.xlsx(all_training_sets[[i]], file = chemin_fichier)
}














