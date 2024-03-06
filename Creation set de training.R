#clean console, environment & plots
rm(list = ls())
cat("\014")
# Clear all plots
try(dev.off(dev.list()["RStudioGD"]),silent=TRUE)
try(dev.off(),silent=TRUE)

install.packages("readxl")
install.packages("arrangements")
install.packages("tidyr")
install.packages("dplyr")
install.packages("openxlsx")

library(readxl)
library(arrangements)
library(tidyr)
library(dplyr)
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
# 3eme partie : Création de tous les training sets possibles et utilisables
# 4eme partie : Evaluation de la qualité des training sets (pour le moment uniquement via le kinship)

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

###############################################################################
##### Partie 3 : Création de tous les training sets possibles et utilisables

# Fannie
data <- read_excel("C:/Users/Fannie Beurrier/Documents/Stage M2 CdP/Kinship/Tableau Paires.xlsx", 
                   sheet = "Feuil2")
View(data)

## Attention : la variable "kindegree" ne correspond pas au réel kindegree des paires, il permet de différencier les liens de parenté de la facon suivante:
# Pour les kin : 1 = mother-child ; 0.5 = full-siblings ; 0.25 = maternal half-siblings
# Pour les nk : 0.0625 = cousins ; 0 = au-dela de cousins

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
identical(individuals_kinship_0, individuals_kinship_1) # yoh n'est présente que dans les kin


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
  df$age_id1<-NA
  df$age_id2<-NA
  df$age_dif<-NA
  df$age_mean<-NA
  df$sex_id1<-NA
  df$sex_id2<-NA
  df$sex_dif<-NA
#  df$rank_id1<-NA
#  df$rank_id2<-NA
#  df$rank_dif<-NA
#  df$rank_mean<-NA
  df$possibility<-NA
  return(df)
}
# Application de la fonction à chaque combinaisons de paires de nk
# prend beaucoup de temps
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
      age_id1 <- kinship_0$age_id1[kinship_0$pair_id == pair]
      age_id2 <- kinship_0$age_id2[kinship_0$pair_id == pair]
      age_dif <- kinship_0$age_dif[kinship_0$pair_id == pair]
      age_mean <- kinship_0$age_mean[kinship_0$pair_id == pair]
      sex_id1 <- kinship_0$sex_id1[kinship_0$pair_id == pair]
      sex_id2 <- kinship_0$sex_id2[kinship_0$pair_id == pair]
      sex_dif <- kinship_0$sex_dif[kinship_0$pair_id == pair]
#      rank_id1 <- kinship_0$rank_id1[kinship_0$pair_id == pair]
#      rank_id2 <- kinship_0$rank_id2[kinship_0$pair_id == pair]
#      rank_dif <- kinship_0$rank_dif[kinship_0$pair_id == pair]
#      rank_mean <- kinship_0$rank_mean[kinship_0$pair_id == pair]
      possibility <- kinship_0$possibility[kinship_0$pair_id == pair]
      id1 <- kinship_0$id1[kinship_0$pair_id == pair]
      id2 <- kinship_0$id2[kinship_0$pair_id == pair]
      nouveaux_dataframes[[i]]$kinship[x] <- kinship # pour chaque variable, restitution des valeurs des objets définis dans le nouveaux_dataframes i à la ligne x
      nouveaux_dataframes[[i]]$kindegree[x] <- kindegree
      nouveaux_dataframes[[i]]$age_id1[x] <- age_id1
      nouveaux_dataframes[[i]]$age_id2[x] <- age_id2
      nouveaux_dataframes[[i]]$age_dif[x] <- age_dif
      nouveaux_dataframes[[i]]$age_mean[x] <- age_mean
      nouveaux_dataframes[[i]]$sex_id1[x] <- sex_id1
      nouveaux_dataframes[[i]]$sex_id2[x] <- sex_id2
      nouveaux_dataframes[[i]]$sex_dif[x] <- sex_dif
#      nouveaux_dataframes[[i]]$rank_id1[x] <- rank_id1
#      nouveaux_dataframes[[i]]$rank_id2[x] <- rank_id2
#      nouveaux_dataframes[[i]]$rank_dif[x] <- rank_dif
#      nouveaux_dataframes[[i]]$rank_mean[x] <- rank_mean
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
        age_id1 <- df_2n_no_moins_option$age_id1[df_2n_no_moins_option$pair_id == pair]
        age_id2 <- df_2n_no_moins_option$age_id2[df_2n_no_moins_option$pair_id == pair]
        age_dif <- df_2n_no_moins_option$age_dif[df_2n_no_moins_option$pair_id == pair]
        age_mean <- df_2n_no_moins_option$age_mean[df_2n_no_moins_option$pair_id == pair]
        sex_id1 <- df_2n_no_moins_option$sex_id1[df_2n_no_moins_option$pair_id == pair]
        sex_id2 <- df_2n_no_moins_option$sex_id2[df_2n_no_moins_option$pair_id == pair]
        sex_dif <- df_2n_no_moins_option$sex_dif[df_2n_no_moins_option$pair_id == pair]
#        rank_id1 <- df_2n_no_moins_option$rank_dif[df_2n_no_moins_option$pair_id == pair]
#        rank_id2 <- df_2n_no_moins_option$rank_dif[df_2n_no_moins_option$pair_id == pair]
#        rank_dif <- df_2n_no_moins_option$rank_dif[df_2n_no_moins_option$pair_id == pair]
#        rank_mean <- df_2n_no_moins_option$rank_mean[df_2n_no_moins_option$pair_id == pair]
        possibility <- df_2n_no_moins_option$possibility[df_2n_no_moins_option$pair_id == pair]
        id1 <- df_2n_no_moins_option$id1[df_2n_no_moins_option$pair_id == pair]
        id2 <- df_2n_no_moins_option$id2[df_2n_no_moins_option$pair_id == pair]
        new_df_o[[i]]$kinship[x] <- kinship
        new_df_o[[i]]$kindegree[x] <- kindegree
        new_df_o[[i]]$age_id1[x] <- age_id1
        new_df_o[[i]]$age_id2[x] <- age_id2
        new_df_o[[i]]$age_dif[x] <- age_dif
        new_df_o[[i]]$age_mean[x] <- age_mean
        new_df_o[[i]]$sex_id1[x] <- sex_id1
        new_df_o[[i]]$sex_id2[x] <- sex_id2
        new_df_o[[i]]$sex_dif[x] <- sex_dif
#        new_df_o[[i]]$rank_id1[x] <- rank_id1
#        new_df_o[[i]]$rank_id2[x] <- rank_id2
#        new_df_o[[i]]$rank_dif[x] <- rank_dif
#        new_df_o[[i]]$rank_mean[x] <- rank_mean
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

# on commence par dé-lister linked_all (car c'est une liste de liste)
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


## 3.5 Sélection des training sets selon les critères de kindegree
# contrainte 1 : on veut uniquement les training sets avec les 3 kindegree differents pour les kin (1, 0.5 et 0.25)
# contrainte 2 : on veut uniquement les training sets avec les 2 kindegree differents pour les nk (0 et 0.0625)

# Définir une fonction pour vérifier les conditions
check_conditions <- function(df) {
  kinship_1_data <- subset(df, kinship == 1) # on va filtrer les données pour kinship = 1
  # Vérifier si kindegree prend au moins une fois les valeurs 0.25, 0.5 et 1
  contains_025 <- 0.25 %in% kinship_1_data$kindegree
  contains_05 <- 0.5 %in% kinship_1_data$kindegree
  contains_1 <- 1 %in% kinship_1_data$kindegree
  kinship_0_data <- subset(df, kinship == 0) # on va filtrer les données pour kinship = 0
  # Vérifier si kindegree prend au moins une fois les valeurs 0.0625 et 0
  contains_00625 <- 0.0625 %in% kinship_0_data$kindegree
  contains_0 <- 0 %in% kinship_0_data$kindegree
  # Renvoyer TRUE si toutes les conditions sont remplies, sinon FALSE
  return(contains_025 & contains_05 & contains_1 & contains_0 & contains_00625)
}
# Filtrer la liste all_training_sets selon les conditions
filtered_training_sets <- all_training_sets[sapply(all_training_sets, check_conditions)]

## Maintenant, filtered_training_sets contient tous les dataframes qu'on peut potentiellement utiliser


### 4. Exportation de tous les training sets

# on exporte tous les training sets en xlsx
chemin <- "D:/Stage M2 CdP/Kinship/Training sets/"
for (i in 1:length(filtered_training_sets)) {
  nom_fichier <- paste0("training_set_", i, ".xlsx")
  chemin_fichier <- file.path(chemin, nom_fichier)
  write.xlsx(filtered_training_sets[[i]], file = chemin_fichier)
}

#######################################################

##### Partie 4 : Evaluation de la qualité des sets de training

### 1. Création des tableaux des combinaisons

# D'abord, il faut faire toutes les combinaisons de paires de kin et nk possibles pour chaque training set

# Définir une fonction pour générer les combinaisons de pair_id entre k0 et k1
generate_combinations <- function(df) {
  # Filtrer les données pour k0 et k1
  k0 <- subset(df, kinship == 0)
  k1 <- subset(df, kinship == 1)
  # Extraire les valeurs uniques de pair_id pour k0 et k1
  k0_p_id <- unique(k0$pair_id)
  k1_p_id <- unique(k1$pair_id)
  # Générer toutes les combinaisons possibles de pair_id entre k0 et k1
  combinations <- expand.grid(k0_p_id, k1_p_id)
  colnames(combinations) <- c("pair_id_k0", "pair_id_k1")
  combinations$combi_id <- apply(combinations[1:2], 1, function(x) paste(x, collapse = "-")) # création de la variable pair_id pour attribuer une identité à la paire
  combinations$id1<-NA
  combinations$id2<-NA
  combinations$id3<-NA
  combinations$id4<-NA
  combinations$kinship_p1<-NA  
  combinations$kinship_p2<-NA  
  combinations$kindegree_p1<-NA 
  combinations$kindegree_p2<-NA
  combinations$age_dif_p1<-NA
  combinations$age_dif_p2<-NA
  combinations$age_mean_p1<-NA
  combinations$age_mean_p2<-NA
  combinations$sex_dif_p1<-NA
  combinations$sex_dif_p2<-NA
#  combinations$rank_dif_p1<-NA
#  combinations$rank_dif_p2<-NA
#  combinations$rank_mean_p1<-NA
#  combinations$rank_mean_p2<-NA
  return(combinations)
}
# Appliquer la fonction à chaque dataframe de filtered_training_sets
all_combinations <- lapply(filtered_training_sets, generate_combinations)

# Maintenant, all_combinations contient une liste de dataframes, chaque dataframe représentant toutes les combinaisons possibles de pair_id entre k0 et k1 pour un dataframe de filtered_training_sets.

# Remplissage des tableaux de combinaisons de paires de nonkin (type id1, id2, kinship, kindegree, pair_id) à partir des données initiales
# prend un peu de temps
for(i in 1:length(all_combinations)) {
  for(x in 1:nrow(all_combinations[[i]])) {
    pair0 <- all_combinations[[i]]$pair_id_k0[x]
    pair1 <- all_combinations[[i]]$pair_id_k1[x]
    loc0 <- which(all_combinations[[i]]$pair_id_k0[x]==data$pair_id)# donne la ligne dans data où la pair_id_k0 est la même que la ligne x du all_combinations i
    loc1 <- which(all_combinations[[i]]$pair_id_k1[x]==data$pair_id)# donne la ligne dans data où la pair_id_k1 est la même que la ligne x du all_combinations i
    if(pair0 == data$pair_id[loc0]) {
      kinship_p1 <- data$kinship[data$pair_id == pair0] # pour chaque variable, attribution de la valeur de la variable à un objet (ici appelé "kinship")
      kindegree_p1 <- data$kindegree[data$pair_id == pair0]
      age_dif_p1 <- data$age_dif[data$pair_id == pair0]
      age_mean_p1 <- data$age_mean[data$pair_id == pair0]
      sex_dif_p1 <- data$sex_dif[data$pair_id == pair0]
#      rank_dif_p1 <- data$rank_dif[data$pair_id == pair0]
#      rank_mean_p1 <- data$rank_mean[data$pair_id == pair0]
      id1 <- data$id1[data$pair_id == pair0]
      id2 <- data$id2[data$pair_id == pair0]
      all_combinations[[i]]$kinship_p1[x] <- kinship_p1 # pour chaque variable, restitution des valeurs des objets définis dans le all_combinations i à la ligne x
      all_combinations[[i]]$kindegree_p1[x] <- kindegree_p1
      all_combinations[[i]]$age_dif_p1[x] <- age_dif_p1
      all_combinations[[i]]$age_mean_p1[x] <- age_mean_p1
      all_combinations[[i]]$sex_dif_p1[x] <- sex_dif_p1
#      all_combinations[[i]]$rank_dif_p1[x] <- rank_dif_p1
#      all_combinations[[i]]$rank_mean_p1[x] <- rank_mean_p1
      all_combinations[[i]]$id1[x] <- id1
      all_combinations[[i]]$id2[x] <- id2
    }
    if(pair1 == data$pair_id[loc1]) {
        kinship_p2 <- data$kinship[data$pair_id == pair1] # pour chaque variable, attribution de la valeur de la variable à un objet (ici appelé "kinship")
        kindegree_p2 <- data$kindegree[data$pair_id == pair1]
        age_dif_p2 <- data$age_dif[data$pair_id == pair1]
        age_mean_p2 <- data$age_mean[data$pair_id == pair1]
        sex_dif_p2 <- data$sex_dif[data$pair_id == pair1]
#      rank_dif_p2 <- data$rank_dif[data$pair_id == pair1]
#      rank_mean_p2 <- data$rank_mean[data$pair_id == pair1]
        id1 <- data$id1[data$pair_id == pair1]
        id2 <- data$id2[data$pair_id == pair1]
        all_combinations[[i]]$kinship_p2[x] <- kinship_p2 # pour chaque variable, restitution des valeurs des objets définis dans le all_combinations i à la ligne x
        all_combinations[[i]]$kindegree_p2[x] <- kindegree_p2
        all_combinations[[i]]$age_dif_p2[x] <- age_dif_p2
        all_combinations[[i]]$age_mean_p2[x] <- age_mean_p2
        all_combinations[[i]]$sex_dif_p2[x] <- sex_dif_p2
#      all_combinations[[i]]$rank_dif_p2[x] <- rank_dif_p2
#      all_combinations[[i]]$rank_mean_p2[x] <- rank_mean_p2
        all_combinations[[i]]$id3[x] <- id1
        all_combinations[[i]]$id4[x] <- id2
    }
  }
}
# tous les dataframes sont remplis

### 2. Evaluation de la qualité en fonction du kinship

# Parcourir chaque dataframe de all_combinations (OSEF car on va choisir que les dataset avec un nombre d'individus = 6 pour que chaque individu soit présent de manière équilibrée dans les kin et nk (pour n=3))
# prend du temps
for (i in seq_along(all_combinations)) { # pour chaque dataframe
  m <- n*n # pour considérer toutes les lignes représentant les différentes combinaisons mais pas les lignes qu'on ajoute en plus par la suite
  df <- all_combinations[[i]]
  unique_individuals <- sort(unique(c(df$id1, df$id2))) # liste des individus présents (OSEF de id3 et id4 car dans tous les cas, ils correspondent à id1 et id2)
  # On crée un nouveau dataframe "df_ind_count" dans lequel on compte le nombre de fois où l'individu apparaît dans le training set pour calculer son score (plus tard)
  liste_individuals <- list(individuals=c(df$id1, df$id2, df$id3, df$id4))
  ind_counts <- table(liste_individuals$individuals) # on compte le nombre de fois que chaque individu est présent en tant que kin
  df_ind_counts <- as.data.frame(ind_counts)
  colnames(df_ind_counts) <- c("ID", "Freq")
  df_ind_counts <- df_ind_counts[order(as.character(df_ind_counts$ID)), ] # organisation par ordre alphabétique (pour une vérification plus facile)
  # Ajouter une nouvelle ligne "total" avec des valeurs NA au dataframe
  total_row <- data.frame(matrix(NA, ncol = ncol(df), nrow = 1))
  colnames(total_row) <- colnames(df)
  rownames(total_row) <- "total"
  df <- rbind(df, total_row)
  # Créer des variables pour chaque individu pour calculer leur score par la suite
  for (individual in unique_individuals) {
    df[[individual]] <- NA
    # Dans chaque variable df[[individual]], on va mettre 1 quand l'individu est dans la paire "gagnante" (kin) et 0 si il est dans la paire "perdante" (nk)
    for (x in 1:m){ # pour que la derniere ligne "total" ne soit pas concernee
      ifelse (df$kindegree_p2[x] > df$kindegree_p1[x] & c(individual %in% df$id3[x] || individual %in% df$id4[x]),df[[individual]][x] <- 1, df[[individual]][x] <- 0 )}
    # Parcourir les colonnes de df qui représentent un individu (donc à partir de 12)
    for (y in 12:ncol(df)) {
      # On va compléter la ligne "total" pour avoir le score de chaque individu. Autrement dit, le nombre de fois où l'individu gagne divisé par le nombre de fois où il apparaît. Plus c'est proche de 0.5, mieux c'est car cela veut dire qu'il apparaît autant de fois en tant que gagnant que perdant
      col_name <- names(df)[y] # Obtenir le nom de la colonne
      somme <- sum(df[[y]][1:m]) # Somme des 1 (donc total du nombre de fois où l'individu "gagne")
      # Si le nom de la colonne correspond à une valeur dans df_ind_counts$ID,
      if (col_name %in% df_ind_counts$ID) {
        # Récupérer l'index correspondant à l'ID dans df_ind_counts
        idx <- which(df_ind_counts$ID == col_name) # on récupère l'index correspondant à l'ID dans df_ind_counts
        freq <- df_ind_counts$Freq[idx] # on attribue la fréquence (df_ind_counts$Freq) à l'objet "freq"
        # on calcule le score pour chaque individu dans df
        df["total", y] <- somme/freq
      }
    }
  }
  # Ajouter une nouvelle ligne "variance" avec des valeurs NA au dataframe
  variance_row <- data.frame(matrix(NA, ncol = ncol(df), nrow = 1))
  colnames(variance_row) <- colnames(df)
  rownames(variance_row) <- "variance"
  df <- rbind(df, variance_row)
  for (z in 12:ncol(df)) {
    df["variance",z] <- abs(df["total", z]-0.5)}
  # Calculer la valeur maximale des valeurs dans les colonnes 14 à la dernière colonne
  max_value <- max(apply(df["variance", 14:ncol(df)], 1, max, na.rm = TRUE))
  # Ajouter une nouvelle colonne "variance_max" avec des valeurs NA au dataframe
  df$variance_max <- NA
  # Assigner la valeur maximale à la cellule "variance_max" dans la ligne "variance"
  df["variance", "variance_max"] <- max_value
  
  # Mettre à jour le dataframe dans la liste
  all_combinations[[i]] <- df
}

### Comme il y a soit 5, soit 6 individus qui composent les training set (si n=3), on ne va sélectionner que les training set avec 6 individus pour que chaque individu soit équilibré et apparaisse autant de fois dans kin que dans nk.

# Création d'une fonction pour conserver uniquement les df où la liste des individus est égale à 6
filter<-function(df) {
  ind <- unique(c(df$id1, df$id2))
  length(ind)==6
}
# Application de la fonction
training_sets_equi <- lapply(filtered_training_sets, function(df) {
  if(filter(df)) {
    return(df)
  }
})
training_sets_equi <- training_sets_equi[!sapply(training_sets_equi, is.null)] # Supprimer les éléments vides de la liste
# je ne sais pas comment vérifier s'il a bien fait ça


### 3. Evaluation de la qualité en fonction du sexe

## Retirer les df où la liste des sex_dif entre kin et nk est différente
# Création d'une fonction pour retirer le df quand les deux listes ne sont pas identiques
filter_and_check<-function(df) {
  k0 <- subset(df, kinship == "0")
  k1 <- subset(df, kinship == "1")
  sex_dif_k0 <- sort(unique(k0$sex_dif))
  sex_dif_k1 <- sort(unique(k1$sex_dif))
  identical(sex_dif_k0, sex_dif_k1)
}
# Application de la fonction
filtered_training_sets_equi <- lapply(training_sets_equi, function(df) {
  if(filter_and_check(df)) {
    return(df)
  }
})
filtered_training_sets_equi <- filtered_training_sets_equi[!sapply(filtered_training_sets_equi, is.null)] # Supprimer les éléments vides de la liste
# je ne sais pas comment vérifier s'il a bien fait ça

# On peut regarder nombre de mâles contenus pour chaque df
for(i in 1:length(filtered_training_sets_equi)) {
  sum_sex <- sum(c(filtered_training_sets_equi[[i]]$sex_id1, filtered_training_sets_equi[[i]]$sex_id2))
  print(c(i,sum_sex/2)) # tous les indices des df et le nombre de mâles dans le set de training sont affichés
}

# Si on veut qu'il y ait au moins un mâle et une femelle dans le set de training, ce qui permet d'avoir la présence des deux sexes
filter_sex<-function(df) {
  sum_sex <- sum(c(df$sex_id1, df$sex_id2))
  nb_males <- sum_sex/2
  between(nb_males,1,5)
}
# Application de la fonction
filtered_training_sets_sex <- lapply(filtered_training_sets_equi, function(df) {
  if(filter_sex(df)) {
    return(df)
  }
})
filtered_training_sets_sex <- filtered_training_sets_sex[!sapply(filtered_training_sets_sex, is.null)] # Supprimer les éléments vides de la liste

# On peut regarder nombre de mâles contenus pour chaque df
for(i in 1:length(filtered_training_sets_sex)) {
  sum_sex <- sum(c(filtered_training_sets_sex[[i]]$sex_id1, filtered_training_sets_sex[[i]]$sex_id2))
  print(c(i,sum_sex/2)) # tous les indices des df et le nombre de mâles dans le set de training sont affichés
}

## Dans l'idéal on ferait ce qui suit mais ça réduit beaucoup trop le nombre de training sets et certains individus sont constamment dedans
# Si on veut qu'il y ait au moins un paire de mâles, une paire de femelle et une paire mixte pour les kin et nk
# Définir une fonction pour vérifier les conditions
check_conditions <- function(df) {
  # Vérifier si sex_dif prend au moins une fois les valeurs 0, 0.5 et 1
  contains_0 <- 0 %in% df$sex_dif
  contains_05 <- 0.5 %in% df$sex_dif
  contains_1 <- 1 %in% df$sex_dif
  # Renvoyer TRUE si toutes les conditions sont remplies, sinon FALSE
  return(contains_0 & contains_05 & contains_1)
}
# Filtrer la liste filtered_training_sets_equi selon les conditions
filtered_training_sets_sex <- filtered_training_sets_equi[sapply(filtered_training_sets_equi, check_conditions)]

# On peut regarder la liste des individus pour chaque df, pour voir si des individus sont constamment présents dans les df
for(i in 1:length(filtered_training_sets_sex)) {
  list_ind <- sort(unique(c(filtered_training_sets_sex[[i]]$id1, filtered_training_sets_sex[[i]]$id2)))
  print(c(i,list_ind)) # tous les indices des df et la liste des individus dans le set de training sont affichés
}


### 4. Evaluation de la qualité en fonction de l'âge
# On veut que la moyenne des différences d'âge pour les kin et nk soit la plus proche (donc on veut pas que le singe puisse choisir en fonction de la dif d'âge entre les paires (par ex. choisir toujours la paire avec la plus grande dif d'âge))
# On veut que la moyenne des âges moyens pour les kin et nk soit la plus proche (donc on veut pas que le singe puisse choisir en fonction de la moy d'âge des paires (par ex. choisir toujours la paire la plus jeune en moyenne))

# Parcourir chaque dataframe de filtered_training_sets_sex
for (i in seq_along(filtered_training_sets_sex)) { # pour chaque dataframe
  m <- 2*n # pour considérer toutes les lignes représentant les différentes combinaisons mais pas les lignes qu'on ajoute en plus par la suite
  df <- filtered_training_sets_sex[[i]]
  k0 <- subset(df, kinship == "0")
  k1 <- subset(df, kinship == "1")
  # Ajouter 3 nouvelles lignes "age_k0", "age_k1" & "dif_age" avec des valeurs NA au dataframe
  age_row <- data.frame(matrix(NA, ncol = ncol(df), nrow = 3))
  colnames(age_row) <- colnames(df)
  rownames(age_row) <- c("age_k0","age_k1","dif_age")
  df <- rbind(df, age_row)
  df["age_k0","age_dif"] <- mean(k0$age_dif)
  df["age_k1","age_dif"] <- mean(k1$age_dif)
  df["dif_age","age_dif"] <- abs(df["age_k0","age_dif"]-df["age_k1","age_dif"]) # donne la différence (valeur absolue), entre les kin et nk, de la moyenne des différences d'âge
  df["age_k0","age_mean"] <- mean(k0$age_mean)
  df["age_k1","age_mean"] <- mean(k1$age_mean)
  df["dif_age","age_mean"] <- abs(df["age_k0","age_mean"]-df["age_k1","age_mean"]) # donne la différence (valeur absolue), entre les kin et nk, de la moyenne des moyennes d'âge
  # Mettre à jour le dataframe dans la liste
  filtered_training_sets_sex[[i]] <- df
}

# Si on veut que les deux différences calculées soient inférieures à 1 an d'écart
# Définir une fonction pour vérifier les conditions
check_conditions <- function(df) {
  # Vérifier si les deux différences sont < 1
  condition1 <- df["dif_age","age_dif"] < 1
  condition2 <- df["dif_age","age_mean"] < 1
  # Renvoyer TRUE si toutes les conditions sont remplies, sinon FALSE
  return(condition1 & condition2)
}
# Filtrer la liste filtered_training_sets_sex selon les conditions
filtered_training_sets_age <- filtered_training_sets_sex[sapply(filtered_training_sets_sex, check_conditions)]


## Autre manière de faire
all_combinations <- lapply(filtered_training_sets_sex, generate_combinations)
# Remplissage des tableaux de combinaisons de paires de nonkin (type id1, id2, kinship, kindegree, pair_id) à partir des données initiales
# prend un peu de temps
for(i in 1:length(all_combinations)) {
  for(x in 1:nrow(all_combinations[[i]])) {
    pair0 <- all_combinations[[i]]$pair_id_k0[x]
    pair1 <- all_combinations[[i]]$pair_id_k1[x]
    loc0 <- which(all_combinations[[i]]$pair_id_k0[x]==data$pair_id)# donne la ligne dans data où la pair_id_k0 est la même que la ligne x du all_combinations i
    loc1 <- which(all_combinations[[i]]$pair_id_k1[x]==data$pair_id)# donne la ligne dans data où la pair_id_k1 est la même que la ligne x du all_combinations i
    if(pair0 == data$pair_id[loc0]) {
      kinship_p1 <- data$kinship[data$pair_id == pair0] # pour chaque variable, attribution de la valeur de la variable à un objet (ici appelé "kinship")
      kindegree_p1 <- data$kindegree[data$pair_id == pair0]
      age_dif_p1 <- data$age_dif[data$pair_id == pair0]
      age_mean_p1 <- data$age_mean[data$pair_id == pair0]
      sex_dif_p1 <- data$sex_dif[data$pair_id == pair0]
      #      rank_dif_p1 <- data$rank_dif[data$pair_id == pair0]
      #      rank_mean_p1 <- data$rank_mean[data$pair_id == pair0]
      id1 <- data$id1[data$pair_id == pair0]
      id2 <- data$id2[data$pair_id == pair0]
      all_combinations[[i]]$kinship_p1[x] <- kinship_p1 # pour chaque variable, restitution des valeurs des objets définis dans le all_combinations i à la ligne x
      all_combinations[[i]]$kindegree_p1[x] <- kindegree_p1
      all_combinations[[i]]$age_dif_p1[x] <- age_dif_p1
      all_combinations[[i]]$age_mean_p1[x] <- age_mean_p1
      all_combinations[[i]]$sex_dif_p1[x] <- sex_dif_p1
      #      all_combinations[[i]]$rank_dif_p1[x] <- rank_dif_p1
      #      all_combinations[[i]]$rank_mean_p1[x] <- rank_mean_p1
      all_combinations[[i]]$id1[x] <- id1
      all_combinations[[i]]$id2[x] <- id2
    }
    if(pair1 == data$pair_id[loc1]) {
      kinship_p2 <- data$kinship[data$pair_id == pair1] # pour chaque variable, attribution de la valeur de la variable à un objet (ici appelé "kinship")
      kindegree_p2 <- data$kindegree[data$pair_id == pair1]
      age_dif_p2 <- data$age_dif[data$pair_id == pair1]
      age_mean_p2 <- data$age_mean[data$pair_id == pair1]
      sex_dif_p2 <- data$sex_dif[data$pair_id == pair1]
      #      rank_dif_p2 <- data$rank_dif[data$pair_id == pair1]
      #      rank_mean_p2 <- data$rank_mean[data$pair_id == pair1]
      id1 <- data$id1[data$pair_id == pair1]
      id2 <- data$id2[data$pair_id == pair1]
      all_combinations[[i]]$kinship_p2[x] <- kinship_p2 # pour chaque variable, restitution des valeurs des objets définis dans le all_combinations i à la ligne x
      all_combinations[[i]]$kindegree_p2[x] <- kindegree_p2
      all_combinations[[i]]$age_dif_p2[x] <- age_dif_p2
      all_combinations[[i]]$age_mean_p2[x] <- age_mean_p2
      all_combinations[[i]]$sex_dif_p2[x] <- sex_dif_p2
      #      all_combinations[[i]]$rank_dif_p2[x] <- rank_dif_p2
      #      all_combinations[[i]]$rank_mean_p2[x] <- rank_mean_p2
      all_combinations[[i]]$id3[x] <- id1
      all_combinations[[i]]$id4[x] <- id2
    }
  }
}
# tous les dataframes sont remplis

## On veut que la règle testée ne puisse pas se confondre avec la règle du kinship
# selon la règle du choix de la paire ayant la plus grande différence d'âge
for (i in seq_along(all_combinations)) { # pour chaque dataframe
  df <- all_combinations[[i]]
  # Ajouter 2 nouvelles colonnes "test_p1" & "test_p2" avec des valeurs NA au dataframe
  test_col <- data.frame(matrix(NA, ncol = 2, nrow = nrow(df)))
  colnames(test_col) <- c("test_p1", "test_p2")
  rownames(test_col) <- rownames(df)
  df <- cbind(df, test_col)
  for (x in 1:nrow(df)){
    ifelse(df$age_dif_p2[x] > df$age_dif_p1[x], df$test_p2[x] <- 1, df$test_p2[x] <- 0)
    ifelse(df$age_dif_p2[x] < df$age_dif_p1[x], df$test_p1[x] <- 1, df$test_p1[x] <- 0)
  }
  # Mettre à jour le dataframe dans la liste
  all_combinations[[i]] <- df
}

# l'idée, c'est de comparer les colonnes "test" avec les colonnes "kinship". Il faut qu'elles soient différentes pour être sûr que la règle testée ne puisse pas être utilisée à la place de la règle du kinship.
for (i in seq_along(all_combinations)) { # pour chaque dataframe
  m <- n*n
  df <- all_combinations[[i]]
  # Ajouter une nouvelle ligne "total" avec des valeurs NA au dataframe
  total_row <- data.frame(matrix(NA, ncol = ncol(df), nrow = 1))
  colnames(total_row) <- colnames(df)
  rownames(total_row) <- "total"
  df <- rbind(df, total_row)
  df["total","kinship_p2"] <- sum(df$kinship_p2[1:m]) # donne la somme des 1 dans kinship_p2
  df["total","test_p2"] <- sum(df$test_p2[1:m]) # donne la somme des 1 dans test_p2
  # Ajouter une nouvelle colonne "similarity" avec des valeurs NA au dataframe
  similarity_col <- data.frame(matrix(NA, ncol = 1, nrow = nrow(df)))
  rownames(similarity_col) <- rownames(df)
  colnames(similarity_col) <- "similarity_p2"
  df <- cbind(df, similarity_col)
  df["total","similarity_p2"] <- df["total","test_p2"]*100/df["total","kinship_p2"] # donne le pourcentage de similarité de test_p2 par rapport à kinship_p2 (qui est la paire de kin). Plus c'est proche de 0, mieux c'est.
  # Mettre à jour le dataframe dans la liste
  all_combinations[[i]] <- df
}


















