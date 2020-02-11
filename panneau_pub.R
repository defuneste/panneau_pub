### script pour transformer les données mobilié urbain du Grand Lyon avec les bon tags OSM
# source metadonnée :https://data.grandlyon.com/jeux-de-donnees/mobilier-urbain-metropole-lyon/info
# source fichier : https://data.grandlyon.com/jeux-de-donnees/mobilier-urbain-metropole-lyon/ressources
# dl le 11/02/2020


### 1 Chargement des packages ================

library(sp) #  veux package spatiaux R
library(sf) # simple feature for R
library(dplyr) # analyse de données tidyverse
library(ggplot2) # graph rapide
library(stringr) # modif texte 

#### 2 Chargement du geojson =============

panneau_pub <- st_read("data/pvo_patrimoine_voirie.pvomobilierurbain.shp")


#### 3 Analyse rapide =============


#verif du jeux de données et de sa structure
summary(panneau_pub)
str(panneau_pub)

# le fichier comporte 2718 points avec 8 variables

# identifian doit correspondre à l'identifiant de chaque objet dans la base "metier"
# a moins que cela ne soit gid ?

# nom_abri il y 635 valeurs manquante à voir si cela correspond à quelque chose

# adresse une adresse Rue il y a une donnée manquante

panneau_pub[is.na(panneau_pub$adresse),]
# cela correspond à l'identifian AB00201 'Pont de la Gravière' à Sainte-Foy-les-lyon

# commune et cod_insee pe redondant il y a aussi un NA

panneau_pub[is.na(panneau_pub$commune),]
# cela correspond à lidentifian AB01111 adresse Montée des Esses

# famillemob semble être le plus interessant 

panneau_pub %>%
  st_drop_geometry() %>% 
  group_by(famillemob) %>% 
  summarize(nombre = n()) %>% 
ggplot( aes(x = reorder(famillemob, -nombre), y = nombre,  fill = famillemob)) +
  geom_bar(stat = "identity") +
  labs(x = "") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


#### 4 Export =============

#nb va poser pb avec mon NA sur le code_insee

outlist <- list() # initialisation d'un liste
longueur <- length(unique(panneau_pub$code_insee)) # le nombre de fichier souhaité

for(i in 1:longueur) { 
  # on passe par une liste, c'est pas indispensable mais je voulais verifier un peu avant d'ecrire des fichier
  outlist[[i]] <- panneau_pub %>%
    filter(code_insee == unique(panneau_pub$code_insee)[i]) 
  # on écrit des tas de fichiers 
  st_write(outlist[[i]], dsn = paste0(unique(panneau_pub$code_insee)[i], ".geojson"))
}

rm(longueur, outlist)