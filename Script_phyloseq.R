#Script de TD de bioinformatique
#Par Arthur Cousson
# arthur.cousson@ird.fr

#Je vous remercierai de ne pas distribuer ce script n'importe comment.
#Il s'agit d'une synthèse de mon travail. 
#Merci de le respecter.



#Pour commencer, appuyez sur le bouton tout à droite de la fenêtre de texte pour dérouler les chapitres du script
#Juste à droit du bouton "Source"
#Voilà, bravo, pensez à utiliser cette fonctionalité qui vous aide à vous y retrouver
#regardez comment sont structurés mes titres, avec 4 tirets à la suite pour marquer le formatage






#I-Installations ----
##I-1 R -----
#Linux
#A insérer dans le terminal sans le dièse de début de ligne
apt-get update 
apt-get install r-base r-base-dev
#Attention, ce ne sont pas des commandes pour R mais pour le terminal Unix (pas le terminal Windows non plus)


#Pour les autres 
# https://cran.rstudio.com/

##I-2 Rstudio ----

#Pour installer R studio
# https://posit.co/download/rstudio-desktop/


##I-3 Phyloseq ----
#Pour installer le package d'analyse de microbiome le plus utilisé : Phyloseq
#Temps estimé : 15 minutes
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("phyloseq")

#Une fois installé, vous pouvez commenter les lignes.
#Ajoutez un dièse au début de la ligne, afin d'éviter de réinstaller à chaque fois, ce qui est plutôt long.

#Pendant que ça s'installe, lisez donc l'introduction à phyloseq sur leur site officiel :

# https://joey711.github.io/phyloseq/

##I-4 Packages ----
install.packages(c("vegan","ape", "ggplot2", "RColorBrewer", "grid"))

library(vegan)
library(ape)
library(ggplot2)
library(phyloseq)
library(RColorBrewer)
library(grid)
#II-Phyloseq----

##II-A Enterotype ----
data("enterotype")


#Extraction de la table de métadonnées de l'objet phyloseq
metadata <- data.frame(enterotype@sam_data)
#Consultez le haut du tableau metadata avec la fonction suivante :
head(metadata)
#Gardez en tête les différents intitulés de metadata en nom de colonne.
#Ce sont les seuls paramètres explicatifs dont vous disposerez
#Ecrivez le genre d'informations que vous auriez aimé voir dans ce jeu de données pour maximiser votre analyse
#QUESTION -----
#
#
#
#
#
#
#
#Pour faciliter la lecture et l'interopérabilité des fonctions
data <- enterotype
?enterotype

#Le séquençage est inégal entre les échantillons.
#C'est inhérent au fonctionnement des séquenceurs. 
#Il faut tenir compte de ces variations de profondeur de séquençage.
#Pour normaliser les abondances absolues en relatives. 
#Attention, il eiste de très nombreuses façons de normaliser, 
#pour donner du poids à certains groupes, prendre en compte la phylogénie etc.
#Dans ce cas, le dataset est déjà normalisé en amont de notre intervention
#Comme on peut le voir avec la fonction suivante
sample_sums(data)

#Dans le cas d'un jeu de données non normalisé, on peut utiliser la fonction suivante
#Elle transformera les abondances brutes en abondances relatives
#data <- transform_sample_counts(data, function(x) x / sum(x) )

#Avec la fonction vegdist, on produit la matrice de distance de Bray-Curtis
#Idem, Bray-Curtis est l'une des plus populaire car elle est très simple. 
#Il en existe qui sont plus réductioniste (JAccard), d'autres qui sont plus holistes (UniFrac)...S
##II-A-1 Distance matrix -----
dist.bray <- vegdist(t(data@otu_table), method = "bray") #Cette fonction provient du package vegan
#D'autres mesures de distance pouvant être utilisées dans cette fonction
#"manhattan", "euclidean", "canberra", "clark", "bray", "kulczynski", "jaccard", "gower", "altGower", "morisita", "horn", "mountford", "raup", "binomial", "chao", "cao", "mahalanobis", "chisq", "chord", "hellinger", "aitchison", or "robust.aitchison".
?vegdist()
##II-A-2- PCoA ---- 
#Génération de la PCoA
pcoa.sub <- pcoa(dist.bray) #La fonction pcoa provient du package ape

?pcoa()

#Transformation de la liste en dataframe pour des questions d'accessibilité.
#Il est moins facile d'accéder à l'information dans une liste que dans un dartaframe pour ggplot2

beta_tab <- cbind(pcoa.sub$vectors[,1:2], data@sam_data) #cbind veut dire column binding, pour coller deux tableaux aux mêmes intitulés de ligne ensemble

?cbind()



#Information sur les graphiques :
?ggplot()

##II-A-3-Graphique PCoA ---- 
#Ordination pour réduction dimensionelle


ggplot(beta_tab) + theme_bw() +
  geom_point(aes(x = Axis.1, y = Axis.2),
             size = 5) +
  scale_color_discrete() +
  geom_hline(yintercept = 0, lty = 3) +
  geom_vline(xintercept = 0, lty = 3) +
  xlab(paste("PCo1 (", round(pcoa.sub$values$Relative_eig[1]*100, 1), "%)")) +
  ylab(paste("PCo2 (", round(pcoa.sub$values$Relative_eig[2]*100, 1), "%)")) +
  labs(title = "Microbiota PCoA \nBray-Curtis distance matrix",
       #subtitle = "Plot of length by dose",
       caption = date(), 
       color = "Modify here in script to change the parameter label")

#Que voyez vous sur ce graphuique ?
#Pouvez vous l'interpréter tel quel ? 
#Emettez des hypothèses à partir de vos connaissances sur le microbiote humain et le cours d'aujourd'hui 
#quant à la tendance observée sur ce graphique.
#QUESTION ----
#
#
#
#
#
#
#
#
#
#
#
#
#
#

#Voici une fonction graphique qui va vous permettre d'explorer graphiquement
#Les relations entre les paramètres explicatifs (metadata) et les distances entre les echantillons
#Attention, le script suivant ne tourne pas tout seul. Vous devez trouver la différence qui existe par rapport
#à la fonction précédente. Une fois identifiée, vous devez tester différents paramètres
#Une fois que vous avez un graph qui vous plait, copiez-collez la fonction et essayez d'autres paramètres

#EXERCICE ----
ggplot(beta_tab) + theme_bw() +
  geom_point(aes(x = Axis.1, y = Axis.2, 
                 color = ),
             size = 5) +
  scale_color_discrete() +
  geom_hline(yintercept = 0, lty = 3) +
  geom_vline(xintercept = 0, lty = 3) +
  xlab(paste("PCo1 (", round(pcoa.sub$values$Relative_eig[1]*100, 1), "%)")) +
  ylab(paste("PCo2 (", round(pcoa.sub$values$Relative_eig[2]*100, 1), "%)")) +
  labs(title = "Microbiota PCoA \nBray-Curtis distance matrix",
       #subtitle = "Plot of length by dose",
       caption = date(), 
       color = "Modify here to change the parameter label")

#QUESTION ----
#Quels paramètres à votre avis pertinents avez vous identifiez pour expliquer la séparations des échantillons ?
#
#
#
#
#
#
#
#Pensez vous qu'un graphique soit suffisant pour faire une observation sérieuse ?
#Dans tous les cas de réponse, justifiez la, proposez des choses.
#QUESTION ----
#
#
#
#
#
#
#

#Classification hierarchique ----
#Cette fonction vous permet de générer la classification hiérarchique de vos échantillons 
#en fonction des distances qui séparent les communautés dans l'espace des dissimilarités
plot(hclust(dist.bray, method = "complete", members = metadata$SeqTech))
#QUESTION ----
#D'après vous, comment peut on mettre en lien la classification hiérarchique et le graphique de PCoA ?
#
#
#
#
#
#
#


##II-A-4 Stats ----
#Bon, bien évidemment, un graph ne suffit pas.
#Même si il est très explicite
#Il faut faire un test statistique.
#Pour tester la séparation dans un espace de distance, il faut réaliser une permanova
?adonis2()

#Attention, la fonction ci-dessous ne fonctionne pas toute seule. Vous devez trouver l'élément à modifier 
#pour pouvoir tester ce que vous voulez.
#EXERCICE ----
adonis2(dist.bray ~ beta_tab$, beta_tab, permutations = 999, method = "bray")

#Bon, vous pouvez remarquer que je vous ai volontairement induit en erreur. 
#Vous connaissaez les différences existantes entre les microbiotes de personne obèse et de personnes non-obèses. 
#La claire séparation était sensé vous attirer dans ce piège.
#De plus, le fait quèe je vous ai fait utiliser la fonction 
head(metadata)
#Mais pas 
tail(metadata) #Pour voir les dernières lignes
#Ou encore
View(metadata) #pour voir le tableau en entier.

#Faites toujours le tour de vos données avant de vous lancer tête baissée dans de l'interprétation.

##II-A-5 Heatmap ----
#La fonction suivante permet de sous-sélectionner seulement les 50 entités les plus abondantes
data_heatmap <- prune_taxa(names(sort(taxa_sums(enterotype),TRUE)[1:50]), enterotype)

#Si vous voulez essayer de réduire la taille du jeu de donnée, vous pouvez sélectionner un projet, 
#une méthode de séquençage ou tout autre paramètre avec la fonction suivante
#Remplacez PARAMETER par le nom de paramètrre qui vous intéresse
colnames(metadata) #Voila les paramètres parmis lesquels vous avez le choix
#Remplacez "VALUE" par l'élément discriminant de vous sous échantillonage.
#data_heatmap <- prune_taxa(names(sort(taxa_sums(subset_samples(enterotype, PARAMETER == "VALUE")),TRUE)[1:50]), enterotype)

#Ici on calcule la matrice de distance entre OTUs, ce n'est pas la même chose que toput à l'heure
OTUdf.dist <- vegdist(as.data.frame(data_heatmap@otu_table), method = "bray") 
row.clus <- hclust(OTUdf.dist, "aver") #On trace l'arbre de clusterisation pour le mettre dans la heatmap

#La on calcule les distances entre échantillons, ciomme précédamment
OTUdf.dist.g <- vegdist(as.data.frame(t(data_heatmap@otu_table)), method = "bray")
col.clus <- hclust(OTUdf.dist.g, "aver") #On trace l'arbre de clusterisation pour le mettre dans la heatmap

#La fonction suivante trace la heatmap
heatmap(as.matrix(data_heatmap@otu_table), Rowv = as.dendrogram(row.clus), Colv = as.dendrogram(col.clus), 
        margins = c(13, 11), trace = "none", density.info = "none", 
        xlab = "OTUs", ylab = "Samples", main = "Hierarchical clustering of Total abundance heatmap - Bray-Curtis Matrix", lhei = c(2, 8),
        col = rev(brewer.pal(9,"YlGnBu"))) 
#Essayez de jouer avec les paramètres et de comprendre ce script, les heatmaps sont assez populaires 
#même si elles sont souvent peu informatives


##II-A-6 Composition ----

data_compo <- subset_samples(enterotype, SeqTech == "Sanger")

data_melted <- psmelt(data_compo)

data_melted <- data_melted[is.na(data_melted$Abundance)==FALSE,]

data_melted[data_melted$Abundance<0.04,13] <- "Z_Others"
data_melted[is.na(data_melted$Genus) == TRUE ,13] <- "Z_Others"
data_melted <- data_melted[is.na(data_melted$Nationality) == FALSE,]



plot_col <- c("#33A02C","#FB9A99","#E31A1C","#FDBF6F","green",
                       "#CAB2D6","#6A3D9A","#E41A1C","#377EB8","#4DAF4A","#984EA3",
                       "#FF7F00","#FFFF33","#A65628","#F781BF","#999999","#A6CEE3","#1F78B4","#B2DF8A","#E31A1C", "darkgrey", "grey")
                       

ggplot(data_melted, aes(x = Sample, y = Abundance, fill = Genus)) +
  #facet_grid(rows = vars(factor(data_melted$Enterotype))) + #Si vous voulez séparer par un parametre, il suffit de retirer le dièse de début de ligne
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = plot_col) +
  ylab("Relative Abundance") +
  scale_y_continuous(expand = c(0,0)) + #remove the space below the 0 of the y axis in the graph
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1), #Allows to correctly display names
        axis.ticks.x = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank()) +  #remove minor-grid labels
  labs(title = "Microbiome composition - SANGER sequencing",
       #subtitle = "",
       caption = date()) 
dev.off() #Use dev.off() after a big graph to clean your graph space and avoid troubles

#On peut aussi moyenner les microbiotes par type d'enterotype

data_compo <- subset_samples(enterotype, SeqTech == "Sanger")
data_compo <- subset_samples(data_compo, is.na(Enterotype) == FALSE)
data_compo <- merge_samples(data_compo, "ClinicalStatus")
data_compo <- transform_sample_counts(data_compo, function(x) x / sum(x) )

data_melted <- psmelt(data_compo)

data_melted <- data_melted[is.na(data_melted$Abundance)==FALSE,]

data_melted[data_melted$Abundance<0.01,13] <- "Z_Others"
data_melted[is.na(data_melted$Genus) == TRUE ,13] <- "Z_Others"
#data_melted <- data_melted[is.na(data_melted$Nationality) == FALSE,]

ggplot(data_melted, aes(x = Sample, y = Abundance, fill = Genus)) +
  #facet_grid(rows = vars(factor(data_melted$Enterotype))) + #Si vous voulez séparer par un parametre, il suffit de retirer le dièse de début de ligne
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = plot_col) +
  ylab("Relative Abundance") +
  scale_y_continuous(expand = c(0,0)) + #remove the space below the 0 of the y axis in the graph
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1), #Allows to correctly display names
        axis.ticks.x = element_blank(),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank()) +  #remove minor-grid labels
  labs(title = "Microbiome composition grouped by clinical status - SANGER sequencing",
       #subtitle = "",
       caption = date()) 


#III-Reproduction ----
##III-A-1 A VOTRE TOUR----
#Dans cet exercice, vous travaillez avec un véritable jeu de données de recherche.
#Il est assez propre donc vous pouvez y aller.

#Le but de cet exercice est d'essayer de trouver les variables qui expliquent la séparation des communautés dans l'espace.
#Pour ce faire, réutilisez les fonctions qui sont écrites plus haut. 
#Essayer de générer au moins un graphique à partir de ce véritable jeu de données.
#Après vous êtes tranquille promis.
#Faites varier les paramètres d'affichage graphique (comme les couleurs de vos points) pour essayer de trouver des relations.

load("./Mock_data.rdata")

metadata <- data.frame(data@sam_data)

sample_sums(data) #Vous devrez probablement normaliser ces données avant de pouvoir les travailler correctement


