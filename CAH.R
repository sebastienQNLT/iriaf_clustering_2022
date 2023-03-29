library(dplyr)
library(FactoMineR)
library(factoextra)

#CAH
#prepare_data
fifa_data <- read.csv2("./final_players21.csv",sep=';') %>% select(-X) %>% sample_frac(.5)
fifa_for_pca<-fifa_data %>% select_if(is.numeric) %>%  scale() %>% as.data.frame

ncp.retenu=6
res.pca <- FactoMineR::PCA(fifa_for_pca,  graph = FALSE, ncp = ncp.retenu)
df_for_CAH<-as.data.frame(res.pca$ind$coord) 

# Calculez une matrice de distances entre les observations
dist_matrix <- dist(df_for_CAH)

# Effectuez la CAH en utilisant la méthode "ward.D"
res.cah <- hclust(dist_matrix, method = "ward.D")
rm(dist_matrix)
rm(fifa_for_pca)
rm(res.pca)
gc()


#choix du nombre de cluster ----

# Tracez un graphique des distances entre les clusters en fonction du nombre de clusters
fviz_nbclust(df_for_CAH, hcut, method = "wss") + 
  labs(subtitle = "Elbow method")

# Tracez un graphique des distances entre les clusters en fonction du nombre de clusters
fviz_nbclust(df_for_CAH, hcut, method = "silhouette") + labs(subtitle = "Silhouette method")

# Tracez un graphique des distances entre les clusters en fonction du nombre de clusters
fviz_nbclust(df_for_CAH, hcut, method = "gap_stat") + labs(subtitle = "Gap stat method")

hc.cut <- hcut(df_for_CAH, k = 8, hc_method = "ward.D")


# Visualize dendrogram
fviz_dend(hc.cut, show_labels = FALSE, rect = TRUE,h=5)
# Visualize cluster
fviz_cluster(hc.cut, ellipse.type = "convex")

s<-silhouette(hc.cut$cluster, dist(df_for_CAH))
fviz_silhouette(s)



# ajout du résultat du clustering à la base initiale
# calcul des moyennes des variables et non pas des coordonnées sur les axes de l'ACP
player.clustered <-fifa_for_pca %>%
  mutate(cluster = hc.cut$cluster,Role=fifa_data$Role)

stats<-player.clustered %>% 
  group_by(cluster) %>%
  summarise_all("mean")

stats.reshaped<-reshape2::melt(stats.scaled, id.vars=c("stats$cluster"))
ggplot(data = stats.reshaped, aes(x = `stats$cluster`, y =variable, fill = value)) +
  scale_x_continuous(breaks = seq(1, 8, by = 1)) +
  geom_tile() +
  coord_equal() +
  scale_fill_gradientn(colours = topo.colors(100))+
  theme_classic()

# verification de notre clustering avec la position reelle des joueurs
player.clustered %>% group_by(Role,cluster) %>% count

p <-  ggplot(player.clustered, aes(Role))  + geom_bar() +  facet_grid(. ~ cluster)
ggplotly(p)


p <-  ggplot(player.clustered, aes(cluster))  + geom_bar() +  facet_grid(. ~ Role)
ggplotly(p)

