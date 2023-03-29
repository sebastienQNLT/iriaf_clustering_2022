library(dplyr)
library(FactoMineR)
library(factoextra)
library(cluster)
library(plotly)

#prepare_data
fifa_data <- read.csv2("./final_players21.csv",sep=';') %>% select(-X) 
fifa_for_pca<-fifa_data %>% select_if(is.numeric) %>%  scale() %>% as.data.frame

ncp.retenu=6
res.pca <- FactoMineR::PCA(fifa_for_pca,  graph = FALSE, ncp = ncp.retenu)
df_for_kmeans<-as.data.frame(res.pca$ind$coord)

#kmeans----
K=9
km.res <- kmeans(df_for_kmeans, centers = K, nstart = 25)
p<-fviz_cluster(km.res, data = df_for_kmeans,geom="point",alpha.ind=.8,addEllipses = TRUE)
ggplotly(p)

# Tracez un graphique des distances entre les clusters en fonction du nombre de clusters #elbow
fviz_nbclust(df_for_kmeans, kmeans, method = "wss", k.max = 10) + 
  labs(subtitle = "Elbow method")

# Tracez un graphique des distances entre les clusters en fonction du nombre de clusters  #silouhette
fviz_nbclust(df_for_kmeans, kmeans, method = "silhouette", k.max = 10) + labs(subtitle = "Silhouette method")

# Tracez un graphique des distances entre les clusters en fonction du nombre de clusters #gap_stat
fviz_nbclust(df_for_kmeans, kmeans, method = "gap_stat", k.max = 10) + labs(subtitle = "Gap stat method")


# kmeans ----
k.retenu=9
cluster.2 <- kmeans(df_for_kmeans, k.retenu, nstart = 50)
cluster.2$size
# graph des individus, 
p<-fviz_cluster(cluster.2, data = df_for_kmeans,geom="point",axes = c(1, 2),alpha.ind=.8,addEllipses = TRUE)
ggplotly(p)

s<-silhouette(cluster.2$cluster, dist(df_for_kmeans))
fviz_silhouette(s)


# ajout du résultat du clustering à la base initiale
# calcul des moyennes des variables et non pas des coordonnées sur les axes de l'ACP
player.clustered <-fifa_for_pca %>%
  mutate(cluster = cluster.2$cluster, Role=fifa_data$Role)

stats<-player.clustered %>% 
  group_by(cluster) %>%
  summarise_all("mean")

# affichage sous forme de heatmap
stats.reshaped<-reshape2::melt(stats, id.vars=c("cluster"))
ggplot(data = stats.reshaped, aes(x = cluster, y =variable, fill = value)) +
  scale_x_continuous(breaks = seq(1, 8, by = 1)) +
  geom_tile() +
  coord_equal() +
  scale_fill_gradientn(colours = heat.colors(100))+
  theme_classic()


# verification de notre clustering avec la position reelle des joueurs
player.clustered %>% group_by(Role,cluster) %>% count %>% View

p <-  ggplot(player.clustered, aes(Role))  + geom_bar() +  facet_grid(. ~ cluster)
ggplotly(p)


p <-  ggplot(player.clustered, aes(cluster))  + geom_bar() +  facet_grid(. ~ Role)
ggplotly(p)
