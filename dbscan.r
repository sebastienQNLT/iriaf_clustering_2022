install.packages("dbscan")
library(dbscan)
library(cluster)

#CAH
#prepare_data
fifa_data <- read.csv2("./final_players21.csv",sep=';') %>% select(-X) 
fifa_for_pca<-fifa_data %>% select_if(is.numeric) %>%  scale() %>% as.data.frame

ncp.retenu=6
res.pca <- FactoMineR::PCA(fifa_for_pca,  graph = FALSE, ncp = ncp.retenu)
df_for_dbscan<-as.data.frame(res.pca$ind$coord) 
d<-dist(df_for_dbscan)

dbscan_result <- dbscan(df_for_dbscan, eps = .9, minPts  = 10)
s<-silhouette(dbscan_result$cluster, d)
fviz_silhouette(s)



