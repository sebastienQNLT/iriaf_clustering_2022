library(dplyr)
library(plotly)
library(ggplot2)
library(skimr)
library(FactoMineR)
library(factoextra)
library(plotly)

#prepare_data
fifa_data <- read.csv2("./final_players21.csv",sep=';') %>% select(-X)

#EDA ---- 
fifa_data %>% glimpse
fifa_data %>% skim

fifa_data %>%
  ggplot( aes(x=overall)) +
  geom_density(alpha=0.8)

#dataviz----
p<-fifa_data %>%
  ggplot( aes(x=overall)) +
  geom_density(aes(color=Role), alpha=0.8)
ggplotly(p)


fifa_num<-fifa_data %>% select_if(is.numeric)

library(caret)
nearZeroVar(fifa_num,names=TRUE)

library(corrplot)
M<-cor(fifa_num)
findCorrelation(M,names=TRUE)
corrplot(M, method="color")

corrplot(M, type="upper", order="hclust", col=c("black", "white"),
         bg="lightblue")

#nombre de joueurs par role
fifa_data %>% group_by(Role) %>% count

#scaling des donnees----
#on conserve que les données numériques
fifa_data.scaled<-fifa_num %>%  scale() %>% as.data.frame
fifa_data.scaled %>% glimpse
fifa_data.scaled %>% skim

#ACP----
res.pca <- FactoMineR::PCA(fifa_data.scaled,  graph = FALSE, ncp=10)
eig.val <- res.pca$eig
eig.val

#scree plot
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 70))
#on conserve 5 ou 6 axes // au lieu de 41 dimensions


# graph des contributions des  variables
# Contributions of variables to PCx
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 3, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 4, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 5, top = 10)

fviz_contrib(res.pca, choice = "var", axes = 6, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 7, top = 10)

#visualisation des axes 1 et 2
fviz_pca_var(res.pca, col.var = "contrib", 
             ggtheme = theme_minimal(),axes=c(1,3),
             select.var = list(contrib = 15)
)

#graph des individus
#axe 1 et 2
fviz_pca_ind(res.pca, axes = c(1, 2),geom = "point",alpha.ind=.1) +theme_minimal()
p<-fviz_pca_ind(res.pca, axes = c(1, 2),label = "none",habillage=as.factor(fifa_data$Role)) +theme_minimal()
ggplotly(p)
fviz_pca_ind(res.pca, axes = c(1, 3),label = "none",habillage=as.factor(fifa_data$Role),addEllipses = TRUE) +theme_minimal()
fviz_pca_ind(res.pca, axes = c(2, 3),label = "none",habillage=as.factor(fifa_data$Role),addEllipses = TRUE) +theme_minimal()
