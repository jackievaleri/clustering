# Purpose: try out PCA then kmeans 

# do pca
df = data.frame(as.matrix(DRUID::cmap_druid$tfidf))
pca = prcomp(df, scale = TRUE)

# install some stuff for visualization
install.packages("factoextra")
library(factoextra)

# visualize percentage of variance explained by each eigenvalue
fviz_eig(pca)