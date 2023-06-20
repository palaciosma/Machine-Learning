######################################################################
#
#   CS5706
#   Lab 05 - Cluster Analysis in R
#   Exercise: Hierchical Clustering and k-means
#   Alessandro Pandini
#
######################################################################
# 1. load data and summary statistics
### 1.1 load the data from the customers_data.csv file and inspect it
###   note: the dataset contains annual spending in monetary units (m.u.)
###     on diverse product categories

df <- read.csv("customers_data.csv")

### 1.2 get a summary report of the variables

summary(df)

### 1.3 remove customers that are outliers
###   according to their individual spending on Fresh, Milk and Groceries

library(ggplot2)

ggplot(df) +
  geom_boxplot(aes(y=Fresh))
ggplot(df) +
  geom_boxplot(aes(y=Milk))
ggplot(df) +
  geom_boxplot(aes(y=Grocery))


outliers_fresh <- boxplot.stats(df$Fresh)$out
outliers_fresh_rows <- which(df$Fresh %in% c(outliers_fresh))
outliers_fresh_rows

outliers_milk <- boxplot.stats(df$Milk)$out
outliers_milk_rows <- which(df$Milk %in% c(outliers_milk))
outliers_milk_rows

outliers_grocery <- boxplot.stats(df$Grocery)$out
outliers_grocery_rows <- which(df$Grocery %in% c(outliers_grocery))
outliers_grocery_rows

df_clean <- df[-c(outliers_fresh_rows), ]
df_clean <- df_clean[-c(outliers_milk_rows), ]
df_clean <- df_clean[-c(outliers_grocery_rows), ]



######################################################################
# 2. cluster analysis
### 2.1 hierarchical clustering - complete linkage
###   note: exclude the Region attribute

dist_df <- dist(df_clean[,-1], method = 'euclidian')
#   then apply complete linkage
hc_df <- hclust(dist_df, method = 'complete')
hc_df

### 2.2 plot the associated dendrogram
plot(hc_df, hang = -0.1, labels = df_clean$Region)


### 2.3 select a partition containing 3 groups
hc_df_k3 <- cutree(hc_df, k = 3)


### 2.4 k-means with 3 groups
k_df <- kmeans(df_clean[,-1], 3)
k_df

k_cluster_id <- k_df$cluster


######################################################################
# 3. Evaluation of cluster results
### 3.1 silhoutte score
sil_hc <- cluster::silhouette(hc_df_k3, dist_df)
sil_k <- cluster::silhouette(k_cluster_id, dist_df)


### 3.2 silhoutte plots
###   note: use border = 'grey' to be able to see the plot lines

opar <- par(no.readonly = TRUE)
par(mfrow = c(2,1))
plot(sil_hc, border = "grey")
plot(sil_k, border = "grey")
par(opar)




### 3.3 get the attributes averages per cluster
###   for the best clustering result (according to the silhoutte plots)
###   and join the results in a data frame

cluster1 <- df_clean[c(k_df$cluster == 1),]
cluster2 <- df_clean[c(k_df$cluster == 2),]
cluster3 <- df_clean[c(k_df$cluster == 3),]

summary(cluster1)



######################################################################
# 4. My approach to number of cluster selection
#install.packages("factoextra")

library(GGally)
library(factoextra)
library(cluster)

k_df1 <- kmeans(df_clean[,-1], 3, nstart = 25)
#25 different random starts to avoid missplacements

sil <- silhouette(k_df1$cluster, dist_df)
fviz_silhouette(sil)
#As seen, there are negative values for the second cluster, which might indicate that its been missclasified. The clusters are all above the avg coefficient line, 
# which indicates that 3 might be the optimal number of clusters.

#Now we can try with 2 and 4 clusters
k_df2 <- kmeans(df_clean[,-1], 2, nstart = 25)
sil2 <- silhouette(k_df2$cluster, dist_df)
fviz_silhouette(sil2)
#As seen, 2 does not seem to be a good partition

k_df4 <- kmeans(df_clean[,-1], 4, nstart = 25)
sil4 <- silhouette(k_df4$cluster, dist_df)
fviz_silhouette(sil4)

k_df5 <- kmeans(df_clean[,-1], 5, nstart = 25)
sil5 <- silhouette(k_df5$cluster, dist_df)
fviz_silhouette(sil5)

#Neither do 4 or 5. Therefore, we can state that 3 is the optimal number of clusters.

