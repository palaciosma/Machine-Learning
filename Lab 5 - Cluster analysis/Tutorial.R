######################################################################
#
#   CS5706
#   Lab 05 - Cluster Analysis in R
#   Tutorial
#   Alessandro Pandini
#
######################################################################
#
#   1. Data preparation
#   2. Cluster analysis (agglomerative hierarchical and k-means clustering)
#   3. Evaluation of cluster results
#
######################################################################
# 1. Data preparation

# load the data from the protein.csv file and inspect it
#   note: it reports daily g/person protein intake per country (before 1989)
protein <- read.csv("protein.csv")
str(protein)

# get a summary report
summary(protein)

######################################################################
# 2. Cluster analysis (agglomerative hierarchical and k-means)

# hierarchical clustering
#   first generate the distance matrix with euclidian distance
#     note: exclude the country attribute
dist_protein <- dist(protein[,-1], method = 'euclidian')
#   then apply complete linkage
hc_protein <- hclust(dist_protein, method = 'complete')
hc_protein

# plot the associated dendrogram
plot(hc_protein, hang = -0.1, labels = protein$Country)

# 'cut' the dendrogram to select one partition with 5 groups
#   note: the cutree command requires a distance cutoff h
#      or the number k of desired groups
hc_cluster_id_protein <- cutree(hc_protein, k = 5)

# k-means
#   note: select k = 5 groups
k_protein = kmeans(protein[,-1], 5)
k_protein

# get the cluster id from the kmeans object
k_cluster_id_protein <- k_protein$cluster

######################################################################
# 3. Evaluation of cluster results

# silhoutte plot
# first install the package cluster
if(require(cluster) == FALSE){
    install.packages('cluster')
}
# then calculate the silhoutte score for the two cluster solutions
#   note: look at the help for silhoutte to understand the required input
sil_hc_protein <- cluster::silhouette(hc_cluster_id_protein, dist_protein)
sil_k_protein <- cluster::silhouette(k_cluster_id_protein, dist_protein)

# plot the results of the silhoutte analysis for the two cluster solutions
opar <- par(no.readonly = TRUE)
par(mfrow = c(2,1))
plot(sil_hc_protein)
plot(sil_k_protein)
par(opar)

