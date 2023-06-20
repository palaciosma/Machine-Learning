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
customers <- read.csv("customers_data.csv")
str(customers)

### 1.2 get a summary report of the variables
summary(customers)

### 1.3 remove customers that are outliers
###   according to their individual spending on Fresh, Milk and Groceries
customers_boxplot <- boxplot(customers$Fresh, plot = F)
Fresh_threshold <- min(customers_boxplot$out)
customers_boxplot <- boxplot(customers$Milk, plot = F)
Milk_threshold <- min(customers_boxplot$out)
customers_boxplot <- boxplot(customers$Grocery, plot = F)
Grocery_threshold <- min(customers_boxplot$out)
spending_filter <- customers$Fresh < Fresh_threshold & customers$Milk < Milk_threshold & customers$Grocery < Grocery_threshold
customers_clean <- customers[spending_filter,]

######################################################################
# 2. cluster analysis
### 2.1 hierarchical clustering - complete linkage
###   note: exclude the Region attribute
dist_customers_clean <- dist(customers_clean[,-1], method = 'euclidian')
hc_customers_clean <- hclust(dist_customers_clean, method = 'complete')

### 2.2 plot the associated dendrogram
plot(hc_customers_clean, hang = -0.1)

### 2.3 select a partition containing 3 groups
hc_cluster_id_customers_clean <- cutree(hc_customers_clean, k = 3)

### 2.4 k-means with 3 groups
k_customers_clean = kmeans(customers_clean[,-1], 3)
k_cluster_id_customers_clean <- k_customers_clean$cluster

######################################################################
# 3. Evaluation of cluster results
### 3.1 silhoutte score
sil_hc_customers_clean <- cluster::silhouette(hc_cluster_id_customers_clean, dist_customers_clean)
sil_k_customers_clean <- cluster::silhouette(k_cluster_id_customers_clean, dist_customers_clean)

### 3.2 silhoutte plots
###   note: use border = 'grey' to be able to see the plot lines
opar <- par(no.readonly = TRUE)
par(mfrow = c(2,1))
plot(sil_hc_customers_clean, border = 'grey')
plot(sil_k_customers_clean, border = 'grey')
par(opar)

### 3.3 get the attributes averages per cluster
###   for the best clustering result (according to the silhoutte plots)
###   and join the results in a data frame
customers_cl_1 <- apply(customers_clean[k_cluster_id_customers_clean == 1,-1], 2, mean)
customers_cl_2 <- apply(customers_clean[k_cluster_id_customers_clean == 2,-1], 2, mean)
customers_cl_3 <- apply(customers_clean[k_cluster_id_customers_clean == 3,-1], 2, mean)
customers_cluster_averages <- rbind(customers_cl_1, customers_cl_2, customers_cl_3)
customers_cluster_averages

