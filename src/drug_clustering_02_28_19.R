## https://www.datacamp.com/community/tutorials/k-means-clustering-r

df = data.frame(as.matrix(DRUID::cmap_druid$tfidf))

# adding a comment to test out github

# print(df) # nah homeboy too long
#write.csv(df, file = 'tfidf')

library(dplyr)
summary(df)

set.seed(20)

kmeansCluster <- function(numClusters, save=TRUE) {
  clusters <- kmeans(df, numClusters)
  str(clusters)
  
  df$drugClass <- as.factor(clusters$cluster)
  labels <- data.frame(cbind(row.names(df),df$drugClass))
  
  print(labels)
  if(save) {
    path = '/Users/jackie16201/Desktop/Lab/clustering/out/'
    filename = paste(path, 'drug_labels_', numClusters, '_clusters_non_size_norm', sep="")
    write.csv(labels, file = filename)
  }
}


# non-size normalized
numClusters = 5
kmeansCluster(numClusters)

numClusters = 10
kmeansCluster(numClusters)

numClusters = 50
kmeansCluster(numClusters)

numClusters = 100
kmeansCluster(numClusters)

numClusters = 300
kmeansCluster(numClusters)


# uh oh- too many in one cluster- need to find a workaround

# debug 1: check max of data frame to see if I need to renormalize it

row_maxes= row.names(df)
for (row in 1:length(row.names(df))) {
  curr_max = max(as.numeric(df[row,]), na.rm=TRUE)
  print(curr_max)
  row_maxes[row] = curr_max
}

print(max(as.numeric(row_maxes)))
# answer is 0.2088- this indicates that we're dealing with pretty small numbers

print(min(as.numeric(row_maxes)))
# answer is  7.41168e-05- but not zero, good

print(mean(as.numeric(row_maxes)))
# answer is 0.02266- this is really small, drug expression maxes out on avg at 0.0226!

col_maxes= colnames(df)
for (col in 1:(length(names(df))-1)) {
  curr_max = max(df[,col], na.rm=TRUE)
  print(curr_max)
  col_maxes[col] = curr_max
}

print(min(col_maxes))
# answer is "0.00010065883173671"- but not zero- good

# conclusion- I think this is fine- but- lots and lots of zeros


# debug 2: can we check how many drugs have >90% zeros?
drug_zero_percent= row.names(df)
num_cols = length(names(df))
for (row in 1:length(row.names(df))) {
  curr_num_zeros = sum(df[row,] == 0)
  print(curr_num_zeros)
  drug_zero_percent[row] = curr_num_zeros / num_cols
}
print(drug_zero_percent)

# can we check how many drugs have >90% zeros?
gene_zero_percent= colnames(df)
num_rows = length(row.names(df))
for (col in 1:length(colnames(df))) {
  curr_num_zeros = sum(df[,col] == 0)
  print(curr_num_zeros)
  gene_zero_percent[col] = curr_num_zeros / num_rows
}
print(gene_zero_percent)

# debug 3: try test set
test_path = '/Users/jackie16201/Desktop/Lab/clustering/data/'
test_set <- read.csv(file=paste(test_path, 'test_drugs', sep=''), header=FALSE, sep=",")
print(test_set)

# read in any k means clustering result and check against test set
checkTestSet <- function(kmeans_path, test_set) {
  kmeans_test = read.csv(kmeans_path, header=TRUE, sep=",")
  
  for (row_test in 1:nrow(test_set)) {
    test_drug = test_set[row_test, "V2"]
    test_class = test_set[row_test, "V1"]
    for (row in 1:nrow(kmeans_test)) {
      drug <- kmeans_test[row, "X1"]
      cluster  <- kmeans_test[row, "X2"]
    
      if(test_drug == drug) {
        print(paste(drug, 'was in cluster', cluster, 'in test set but actually class', test_class))
      }
    }
  }
}

# Use the test function now!
kmeans_5_path = paste('/Users/jackie16201/Desktop/Lab/clustering/out/drug_labels_5_clusters_non_size_norm')
checkTestSet(kmeans_5_path, test_set)

kmeans_10_path = paste('/Users/jackie16201/Desktop/Lab/clustering/out/drug_labels_10_clusters_non_size_norm')
checkTestSet(kmeans_10_path, test_set)

kmeans_50_path = paste('/Users/jackie16201/Desktop/Lab/clustering/out/drug_labels_50_clusters_non_size_norm')
checkTestSet(kmeans_50_path, test_set)

kmeans_100_path = paste('/Users/jackie16201/Desktop/Lab/clustering/out/drug_labels_100_clusters_non_size_norm')
checkTestSet(kmeans_100_path, test_set)

kmeans_300_path = paste('/Users/jackie16201/Desktop/Lab/clustering/out/drug_labels_300_clusters_non_size_norm')
checkTestSet(kmeans_300_path, test_set)