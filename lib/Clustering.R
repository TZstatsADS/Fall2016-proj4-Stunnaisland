install.packages("cluster")
library(cluster)

setwd("C:/Users/Zachary/Desktop/ADS - Proj.4")
load("C:/Users/Zachary/Desktop/ADS - Proj.4/lyr.RData")

common_id = read.table("common_id.txt")
View(common_id)
msm_train = read.table("mxm_dataset_train.txt",header=F, sep=",")

#reading the bag of words into R
head(lyr)
View(lyr)

lyr_2 = na.omit(lyr)
scale(lyr_2)

k_means = kmeans(lyr_2,3)
summary(k_means)

#get cluster means 
aggregate(lyr_2,by=list(fit$cluster),FUN=mean)
lyr_3 = data.frame(lyr_2, fit$cluster)


########
##Hierarchical Algothrims 
distance = dist(lyr_2, method = "euclidean") # distance matrix
fit = hclust(distance, method="ward") 
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red")

