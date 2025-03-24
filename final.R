###MA347 Final Project
library(dplyr)
library(tidyr)   
library(forcats) 
library(rpart)
library(rpart.plot)
library(mosaic)
library(cluster)
library(ggplot2)

dat_new<-read.csv("eulapse_new.csv")
dat_new<-subset(dat_new, select =-X)
chars <- names(dat)[sapply(dat_new, is.character)]
quant.df <- subset(dat_new, select = !names(dat_new) %in% chars)

#Encode Categorical
data_encoded <- dat %>%
  mutate_at(vars(chars), as.factor) %>%
  mutate_at(vars(chars), as.integer)

str(data_encoded)
write.csv(data_encoded, file="eulapse_final.csv")

#Cluster Analysis
str(quant.df)
scaled.df <- scale(quant.df)
d <- dist(scaled.df, method = "euclidean")

methods <- c("single", "complete", "average", "centroid", "ward.D")

cluster_single <- hclust(d, method = "single")
#plot(cluster_single, main = paste("Dendrogram (Method:", "Single", ")"))

cluster_complete <- hclust(d, method = "complete")
#plot(cluster_complete, main = paste("Dendrogram (Method:", "Complete", ")"))

cluster_average <- hclust(d, method = "average")
#plot(cluster_average, main = paste("Dendrogram (Method:", "Average", ")"))

cluster_centroid <- hclust(d, method = "centroid")
#plot(cluster_centroid, main = paste("Dendrogram (Method:", "Centroid", ")"))

cluster_ward <- hclust(d, method = "ward.D")
#plot(cluster_ward, main = paste("Dendrogram (Method:", "Ward's", ")"))

plot(cluster_ward,hang=-1, ann = FALSE)
rect.hclust(cluster_ward, k=7)

#Test different methods, Ward's was best
memb <- cutree(cluster_single, k = 10)
table(memb)
memb <- cutree(cluster_complete, k = 10)
table(memb)
memb <- cutree(cluster_average, k = 10)
table(memb)
memb <- cutree(cluster_centroid, k = 10)
table(memb)
memb <- cutree(cluster_ward, k = 7)
table(memb)


cl_member<-factor(memb)
par(mar = c(4,4,4,4))
clusmeans<-aggregate(.~memb,data=quant.df,FUN=mean)
clusmeans
clusmed<-aggregate(.~memb,data=quant.df,FUN=median)
clusmed
clusSD<-aggregate(.~memb,data=quant.df,FUN=sd)
clusSD
boxplot(dat_new$polholder_age ~cl_member,main="Age Across Clusters")
boxplot(dat_new$vehicl_agepurchase ~cl_member,main="Vehicle Age at Purchase Across Clusters")
boxplot(dat_new$vehicl_age ~cl_member,main="Vehicle Age Across Clusters")
boxplot(dat_new$lapse ~cl_member,main="Lapse Across Clusters")
boxplot(dat_new$prem_final~cl_member,main="Premium Across Clusters")

clusters <- cutree(cluster_ward, k = 7)
clustered_data <- cbind(dat_new, Cluster = clusters)

histogram(~clusters | policy_caruse, data=clustered_data, main="Vehicle Use by Cluster")
histogram(~clusters | lapse==1, data=clustered_data, main="Lapse Count by Cluster")
histogram(~clusters | vehicl_garage, data=clustered_data, main="Garage by Cluster")
histogram(~clusters | prem_pure, data=clustered_data, main="Garage by Cluster")
