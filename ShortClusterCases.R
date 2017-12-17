url = 'http://www.biz.uiowa.edu/faculty/jledolter/DataMining/protein.csv'
food <- read.csv(url)
head(food)
library(dplyr)

# Distribution of Red Meat Demand per Countries
food_RedMeat_10=arrange(food, desc(RedMeat))
food_RedMeat_10=head(food_RedMeat_10,10)

ggplot(aes(x=Country, y=RedMeat, fill=Country), data= food_RedMeat_10)+
  geom_bar(stat='identity')+
  scale_fill_brewer()+
  theme_gray()

# Distribution of White Meat Demand per Countries
food_WhiteMeat_10=arrange(food, desc(WhiteMeat))
food_WhiteMeat_10=head(food,10)

ggplot(aes(x=Country, y=WhiteMeat, fill=Country), data= food_WhiteMeat_10)+
  geom_bar(stat='identity')+
  scale_fill_brewer()+
  theme_gray()


# KMEANS method

set.seed(2) ## to fix the random starting clusters
grpMeat <- kmeans(food[,c("WhiteMeat","RedMeat")], centers=3, nstart=10)
grpMeat

## list of cluster assignments
o=order(grpMeat$cluster)
data.frame(food$Country[o],grpMeat$cluster[o])

# Plot cluster
plot(food$Red, food$White, type="n", xlim=c(3,19), xlab="Red Meat", ylab="White Meat")
text(x=food$Red, y=food$White, labels=food$Country,col=grpMeat$cluster+1)
title('3 Clusters RedMeat VS WhiteMeat')

# Conclude that the UK and France consume the largest amounts of red meat
# Whereas Austria and the Netherlands consume the largest amounts of white meat.
# Albania almost doesnt consume any white meat at all


## same analysis, but now with clustering on all
## protein groups change the number of clusters to 7
set.seed(2)
grpProtein <- kmeans(food[,-1], centers=7, nstart=10)
o=order(grpProtein$cluster)
data.frame(food$Country[o],grpProtein$cluster[o])


library(cluster)
clusplot(food[,-1], grpProtein$cluster, 
         main='2D representation of the Cluster solution', 
         color=TRUE, shade=TRUE, labels=2, lines=0)


#### Dendogram


foodagg=agnes(food,diss=FALSE,metric="euclidian")
plot(foodagg, main='Dendrogram') 


groups <- cutree(foodagg, k=4) # cut tree into 3 clusters
rect.hclust(foodagg, k=4, border="red") 









###########
# Good cluster is one which contains the smallest possible within-cluster 
# variation of all observations in relation to each other. 
# Another clustering  example

# If more than two variables in data, one option would be to perform 
# Principal Component Analysis (PCA) and then
# plot the first two vectors 
# Standardize the data
# Whether the number of clusters obtained truly represent the underlying pattern found in data 

library(datasets)
str(attitude)
summary(attitude)


# Subset the attitude data
dat = attitude[,c(3,4)]

# Plot subset data
plot(dat, main = "% of favourable responses to
     Learning and Privilege", pch =20, cex =2)


# Perform k means cluster with 2 clusters 

set.seed(7)
kml=kmeans(dat,4,nstart=50)
plot(dat,col=(kml$cluster+1),main="K-Menas result with 2 clusters",pch=10,cex=2)


# Check for the optimal number of clusters given the data
mydata <- dat
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)

# K means with 6 clusters

set.seed(7)
km2=kmeans(dat,6,nstart=100)


# Plot results
plot(dat, col =(km2$cluster +1) , main="K-Means result with 6 clusters", pch=20, cex=2)

