## -----------------------------------------------------------------------------
library(corrplot)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(gridExtra)
library(stats)


## -----------------------------------------------------------------------------
data <- read.csv('FordKa_demographic.csv', header = TRUE)
dim(data)
names(data)
head(data)
str(data)
summary(data)


## -----------------------------------------------------------------------------
#condense table to only columns 2 to 10 by removing the "respondent" column
df <- data[,2:10]
head(df)


## -----------------------------------------------------------------------------
#get_dist: for computing a distance matrix between the rows of a data matrix. The default distance computed is the Euclidean
distance <- get_dist(df, method = "euclidean")
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


## -----------------------------------------------------------------------------
k<-2
k2 <- kmeans(df, centers = k, nstart=10, iter.max=10)
k2
k2$size
names(k2)


## -----------------------------------------------------------------------------
#plot with number labels
fviz_cluster(k2, data = df)
#plot without number labels
fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")


## -----------------------------------------------------------------------------
df %>%
  mutate(Cluster = k2$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

df$Cluster<- k2$cluster
head(df)
str(df)
data
df

df[df$preference == 1,]#116 rows out of 250 = 46.6% ~ in top 3 (choosers)
df[df$preference == 2,]#72 rows out of 250 = 28.8% ~ in bottom 3 (non-choosers)
df[df$preference == 3,]#62 rows out of 250 = 24.8% ~ in middle 4 


## -----------------------------------------------------------------------------
#determine if preference 1 shows up more in cluster 1 or 2
mode.pref1 <- 0
ones = df[df$preference == 1 & df$Cluster ==1,0]
twos = df[df$preference == 1 & df$Cluster == 2,0]
threes = df[df$preference == 1 & df$Cluster == 3,0]
if(nrow(ones) > nrow(twos)){
  mode.pref1 <- 1
}else 
  mode.pref1 <-2
paste('Preference 1 shows up more in cluster:',mode.pref1)
nrow(ones)
nrow(twos)


#determine if preference 2 shows up more in cluster 1 or 2
mode.pref1 <- 0
ones = df[df$preference == 2 & df$Cluster ==1,0]
twos = df[df$preference == 2 & df$Cluster == 2,0]

if(nrow(ones) > nrow(twos)){
  mode.pref1 <- 1
}else 
  mode.pref1 <-2
paste('Preference 2 shows up more in cluster:',mode.pref1)
nrow(ones)
nrow(twos)


## -----------------------------------------------------------------------------
mode.pref1 <- 0
ones = df[df$preference == 1 & df$Cluster ==1,0]
twos = df[df$preference == 2 & df$Cluster == 1,0]
if(nrow(ones) > nrow(twos)){
  mode.pref1 <- 1
}else 
  mode.pref1 <-2
paste('Preference Mode is:',mode.pref1)

mode.gender1 <- 0
ones = df[df$Gender == 1 & df$Cluster ==1,0]
twos = df[df$Gender == 2 & df$Cluster == 1,0]
if(nrow(ones) > nrow(twos)){
  mode.gender1 <- 1
}else 
  mode.gender1 <-2
paste('Gender Mode is:',mode.gender1)

mode.fp1 <- 0
ones = df[df$FirstPurchase == 1 & df$Cluster ==1,0]
twos = df[df$FirstPurchase == 2 & df$Cluster == 1,0]
if(nrow(ones) > nrow(twos)){
  mode.fp1 <- 1
}else 
  mode.fp1 <-2
paste('First Purchases Mode is:',mode.fp1)


age.range <- range(df[df$Cluster ==1,]$Age)
sprintf("Age Range is: %s - %s", age.range[1],age.range[2])
MaritalStatus.range <- range(df[df$Cluster ==1,]$MaritalStatus)
sprintf("Marital Status Range is: %s - %s", MaritalStatus.range[1],MaritalStatus.range[2])
Children.range <- range(df[df$Cluster ==1,]$Children)
sprintf("Children Range is: %s - %s", Children.range[1],Children.range[2])
AgeCat.range <- range(df[df$Cluster ==1,]$AgeCat)
sprintf("Age Category Range is: %s - %s", AgeCat.range[1],AgeCat.range[2])
ChildCat.range <- range(df[df$Cluster ==1,]$ChildCat)
sprintf("Child Category Range is: %s - %s", ChildCat.range[1],ChildCat.range[2])
IncomeCat.range <- range(df[df$Cluster ==1,]$IncomeCat)
sprintf("Income Category Range is: %s - %s", IncomeCat.range[1],IncomeCat.range[2])


## -----------------------------------------------------------------------------
mode.pref2 <- 0
ones = df[df$preference == 1 & df$Cluster ==2,0]
twos = df[df$preference == 2 & df$Cluster == 2,0]
if(nrow(ones) > nrow(twos)){
  mode.pref2 <- 1
}else 
  mode.pref2 <- 2
paste('Preference Mode is:',mode.pref2)

mode.gender2 <- 0
ones = df[df$Gender == 1 & df$Cluster ==2,0]
twos = df[df$Gender == 2 & df$Cluster == 2,0]
if(nrow(ones) > nrow(twos)){
  mode.gender2 <- 1
}else 
  mode.gender2 <-2
paste('Gender Mode is:',mode.gender2)

mode.fp2 <- 0
ones = df[df$FirstPurchase == 1 & df$Cluster ==2,0]
twos = df[df$FirstPurchase == 2 & df$Cluster == 2,0]
if(nrow(ones) > nrow(twos)){
  mode.fp2 <- 1
}else 
  mode.fp2 <-2
paste('First Purchases Mode is:',mode.fp2)


age.range <- range(df[df$Cluster ==2,]$Age)
sprintf("Age Range is: %s - %s", age.range[1],age.range[2])
MaritalStatus.range <- range(df[df$Cluster ==2,]$MaritalStatus)
sprintf("Marital Status Range is: %s - %s", MaritalStatus.range[1],MaritalStatus.range[2])
Children.range <- range(df[df$Cluster ==2,]$Children)
sprintf("Children Range is: %s - %s", Children.range[1],Children.range[2])
AgeCat.range <- range(df[df$Cluster ==2,]$AgeCat)
sprintf("Age Category Range is: %s - %s", AgeCat.range[1],AgeCat.range[2])
ChildCat.range <- range(df[df$Cluster ==2,]$ChildCat)
sprintf("Child Category Range is: %s - %s", ChildCat.range[1],ChildCat.range[2])
IncomeCat.range <- range(df[df$Cluster ==2,]$IncomeCat)
sprintf("Income Category Range is: %s - %s", IncomeCat.range[1],IncomeCat.range[2])


## -----------------------------------------------------------------------------
d <- dist(df, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, hang = -1)

# Compute with agnes
hc2 <- agnes(df, method = "complete")
names(hc2)

# Agglomerative coefficient
#if close to 1 then its good
hc2$ac

# methods to assess
methods <- c( "average", "single", "complete", "ward")
names(methods) <- c( "average", "single", "complete", "ward.D1")

# function to compute coefficient
ac <- function(x) {
  agnes(df, method = x)$ac
}

map_dbl(methods, ac)

# choose the best one
hc3 <- agnes(df, method = "ward")
pltree(hc3, hang = -1, main = "Dendrogram of agnes") 

#In order to identify sub-groups (i.e. clusters), we can cut the dendrogram with cutree:
# Cut tree into 2 groups
sub_grp <- cutree(hc3, k = 2)
table(sub_grp)

#We can also use the cutree output to add the cluster each observation belongs to to our original data.
data %>%
  mutate(cluster = sub_grp) %>%
  head

#possible to draw the dendrogram with a border around the 2 clusters. 
#The argument border is used to specify the border colors for the rectangles:
rect.hclust(hc3, k = 2, border = 2:5)

#visualize the result in a scatter plot
fviz_cluster(list(data = df, cluster = sub_grp))

#Determining Optimal Clusters
?hcut
#1.Elbow Method:
fviz_nbclust(df, FUN = hcut, method = "wss")
fviz_nbclust
#2.Average Silhouette Method:
fviz_nbclust(df, FUN = hcut, method = "silhouette")


## -----------------------------------------------------------------------------
p.data <- read.csv('FordKa_psychographic.csv', header = TRUE)
dim(p.data)
names(p.data)
head(p.data)
str(p.data)
summary(p.data)


## -----------------------------------------------------------------------------
#Condense table to only columns 2 to 63  by removing the "respondent" column
df2 <- p.data[,2:63]
head(df2)


## -----------------------------------------------------------------------------
#get_dist: for computing a distance matrix between the rows of a data matrix. The default distance computed is the Euclidean
distance2 <- get_dist(df2, method = "euclidean")
fviz_dist(distance2, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


## -----------------------------------------------------------------------------
k.p<-2
k2.p <- kmeans(df2, centers = k.p, nstart=10, iter.max=10)
k2.p
k2.p$size
names(k2.p)


## -----------------------------------------------------------------------------
#plot with number labels
fviz_cluster(k2.p, data = df2)
#plot without number labels
fviz_cluster(k2.p, geom = "point", data = df2) + ggtitle("k = 2")


## -----------------------------------------------------------------------------
#set different k values
k3.p <- kmeans(df2, centers = 3, nstart = 25)
k4.p <- kmeans(df2, centers = 4, nstart = 25)
k5.p <- kmeans(df2, centers = 5, nstart = 25)
k6.p <- kmeans(df2, centers = 6, nstart = 25)

# plots to compare
p2.p <- fviz_cluster(k2.p, geom = "point", data = df2) + ggtitle("k = 2")
p3.p <- fviz_cluster(k3.p, geom = "point",  data = df2) + ggtitle("k = 3")
p4.p <- fviz_cluster(k4.p, geom = "point",  data = df2) + ggtitle("k = 4")
p5.p <- fviz_cluster(k5.p, geom = "point",  data = df2) + ggtitle("k = 5")
p6.p <- fviz_cluster(k6.p, geom = "point",  data = df2) + ggtitle("k = 6")

#print plots
grid.arrange(p2.p, p3.p, p4.p, p5.p, p6.p, nrow = 3)


## -----------------------------------------------------------------------------
#Elbow Method:
set.seed(1000)

#compute total within-cluster sum of square 
wss.p <- function(k.p) {
  kmeans(df2, k.p, nstart = 10 )$tot.withinss
}

#Compute and plot wss for k = 1 to k = 10
k.values.p <- 1:10
 
# extract wss for 1-10 clusters
wss_values.p <- map_dbl(k.values.p, wss.p)
plot(k.values.p, wss_values.p,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


## -----------------------------------------------------------------------------
fviz_nbclust(df2, kmeans, method = "silhouette")


## -----------------------------------------------------------------------------
#A merge: This is our own function to plot rsq vs. silhouette
kmeans_perf2.p = function(p.data,maxc,ns)
{
  result.p = as.data.frame(matrix(ncol=3, nrow=maxc-1))
  colnames(result.p) = c("clusters", "rsq","silhouette")
  dst <- daisy(df2)
  for(i in 2:maxc) {
    cst <- kmeans(df2,i,iter.max=100,nstart=ns)
    rsq <- 1-cst$tot.withinss/(cst$totss)
    slht <- silhouette(cst$cluster,dst)
    result.p[i-1,]=c(i,rsq,mean(slht[,3]))
  }
  ggplot(result.p, aes(clusters)) + 
    geom_line(aes(y = rsq, colour = "rsq")) + 
    geom_line(aes(y = silhouette, colour = "silhouette"))
  }
kmeans_perf2.p(df2,10,ns=10)


## -----------------------------------------------------------------------------
set.seed(100)
k<-4
final.p <- kmeans(df2, centers = k, nstart = 25)
final.p
final.p$size
fin <- fviz_cluster(final.p, geom = "point",  data = df2) + ggtitle("k = 4")
print(fin)
df2

df2 %>%
  mutate(Cluster = final.p$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

newdf <- cbind(df2,Cluster = final.p$cluster)
head(newdf)
str(newdf)


## -----------------------------------------------------------------------------
d <- dist(df2, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, hang = -1)

# Compute with agnes
hc2 <- agnes(df2, method = "complete")
names(hc2)

# Agglomerative coefficient
#if close to 1 then its good
hc2$ac

# methods to assess
methods <- c( "average", "single", "complete", "ward")
names(methods) <- c( "average", "single", "complete", "ward.D1")

# function to compute coefficient
ac <- function(x) {
  agnes(df, method = x)$ac
}

map_dbl(methods, ac)

# choose the best one
hc3 <- agnes(df2, method = "ward")
pltree(hc3, hang = -1, main = "Dendrogram of agnes") 

#In order to identify sub-groups (i.e. clusters), we can cut the dendrogram with cutree:
# Cut tree into 2 groups
sub_grp <- cutree(hc3, k = 4)
table(sub_grp)

#We can also use the cutree output to add the cluster each observation belongs to to our original data.
data %>%
  mutate(cluster = sub_grp) %>%
  head

#possible to draw the dendrogram with a border around the 2 clusters. 
#The argument border is used to specify the border colors for the rectangles:
rect.hclust(hc3, k = 4, border = 2:5)

#visualize the result in a scatter plot
fviz_cluster(list(data = df2, cluster = sub_grp))

#Determining Optimal Clusters
?hcut
#1.Elbow Method:
fviz_nbclust(df2, FUN = hcut, method = "wss")
fviz_nbclust
#2.Average Silhouette Method:
fviz_nbclust(df2, FUN = hcut, method = "silhouette")

