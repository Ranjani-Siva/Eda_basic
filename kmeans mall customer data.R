library(lattice)
library(cluster)
data=read.csv('C:\\Users\\India\\Documents\\Mall_Customers.csv.csv')
head(data)

#descriptive statistics
summary(data)
str(data)

#Checking the null values
sum(is.na(data))

#subset the dataset for assumption
sub=data.frame(data$Gender,data$Age,data$Annual.Income..k..,data$Spending.Score..1.100.)

#Eda
#Univariant
#Annual income of the customer
histogram(sub$data.Annual.Income..k..,breaks=c(5),col = '#F52581',xlab='Annual income',main='Annual income of the customer')

#Gender wise Annual income of the customer
histogram(~sub$data.Annual.Income..k..|factor(sub$data.Gender),breaks=c(5),col=c(3),xlab='Gender wise Annual income',main='Annual income of the customer')

#Age of the customer
histogram(sub$data.Age,breaks=c(5), col='pink',xlab='Age',main='The age of the Customers')

#Spending score of the customer
histogram(sub$data.Spending.Score..1.100.,breaks=c(5), col='orange',xlab='Spending score',main='The Spending score of the customers')

#Gender wise spending score of the customer
histogram(~sub$data.Spending.Score..1.100.|factor(sub$data.Gender),breaks=c(6),col=c(4),xlab='Gender wise spending score',main='Spending score of the customer')

#Bi variant
#Annual income Vs Age of the customer
plot(sub$data.Annual.Income..k.. ,sub$data.Age,col='purple',xlab=('Annual income'),ylab=('No of ages'),main=('The Annual income Vs age'))

#Annual income Vs Age of the customer
boxplot(sub$data.Annual.Income..k..,sub$data.Age,col='purple',xlab=('Annual income'),ylab=('No of ages'),main=('The Annual income Vs age'))

#Annual income Vs Spending score
plot(sub$data.Annual.Income..k.. ,sub$data.Spending.Score..1.100.,col='blue',xlab=('Annual income'),ylab=('Spending score'),main=('The Annual income Vs spending score'))

#Age Vs Spending score of the customer
plot(sub$data.Age ,sub$data.Spending.Score..1.100.,col='purple',xlab=('Age'),ylab=('Spending score'),main=('Spending score Vs age'))

#multivariant

hi = sub[c("data.Age","data.Annual.Income..k..","data.Spending.Score..1.100.")]
hea = cor(hi)
levelplot(hea,main = "Correlation Heatmap of Orange Attributes",xlab = "Attributes", ylab = "Attributes",
          col.regions = colorRampPalette(c("white", "red")))
     
#Determining number of clusters
sub2=data.frame(data$Age,data$Annual.Income..k..,data$Spending.Score..1.100.)
scale(sub2)
wss=(nrow(sub2)-1)*sum(apply(sub2,2,var))
for (i in 2:15)wss[i]=sum(fit=kmeans(sub2,centers=i,25)$withinss)
plot(1:15,wss,type='b',main='Cluster of 15',xlab='No. of clusters',ylab='With cluster sum of squares')


#kmeans function with cluster objects k=2
set.seed(20)
cluste=kmeans(sub2,2,iter.max=1000)
head(cluste)

#find centroids
clustemean=aggregate (sub2,by=list(cluste$cluster),FUN=mean)
head(clustemean)

#cluster visualization
plot(sub2,col=cluste$cluster,pch=15)
points(cluste$centers,col=1:5,pch=5)
clusplot(sub2,cluste$cluster,color=TRUE,shade=TRUE,labels=2,lines=0)

#silhoutee index
silhouette_index = silhouette(cluste$cluster, dist(sub2))
plot(silhouette_index)

#computing silhoutee index
silh=silhouette_index[,1:3]

#objects with negative silhouette 
nesil=which(silh[,'sil_width']<0)
silh[nesil, , drop=FALSE]


#kmeans function with cluster objects k=3
set.seed(20)
cluste=kmeans(sub2,3,iter.max=1000)
head(cluste)

#find centroids
clustemean=aggregate (sub2,by=list(cluste$cluster),FUN=mean)
head(clustemean)

#cluster visualization
plot(sub2,col=cluste$cluster,pch=15)
points(cluste$centers,col=1:5,pch=5)
clusplot(sub2,cluste$cluster,color=TRUE,shade=TRUE,labels=2,lines=0)

#silhoutee index
silhouette_index = silhouette(cluste$cluster, dist(sub2))
plot(silhouette_index)

#computing silhoutee index
silh=silhouette_index[,1:3]

#objects with negative silhouette 
nesil=which(silh[,'sil_width']<0)
silh[nesil, , drop=FALSE]


#kmeans function with cluster objects k=4
set.seed(20)
cluste=kmeans(sub2,4,iter.max=1000)
head(cluste)

#find centroids
clustemean=aggregate (sub2,by=list(cluste$cluster),FUN=mean)
head(clustemean)

#cluster visualization
plot(sub2,col=cluste$cluster,pch=15)
points(cluste$centers,col=1:5,pch=5)
clusplot(sub2,cluste$cluster,color=TRUE,shade=TRUE,labels=2,lines=0)

#silhoutee index
silhouette_index = silhouette(cluste$cluster, dist(sub2))
plot(silhouette_index)

#computing silhoutee index
silh=silhouette_index[,1:3]

#objects with negative silhouette 
nesil=which(silh[,'sil_width']<0)
silh[nesil, , drop=FALSE]


#kmeans function with cluster objects k=5
set.seed(20)
cluste=kmeans(sub2,5,iter.max=1000)
head(cluste)

#find centroids
clustemean=aggregate (sub2,by=list(cluste$cluster),FUN=mean)
head(clustemean)

#cluster visualization
plot(sub2,col=cluste$cluster,pch=15)
points(cluste$centers,col=1:5,pch=5)
clusplot(sub2,cluste$cluster,color=TRUE,shade=TRUE,labels=2,lines=0)

#silhoutee index
silhouette_index = silhouette(cluste$cluster, dist(sub2))
plot(silhouette_index)

#computing silhoutee index
silh=silhouette_index[,1:3]

#objects with negative silhouette 
nesil=which(silh[,'sil_width']<0)
silh[nesil, , drop=FALSE]


#kmeans function with cluster objects k=6
set.seed(20)
cluste=kmeans(sub2,6,iter.max=1000)
head(cluste)

#find centroids
clustemean=aggregate (sub2,by=list(cluste$cluster),FUN=mean)
head(clustemean)

#cluster visualization
plot(sub2,col=cluste$cluster,pch=15)
points(cluste$centers,col=1:5,pch=5)
clusplot(sub2,cluste$cluster,color=TRUE,shade=TRUE,labels=2,lines=0)

#silhoutee index
silhouette_index = silhouette(cluste$cluster, dist(sub2))
plot(silhouette_index)

#computing silhoutee index
silh=silhouette_index[,1:3]

#objects with negative silhouette 
nesil=which(silh[,'sil_width']<0)
silh[nesil, , drop=FALSE]

#kmeans function with cluster objects k=7
set.seed(20)
cluste=kmeans(sub2,7,iter.max=1000)
head(cluste)

#find centroids
clustemean=aggregate (sub2,by=list(cluste$cluster),FUN=mean)
head(clustemean)

#cluster visualization
plot(sub2,col=cluste$cluster,pch=15)
points(cluste$centers,col=1:5,pch=5)
clusplot(sub2,cluste$cluster,color=TRUE,shade=TRUE,labels=2,lines=0)

#silhoutee index
silhouette_index = silhouette(cluste$cluster, dist(sub2))
plot(silhouette_index)

#computing silhoutee index
silh=silhouette_index[,1:3]

#objects with negative silhouette 
nesil=which(silh[,'sil_width']<0)
silh[nesil, , drop=FALSE]
