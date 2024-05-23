Orange
sub=data.frame(Orange$Tree,Orange$age,Orange$circumference)
View(sub)
sum(is.na(sub))

#descriptive statistics
summary(sub)
str(sub)

#Creating attribute radius and diameter with circumference of tree
sub1=sub %>%
  mutate(radius = sub$Orange.circumference/6.28)
sub2=sub %>%
  mutate(diameter = sub$Orange.circumference/3.14)
sub2

#Univariant
hist(sub$Orange.circumference, col = '#F52581',xlab='circumference of tree',main='circumference of the tree')
hist(sub$Orange.age, col='pink',xlab='age of the tree',main='The age of the tree attribute')
hist(sub2$diameter, col='orange',xlab='diameter of the tree',main='The diameter of the tree attribute')

#Bi variant

plot(sub2$Orange.Tree,sub2$Orange.age,col='purple',xlab=('tree varity'),ylab=('No of ages'),main=('The tree varity with respect to age'))
plot(sub2$Orange.Tree,sub2$Orange.circumference,col='#F29034',xlab='Tree varity',ylab=('Circumference'),main=('The tree varity with respect to circumference'))
plot(sub2$Orange.Tree,sub2$diameter,col='brown',xlab='Tree varity',ylab=('Diameter'),main=('The tree varity with respect to diameter'))
plot(sub2$Orange.age,sub2$Orange.circumference,col='red',xlab='Age of the tree',ylab=('Circumference'),main=('The age with respect to circumference'))
plot(sub2$Orange.age,sub2$diameter,col='green',xlab='Age of the tree',ylab=('Diameter'),main=('The age with respect to diameter'))

sub2=scale(sub2)
head(sub2)
#Determining number of clusters
sub2=data.frame(sub2$Orange.age,sub2$Orange.circumference)
wss=(nrow(sub2)-1)*sum(apply(sub2,2,var))
for (i in 2:15)wss[i]=sum(fit=kmeans(sub2,centers=i,25)$withinss)
plot(1:15,wss,type='b',main='Cluster of 15',xlab='No. of clusters',ylab='With cluster sum of squares')

#kmeans function with cluster objects
set.seed(20)
cluste=kmeans(sub2,5,iter.max=80)
View(cluste)

#find centroids
clustemean=aggregate (sub2,by=list(cluste$cluster),FUN=mean)
View(clustemean)

#cluster visualization
plot(sub2,col=cluste$cluster,pch=15)
points(cluste$centers,col=1:5,pch=5)
clusplot(sub2,cluste$cluster,color=TRUE,shade=TRUE,labels=2,lines=0)
plotcluster(sub2,cluste$cluster)

#silhoutee index
silhouette_index = silhouette(cluste$cluster, dist(sub2))
plot(silhouette_index)

#computing silhoutee index
silh=silhouette_index[,1:3]

#objects with negative silhouette 
nesil=which(silh[,'sil_width']<0)
silh[nesil, , drop=FALSE]

#confusion matrix

conf_matrix=matrix(0, nrow = 39, ncol =4)

# Iterate through each predicted and true label, and update the confusion matrix
for (i in 1:length(sub2$Orange.Tree)) {
  pred = model[i]
  true = sub2$sub2.Orange.age[i]
  conf_matrix[pred, true] =conf_matrix[pred, true] + 1
}

# Print the confusion matrix
print(conf_matrix)

