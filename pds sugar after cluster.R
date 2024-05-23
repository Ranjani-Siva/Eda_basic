
head(data)
sum(is.na(data))
summary(data)
factor(data$officeCode)
factor(data$sugar)
#createdata
#create a attribute of address is urban / rural
distric_class = data.frame(
  distName=c("Adilabad","Bhadrdri Kothagudem","Hanumakonda","Hyderabad","Jagityal","Janagaon","Jayashankar Bhupalpalli","Jogulamba Gadwal","Kamareddy","Karimnagar","Khammam","Kumarambheem Asifabad","Mahabubabad","Mahbubnagar","Manchiryala","Medak","Medchal","Mulugu","Nagarkarnool","Nalgonda","Narayanpet","Nirmal","Nizamabad","Peddapalli","Rajanna Siricilla","Ranga Reddy","Sangareddy","Siddipet","Suryapet","Vikarabad","Wanaparthy","Warangal","Yadadri Bhuvanagiri" ),
  Classifi=c("Rural","Rural","Urban","Urban","Rural","Urban","Rural","Rural","Rural","Rural","Rural","Rural","Rural","Rural","Rural","Rural","Rural","Rural","Rural","Rural","Rural","Urban","Rural","Rural","Rural","Urban","Rural","Rural","Urban","Rural","Urban","Rural","Rural"))
factor(distric_class$distName)
#Merge the classification data with the main dataset based on district names
sub1=merge(data,distric_class, by='distName',all.x=TRUE)

#sub group the data
sub1=data.frame(sub1$distCode,sub1$Classifi,sub1$officeCode,sub1$shopNo,sub1$noOfRcs,sub1$noOfTrans,sub1$riceAfsc,sub1$riceFsc,sub1$riceAap,sub1$sugar,sub1$totalAmount,sub1$otherShopTransCnt)
head(sub1)
sum(is.na(sub1))
summary(sub1)
sub1 = sub1[apply(sub1!= 0, 1, all),]
head(sub1)
summary(sub1)

#Divisive clustering

#distance matrix
distancematrix=dist(sub1,method='euclidean')
head(distancematrix)

#plot of hierarchical view of data
set.seed(40)
hier_clus=hclust(distancematrix,method='average')
plot(hier_clus,hang=0.1,main='hierarchical clustering',xlab = "",ylab="Height")


library(dendextend)
dend=as.dendrogram(hier_clus)
COLS=c("orange","turquoise","pink","lightblue","darkgreen","blue","green","white","brown","black","yellow","purple")
names(COLS)=unique(sub1)
dend=color_labels(dend, col=COLS)
plot(dend,main='hierarchical clustering',xlab = "PDS levels ",ylab="Height")
rect.hclust(hier_clus,k=3,border = 'red')

#accuracy of clustering
fit=cutree(hier_clus,k=3)
table(fit)
cm=as.matrix(table(fit))
cm
accuracy = sum(diag(cm))/sum(cm)
accuracy

