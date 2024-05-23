head(data)
sum(is.na(data))
summary(data)
factor(data$officeCode)
factor(data$sugar)
#creatdata#create a attribute of address is urban / rural
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
#eda

# To know the distCode
histogram(~sub1$sub1.distCode,lwd=2,col=c(2,3),main="district code",xlab="district code",ylab="count")

#To know the office code
histogram(~sub1$sub1.officeCode,col=c(2,3),main="office code",xlab="district classification",ylab="count")

#To know about the shops access from people
histogram(~sub1$sub1.shopNo,lwd=2,col=c(2,3),main="shop no",xlab="Shopno",ylab="count")

#To know about the rcs card is buy on shops
histogram(~sub1$sub1.noOfRcs,lwd=2,col=c(2,3),main="no of rcs card",xlab="rcs card",ylab="count")

#To know the transaction is on shops
histogram(~sub1$sub1.noOfTrans,lwd=2,col=c(2,3),main="no of transactions",xlab="no of transaction",ylab="count")

#To know how much ricefsc card is buy from shops
histogram(~sub1$sub1.riceAfsc,lwd=2,col=c(2,3),main="riceafsc card",xlab="no of riceafsc card",ylab="count")

#To know how much riceFsc card is buy from shops
histogram(~sub1$sub1.riceFsc,lwd=2,col=c(2,3),main="ricefsc card",xlab="rice afsc",ylab="count")

#To know how much riceAap is buy from shop
histogram(~sub1$sub1.riceAap,lwd=2,col=c(2,3),main="riceAap",xlab="rice aap",ylab="count")

#To know the amount of sugar is distributed to the people
histogram(~sub1$sub1.sugar,lwd=2,col=c(2,3),main="sugar",xlab="sugar",ylab="count")

#To know the incoming status on shops
histogram(~sub1$sub1.totalAmount,lwd=2,col=c(2,3),main="amount",xlab="trancation amount",ylab="count")

#To see the how much shop is shared with nearby shops
histogram(~sub1$sub1.otherShopTransCnt,lwd=2,col=c(2,3),main="other transaction",xlab="transaction",ylab="count")
summary(sub1)
scale(sub1$sub1.distCode)
scale(sub1$sub1.officeCode)
scale(sub1$sub1.shopNo)
scale(sub1$sub1.noOfRcs)
scale(sub1$sub1.noOfTrans)
scale(sub1$sub1.riceAfsc)
scale(sub1$sub1.riceFsc)
scale(sub1$sub1.riceAap)
scale(sub1$sub1.sugar)
scale(sub1$sub1.totalAmount)
sub=scale(sub1$sub1.otherShopTransCnt)

summary(sub1)
heatmap(data)
#Divisive clustering
#Determining the number of clusters
wss=(nrow(sub1)-1)*sum(apply(sub1,2,var))
for (i in 2:15)wss[i]=sum(fit=kmeans(sub1,centers=i,15)$withinss)
plot(1:15,wss,type='b',main="cluster of 12",
     xlab = "No of Clusters",ylab="With cluster sum of squares")


#distance matrix
distancematrix=dist(sub1,method='euclidean')
head(distancematrix)

set.seed(240)
hier_clus=hclust(distancematrix,method='average')

pltree(hier_clus,cex=0.6,hang=-1,main="Hierarchical clustering")

plot(hier_clus,hang=0.1,main='hierarchical clustering')

fit=cutree(hier_clus,k=4)
table(fit)
rect.hclust(hier_clus,k=4,border = 'red')
write.csv(sub1,file="data.csv")
