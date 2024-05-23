#Dataset
head(quine)
#Education dataset
edu=quine
#structure of the dataset
str(quine)
#summary of the dataset
summary(edu)

#Check the null values or missing values in dataset
sum(is.na(edu))

#subset1 of Economy ststus is low the learning is high
edu_sub1=data.frame(edu$Eth,edu$Lrn,edu$Age,edu$Sex)

#subset2 of gender and age wise no of days differ
edu_sub2=data.frame(edu$Sex,edu$Age,edu$Days)

#eda of subset1
#understand the attributes of the quine dataset
#histogram of bakground status of students
histogram(~edu_sub1$edu.Eth,breaks = 50, main="Histogram of student background status",
          xlab='students',ylab='frequency',col=c(2,3))
#histogram of learning ratio of students
histogram(~edu_sub1$edu.Lrn,breaks = 50, main="Histogram of student learning",
          xlab='Learning category',ylab='frequency',col=c(2,3))
#histogram of studuents by there age
histogram(~edu_sub1$edu.Age,breaks = 20, main="Histogram of student age",
          xlab='Age',ylab='frequency',col=c(2,3))
#histogram of students by gender
histogram(~edu_sub1$edu.Sex,breaks = 50, main="Histogram of student by gender",
          xlab='gender',ylab='frequency',col=c(2,3))

#Bi variate
bwplot(edu_sub1$edu.Sex~factor(edu_sub1$edu.Age)|edu_sub1$edu.Lrn,scales=list(log=FALSE),
       xlab="Age group",ylab="Gender",main="The age wise gender students absent")
bwplot(edu_sub1$edu.Sex~factor(edu_sub1$edu.Age)|edu_sub1$edu.Eth,scales=list(log=FALSE),
       xlab="Age group",ylab="Gender",main="The age wise economic status")

#Multivarite

#eda of subset2
#histogram of students
histogram(~edu_sub2$edu.Days,breaks = 50, main="Histogram of student absent days",
          xlab='student',ylab='frequency',col=c(2,3))
boxplot(edu_sub2$edu.Days)
days=edu_sub2$edu.Days >= 60
days
iloc(days=TRUE)
i=data.frame(with(edu_sub2,subset(edu.Age,edu.Days >= 40)))
sum(isTRUE(i))
i
