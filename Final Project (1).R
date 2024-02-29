library(dplyr)
library(quantmod)
library(ggplot2)
library(data.table)
library(moments)

project=read.csv("electricityConsumptionAndProductioction.csv")
head(project)


#calculating mean 
mean(project$Consumption)
mean(project$Production)
mean(project$Nuclear)
mean(project$Wind)
mean(project$Hydroelectric)
mean(project$Oil_and_Gas)
mean(project$Coal)
mean(project$Solar)
mean(project$Biomass)




#standard deviation
sd(project$Consumption)
sd(project$Production)
sd(project$Nuclear)
sd(project$Wind)
sd(project$Hydroelectric)
sd(project$Oil_and_Gas)
sd(project$Coal)
sd(project$Solar)
sd(project$Biomass)




#calculating median
median(project$Consumption)
median(project$Production)
median(project$Nuclear)
median(project$Wind)
median(project$Hydroelectric)
median(project$Oil_and_Gas)
median(project$Coal)
median(project$Solar)
median(project$Biomass)


mean=sum(project$Consumption*project$Production)
mean




#producing a histogram of the project
project1=select(project,Consumption)
project1
hist(as.vector(project1$Consumption),xlab="Consumption",main ="project",col ="yellow")

project2=select(project,Production)
project2
hist(as.vector(project2$Production),xlab="Production",main = "project",col = "steelblue")

project3=select(project,Nuclear)
project3
hist(as.vector(project3$Nuclear),xlab="Nuclear",main ="project",col ="red")

project4=select(project,Wind)
project4
hist(as.vector(project4$Wind),xlab="Wind",main ="project",col="orange")

project5=select(project,Hydroelectric)
project5
hist(as.vector(project5$Hydroelectric),xlab="Hydroelectric",main="project",col = "purple")




#linear regression 
model=lm(project$Consumption~project$Production)
summary(model)




#plotting of graph of project
plot(density(project$Consumption),col="red")
plot(density(project$Production),col="blue")
plot(density(project$Hydroelectric),col="black")
plot(density(project$Coal),col="green")





#skewness
skewness(project$Consumption)
kurtosis(project$Consumption)
skewness(project$Production)
kurtosis(project$Production)
skewness(project$Solar)
kurtosis(project$Solar)
x=project$Consumption
y=project$Solar
print(cov(x,y))
print(cov(x,y,method = "pearson"))





#by using ggplot
ggplot(project,aes(Production))+geom_bar()
ggplot(project,aes(Consumption))+geom_bar()
ggplot(project,aes(Wind))+geom_histogram()
ggplot(project,aes(Oil_and_Gas))+geom_boxplot()
ggplot(project,aes(Solar))+geom_point()




#correlation
plot(project$Consumption,project$Production,xlab='Consumption',ylab='Production',col='red')
cor(project$Consumption,project$Production)
#correlation
plot(project$Nuclear,project$Wind,xlab='Nuclear',ylab='Wind',col='purple')
cor(project$Nuclear,project$Wind)




#linear regression
plot(project$Consumption,project$Production,col='orange')
b=lm(project$Consumption~project$Production)
b
#linear regression
plot(project$Nuclear,project$Wind,col='yellow')
c=lm(project$Nuclear~project$Wind)
c



#normal distribution
normal=dnorm(project$Oil_and_Gas)
plot(normal,main = "Normal distribution")



plot(project$Consumption,type="l",col="blue",xlab="Production",ylab="consumption",main="analysis",xlim=c(0,10000),ylim=c(5000,9000))
lines(project$Production,type="l",col="red")

plot(project$Oil_and_Gas,type="l",col="blue",xlab="Production",ylab="consumption",main="analysis",xlim=c(0,10000),ylim=c(5000,9000))
lines(project$Nuclear,type="l",col="red")

A#piechart
install.packages("plotrix")
library("plotrix")
piechart=c(19.2,400.0,175)
names1=c('Consumption','Production','Hydroelectric')
pie3D(piechart,labels = names1,explode = 0.1,main="total average",col = rainbow(7))
legend("toplet",c('Consumption','Production','Hydroelectric'),cex = 0.8,fill = rainbow(names1))



piechart=c(6707,6543,1293,785,1801,1181,1270,151,58)
chart1=c('Consumption','Production','Nuclear','Wind','Hydroelectric','Oil_and_Gas','Coal','Solar','Biomass')
pie3D(piechart,labels = chart1,main="total average",col = rainbow(7))

plot(project$Consumption,type="l",col="blue",xlab="Production",ylab="consumption",main="analysis")
lines(project$Production,type="l",col="red")
