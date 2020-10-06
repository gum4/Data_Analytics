rm(list=ls())

###############   lab2 part1   ###############
EPI_data <- read.csv("EPI_data.csv")
attach(EPI_data); 
boxplot(ENVHEALTH,DALY,AIR_H,WATER_H)

#### ENVHEALTH #####
lmENVH<-lm(ENVHEALTH~DALY+AIR_H+WATER_H)

lmENVH

summary(lmENVH)

cENVH<-coef(lmENVH)
DALYNEW<-c(seq(5,95,5))
AIR_HNEW<-c(seq(5,95,5))
WATER_HNEW<-c(seq(5,95,5))
NEW<-data.frame(DALYNEW,AIR_HNEW,WATER_HNEW)
pENV<- predict(lmENVH,NEW,interval="prediction")
cENV<- predict(lmENVH,NEW,interval="confidence")

#### AIR_E #####
lmENVH<-lm(AIR_E~DALY+AIR_H+WATER_H)

summary(lmENVH)

cENVH<-coef(lmENVH)
DALYNEW<-c(seq(5,95,5))
AIR_HNEW<-c(seq(5,95,5))
WATER_HNEW<-c(seq(5,95,5))
NEW<-data.frame(DALYNEW,AIR_HNEW,WATER_HNEW)
pENV<- predict(lmENVH,NEW,interval="prediction")
cENV<- predict(lmENVH,NEW,interval="confidence")
pENV
cENV

#### CLIMATE #####
lmENVH<-lm(CLIMATE~DALY+AIR_H+WATER_H)

summary(lmENVH)

cENVH<-coef(lmENVH)
DALYNEW<-c(seq(5,95,5))
AIR_HNEW<-c(seq(5,95,5))
WATER_HNEW<-c(seq(5,95,5))
NEW<-data.frame(DALYNEW,AIR_HNEW,WATER_HNEW)
pENV<- predict(lmENVH,NEW,interval="prediction")
cENV<- predict(lmENVH,NEW,interval="confidence")
pENV
cENV

###############   lab2 part2   ###############
# Q1 #
DMR <- read.csv("dataset_multipleRegression.csv")
attach(DMR)
lm1<-lm(ROLL~UNEM+HGRAD)
summary(lm1)
ROLL_pred<--8.256e+03+6.983e+02*0.07+9.423e-01*90000
ROLL_pred
lm2<-lm(ROLL~UNEM+HGRAD+INC)
summary(lm2)
ROLL1_pred<--9.153e+03+4.501e+02*0.07+4.065e-01*90000+4.275e+00*25000
ROLL1_pred

# Q2 #
abalone <- read.csv("abalone.csv")
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight', 'rings' )
abalone$rings <- as.numeric(abalone$rings)
abalone$rings <- cut(abalone$rings, br=c(-1,8,11,35), labels = c("young", 'adult', 'old'))
abalone$rings <- as.factor(abalone$rings)
summary(abalone$rings)
aba <- abalone
aba$sex <- NULL
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
aba[1:7] <- as.data.frame(lapply(aba[1:7], normalize))
summary(aba$shucked_wieght)
ind <- sample(2, nrow(aba), replace=TRUE, prob=c(0.7, 0.3))
KNNtrain <- aba[ind==1,]
KNNtest <- aba[ind==2,]
library(class)
KNNpred <- knn(train = KNNtrain[1:7], test = KNNtest[1:7], cl = KNNtrain$rings, k = 55)
KNNpred
table(KNNpred)

# Q3 #
library(ggplot2)

sapply(iris[,-5], var)
summary(iris)

ggplot(iris,aes(x = Sepal.Length, y = Sepal.Width, col= Species)) + geom_point()

ggplot(iris,aes(x = Petal.Length, y = Petal.Width, col= Species)) + geom_point()

set.seed(300)
k.max <- 12

wss<- sapply(1:k.max,function(k){kmeans(iris[,3:4],k,nstart = 20,iter.max = 20)$tot.withinss})

plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k)", ylab = "Within cluster sum of squares")
icluster <- kmeans(iris[,3:4],3,nstart = 20)
table(icluster$cluster,iris$Species)

# Q4 #
library(dplyr)
install.packages('nycflights13')
library(nycflights13)
E<-as.data.frame(EPI_data$EPI)
D<-as.data.frame(EPI_data$DALY)
se_1<-sample_n(E, 5)
se_2<-sample_frac(E,0.1)
sd_1<-sample_n(D, 5)
sd_2<-sample_frac(D,0.1)
se_1
arrange( sample_n(filter(E), 5) ,desc(E))
arrange( sample_n(filter(D), 5) ,desc(D))
