rm(list=ls())
data("iris")
str(iris)
library(ggplot2)
library(e1071)
#install.packages('e1071')
help("svm")
svm_m1<-svm(Species~.,data=iris)
summary(svm_m1)
plot(svm_m1, data = iris, Petal.Width~Petal.Length, slice = list(Sepal.Width = 3, Sepal.Length = 4))
pred1<-predict(svm_m1,iris)
table1<-table(Predicted=pred1,Actual=iris$Species)
table1
Model1_accuracyRate = sum(diag(table1))/sum(table1)
Model1_accuracyRate
# We can calcuate the missclassification rate 
Model1_MissClassificationRate = 1 - Model1_accuracyRate
Model1_MissClassificationRate
