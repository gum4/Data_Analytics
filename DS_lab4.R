rm(list=ls())
data(Titanic)
str(Titanic)
#install.packages("FNN")
library(FNN)
x = runif(1000,0,pi)#随机生成服从正态分布的100个随机数
e = rnorm(1000,0,0.1)#随机生成100个随机数，均值是0，方差是0.1
y = sin(x)+e

grid2=data.frame(x)
knn1 = knn.reg(train = x, test = grid2, y = y, k = 1)
knn10 = knn.reg(train = x, test = grid2, y = y, k = 10)
knn1000 = knn.reg(train = x, test = grid2, y = y, k = 1000)
plot(x,y,type = 'n')
points(x,y,pch=16,col='black')
I = order(grid2$x)
lines(grid2$x[I],knn1$pred[I],lwd=2,col=5)
lines(grid2$x[I],knn100$pred[I],lwd=2,col=6)
lines(grid2$x[I],knn10$pred[I],lwd=2,col=2)
coe=coef(lm(y~x+I(x^2)))
lines(x[ORD],coe[1]+coe[2]*x[ORD]+coe[3]*x[ORD]^2,
      lwd=2,col=4)

apply(Titanic,c(3),sum)
EPI_data <- read.csv("EPI_data.csv")

x=EPI_data$EPI
x[is.na(x)]=0
x
y=EPI_data$Population07
y[is.na(y)]=0
y
S=cbind(x,y)



out.dist=dist(S,method="euclidean") 
out.hclust=hclust(out.dist,method="complete")
plot(out.hclust) 
