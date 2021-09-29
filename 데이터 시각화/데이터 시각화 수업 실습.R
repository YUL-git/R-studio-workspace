# 1. 정보 시각화 방법
##관계 시각화
#산점도
library(ggplot2)
ggplot(mtcars,aes(wt,mpg))+
  geom_point()+
  ggtitle("자동차 무게와 연비 관계")+
  theme(plot.title=element_text(size=20,hjust=0.5,face="bold"))

#버블 차트
ggplot(mtcars,aes(wt,mpg))+
  geom_point(aes(size=cyl))+
  ggtitle("자동차 실린더 수를 기반으로 무게와 연비 관계")+
  theme(plot.title=element_text(size=20,hjust=0.5,face="bold"))

#히스토그램
install.packages("ggplot2movies")
library(ggplot2movies)         
ggplot(movies,aes(x=rating))+
  geom_histogram(binwidth=0.5,fill="green",colour="white")
##비교시각화

# 1. 범례만들기
plot(1:10,type='n',xlab='',ylab='',main='legend')
legend("bottomright","(x,y)",pch=1,title='bottomright')

# 실습
legend("bottom","(x,y)", pch=1,title="bottom")
legend("bottomleft","(x,y)",pch=1,title="bottomleft")
legend("left","(x,y)",pch=1,title="left")
legend("topleft","(x,y)",pch=1,title="topleft")
legend("top","(x,y)",pch=1,title="top")
legend("topright","(x,y)",pch=1,title="topright")
legend("right","(x,y)",pch=1,title="right")
legend("center","(x,y)",pch=1,title="center")

# 좌표롤 위치를 부여하자
legends <- c("Legend1","Legend2")
legend(3,8,legend=legends,pch=1:2,col=1:2)
legend(7,8,legend=legends,pch=1:2,col=1:2,lty=1:2)

# fill을 이용
legend(3,4,legend=legends,fill=1:2)
legend(7,4,legend=legends,fill =1:2,density=30)
legend(locator(1),legend="Locator",fill=1)

# 2. 좌표축을 그리는 함수
op <- par(no.readonly = TRUE)
par(oma=c(0,0,2,0))
plot(1:5,type="l",main="axis",axes=FALSE,xlab='',ylab='')

axis(side=1,at=1:5,labels=LETTERS[1:5],line=2)
axis(side=2,tick=F,col.axis='blue')
axis(side=3,outer=T)
axis(side=3,at=c(1,3,5),pos=3,col='blue',col.axis='red')
axis(side=4,lty=2,lwd=2)
par(op)

#3. 기타 저수준 그래픽 함수 (grid)
op <- par(no.readonly = TRUE)
par(mar=c(4,4,2,2),mfrow=c(2,1))
plot(iris$Sepal.Length,iris$Sepal.Width,pch=16,col=as.integer(iris$Species))
grid()
title("grid")

plot(iris$Sepal.Length,iris$Sepal.Width,pch=16,col=as.integer(iris$Species))
grid()
title("grid(3,3,lty=1,lwd=2,col=\"blue\")")
par(op)

# rug함수
op <- par(no.readonly=TRUE)
par(mar=c(4,4,2,2),mfrow=c(1,1))
plot(density(quakes$lat),main="rug(lat)")
rug(quakes$lat)

plot(density(quakes$long),main="side=3,col='blue',ticksize=0.04")
rug(quakes$long,side=3,col="blue",ticksize=0.04)
par(op)

#par 함수
m <- matrix(c(1,1,2,3),ncol=2,byrow=T)
m
layout(mat=m)
plot(cars,main="scatter plot of cars data",pch=19,col=4)
hist(cars$speed)
hist(cars$dist)

# layout()
m3 <- matrix(c(1,1,2,3,4,5,3,6,7),ncol=3,byrow=T)
m3
layout(mat=m3)
plot(cars,main="scatter plot of cars data",
     pch=19,col=4)
hist(cars$speed);hist(cars$dist);hist(cars$dist);hist(cars$dist);hist(cars$speed);hist(cars$speed)
#par 함수 안의 fig인수
par(fig=c(0,1,0,1))
plot(cars,main="scatter plot of cars data by fig")

par(fig=c(0,1,0.5,1))
plot(cars,main="scatter plot of cars data by fig")
#par 함수 안의 new인수
op <- par(no.readonly = T)
par(mfrow=c(2,2))
plot(1:10,type="l",main="plot")
par(new=F)
plot(10:1,type='s',main="plot by new=F")
plot(1:10,type="l")
x<- rnorm(10)
plot(x)
par(new=T)
hist(x)
par(op)

#type인수
op <- par(no.readonly = TRUE)
par(mfrow=c(2,3))
plot(0:6,0:6,main="dafault")
plot(0:6,0:6,type="b",main="type=\"b\"")
plot(0:6,0:6,type="c",main="type=\"c\"")
plot(0:6,0:6,type="o",main="type=\"o\"")
plot(0:6,0:6,type="s",main="type=\"s\"")
plot(0:6,0:6,type="S",main="type=\"S\"")
par(op)
