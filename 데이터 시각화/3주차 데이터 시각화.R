#선을 그리는 함수 abline()
head(cars)
z <- lm(dist~speed,data=cars)
z
z$coefficients
plot(cars,main="abline")
abline(h=20)
abline(h=30)
abline(v=20,col='blue')
abline(a=40,b=4,col='red')
abline(z,lty=2,lwd=2,col='green')
abline(z$coef,lty=2,lwd=2,col='red')

#line() 함수
op <- par(no.readonly = TRUE)
par(mar = c(0,2,3,2))
lty1 <- c("blank","solid","dashed","dotted","dotdash","longdash","twodash")
lty2 <- c("33","24","F2","2F","3313","F252","FF29")
plot(0:6,0:6,type = "n",ylim=c(0,20),xlab="",ylab="",main="lines")

#화살표 함수
