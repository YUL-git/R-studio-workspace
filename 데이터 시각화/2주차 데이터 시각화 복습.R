acs <- read.csv(url("http://stat511.cwick.co.nz/homeworks/acs_or.csv"))
head(acs)
bedrm <- table(acs$bedrooms)
bedrm
barplot(bedrm)
mode <- table(acs$mode)
barplot(mode)
plot(acs$age_husband, acs$age_wife)
age_husband <- table(acs$age_husband)
barplot(age_husband)
age_wife <- table(acs$age_wife)
barplot(age_wife)
income_husband <- table(acs$income_husband)
barplot(income_husband)
electricity <- table(acs$electricity)
barplot(electricity)
absent <- c(8,2,0,4,1)
absent
names(absent)
names(absent) <- c("Mon","Tue","Wed","Thr","Fri")
absent
names(absent)
return <- c(4,11,5,3,2)
return
names(return) <- c('shoes','shirt','pants','scarf','belt')
rereturn
return[1]
return[3]
return[1:3]
return[seq(1,2,3)]
return[-2]
x1 <-1:5
y1 <- x1^2
z1 <- 5:1
mat1 <- cbind(x1,y1,z1)
mat1
x <- c(5,2,3,NA,6,9,8,NA,11,7)
y1 <- sum(x,na.rm = TRUE)
y1
d <- 1:9
d>=5
d[d>5]
sum(d>5)
sum(d[d>5])
a <- 30:120
condi <- a%%3==0
sum(condi)
a[condi]
sum(a[condi])
op <- par(no.readonly = TRUE)
par(mfrow = c(2,3))
plot(y1, main= "using index")
plot(x=x1,y=y1,main="x^2")
plot(x1,y1,type= "l",main="line")
plot(x1,y1,type="h",main="high density")
plot(x1,y1,type="n",main="no potting")
par(op)
plot(x1,y1,type="p",main="점 그리프")
plot(x1,y1,type="b", main="점과 선 그래프")
plot(x1,y1,type="c", main="점이 빠진 그래프")
plot(x1,y1,type="o", main="겹친 점과 선 그래프")
x <- rep(1:5,rep(5,5))
x
y <- rep(5:1,5)
y
plot(1:5,type="n",xlim=c(0,7.5),ylim=c(0.5,5.5),main="points by 'pch'")
points(x,y,pch=1:25,cex=1.2)
text(x-0.4,y,labels=as.character(1:25),cex=1.2)
points(rep(6,5),5:1,pch=65:69,cex=1.5)
text(rep(6,5)-0.4,y,labels=as.character(65:69),cex=1.2)
points(rep(7,5),5:1,pch=pchs,cex=1.5)
x <- rep(5,5)
x
rep(1:5,rep(4,5))
