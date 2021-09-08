acs <- read.csv(url("http://stat511.cwick.co.nz/homeworks/acs_or.csv"))
head(acs)
bedrm <- table(acs$bedrooms)
bedrm
barplot(bedrm)
hus_age <- table(acs$age_husband)
hus_age #실제로 범주화 되어있지
barplot(hus_age)
plot(acs$age_husband, acs$age_wife)
age_husband <- table(acs$age_husband)
barplot(age_husband)
age_wife <- table(acs$age_wife)
barplot(age_wife)
income_husband <- table(acs$income_husband)
barplot(income_husband)
electricity <- table(acs$electricity)
barplot(electricity)
number_children <- table(acs$number_children)
barplot(number_children)
internet <- table(acs$internet)
barplot(internet)
own <- table(acs$own)
barplot(own)
languaage <- table(acs$language)
barplot(languaage)
decade_built <- table(acs$decade_built)
barplot(decade_built)
x1 <- 1:5
y1 <- x1^2
z1 <- 5:1
mat1 <- cbind(x1, y1, z1)
mat1
op <- par(no.readonly = TRUE)
par(mfrow = c(2,3))
plot(y1,main="using index") #using index라는 이름을 준거야
plot(x=x1, y=y1, main="x^2")
plot(mat1,main="using matrix")
plot(x1, y1, type = "l", main="line")
plot(x1, y1, type="S", main = "high density")
plot(x1, y1, type = "n", main="no plotting")
par(op)
x <- rep(1:5, rep(5,5))
x #제대로 만들어졌는지 확인
y <- rep(5:1, 5)
y
pchs <- c("$", "z", "Z", "1", "가")
plot(1:5,type="n",xlim = c(0,7.5),ylim=c(0.5,5.5), 
     main = "points by 'pch'")
points(x)
points(y)
points(x,y, pch=1,cex=1.2)
points(x,y, pch=1:25, cex=1.2)
text(x-0.4,y,labels=as.character(1:25),cex=1.2)
