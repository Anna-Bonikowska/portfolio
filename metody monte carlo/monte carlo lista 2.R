
log(2)
#zad 1

#.

#n ilosc generowanych
#lam - parametr lambda
gener_wykl <- function(lam, n){
  
  u<-runif(n)
  
  x<-c()
  
  for(i in 1:n){
    x<-c(x,-(1/lam)*log(x =1 - u[i]))
  }
  
  return(x[])
}

liczby <- gener_wykl(1/3,5000)
hist(liczby , breaks= 40, probability = TRUE)

x<- seq(0,25, length.out = 1000)
wyk<- dexp(x,1/3)*2500*3

hist(gener_wykl(1/3,5000))

lines(x, wyk, type = "l", col='red', lwd= 2)

#..

#n ilosc generowanych
#lam - parametr lambda
#k - parametr
gener_Weibulla <- function(lam,k, n){
  
  u<-runif(n)
  
  x<-c()
  
  for(i in 1:n){
    
    #u[i]
    x<-c(x,lam*((-log(1-u[i]))**(1/k)))
  }
  
  return(x[])
}

liczby <- gener_Weibulla(1,1, 5000)
hist(liczby , breaks= 40, probability = TRUE)


#...

#n ilosc generowanych
#gam - parametr gamma skali
#m - parametr położenia
gener_cauchy <- function(gam,m, n){
  
  u<-runif(n)
  
  x<-c()
  
  for(i in 1:n){
    
    #u[i]
    x<-c(x,gam*tan(pi*(u[i]-1/2))+m)
  }
  
  return(x[])
}

liczby <- gener_cauchy(1,0.05, 5000)
hist(liczby , breaks= 40, probability = TRUE)


#zad 2

#.

#u1, u2 - tablice o tej samej długosci z niezaleznymi rozkładami normalnymi
gener_nor1 <- function(u1,u2){
  
  d<-length(u1)
  
  x<-c()
  
  for(i in 1:d){
    
    #u[i]
    x<-c(x,sqrt(-2*log(u1[i]))*cos(2*pi*u2[i]))
  }
  
  return(x[])
}

gener_nor2 <- function(u1,u2){
  
  d<-length(u1)
  
  x<-c()
  
  for(i in 1:d){
    
    #u[i]
    x<-c(x,sqrt(-2*log(u1[i]))*sin(2*pi*u2[i]))
  }
  
  return(x[])
}

n<-5000

u1<-runif(n)

u2<-runif(n)

x1<-gener_nor1(u1,u2)

hist(z1,freq=FALSE )
x<- seq(-4,4, length.out = 1000)
wyk<- dnorm(x)
lines(x, wyk, type = "l", col='red', lwd= 2)
hist(x2)

#..

n<-1000

u1<-runif(n)
u2<-runif(n)

x1<-gener_nor1(u1,u2)
x2<-gener_nor2(u1,u2)

b1<- x1
b2<- x1+x2

c1<-(x1+x2)/sqrt(2)
c2<-(x1-x2)/sqrt(2)

plot(x1,x2)

plot(b1, b2)

plot(c1, c2)

#...

cor(x1,x2)
cor(b1,b2)
cor(c1,c2)

cor(u1,u2)

