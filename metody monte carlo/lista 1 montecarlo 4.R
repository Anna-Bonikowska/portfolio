

#zad 1

# .


f <- function(a,m,n){
  s<- c(1)
  
  for( i in 1:n){
    
    k<-s[length(s)]
    s<-c(s,(a*k)%%m)
  }
  
  xn<-s/m
  
  return(xn[])
  
}


#okres wynosi 37
f(19,37,50)

#..


generator1<-f(19,37,50)
generator2<-f(39373,2**31 -1,50)

zero <- rep(0,51)

plot(generator1,zero)

plot(generator2,zero)

hist(generator1)
hist(generator2)

#...

parzyste <- function(x){
  
  k<-length(x)
  h<-x[2]
  a<-c(h)
  
  for(i in 2:(k/2)){
    a<-c(a,x[2*i])
  }
  
  return(a[])
}


nieparzyste <- function(x){
  
  k<-length(x)
  h<-x[1]
  a<-c(h)
  
  for(i in 2:(k/2)){
    a<-c(a,x[2*i - 1])
  }
  
  return(a[])
}

generator1<-f(19,37,50)
generator2<-f(39373,2**31 -1,50)

plot(parzyste(generator1),nieparzyste(generator1))
plot(parzyste(generator2),nieparzyste(generator2))


#zad 2

#.

los <- runif(1000)

plot(nieparzyste(los), parzyste(los))

#..

spr_rozk2 <- function(x,y){
  
  
  k<-length(x)
  h<- length(y)
  
  
  a<-0
  b<-0
  c<-0
  d<-0
  e<-0
  f<-0
  g<-0
  h<-0
  i<-0
  
  
  for(i in 1:k){
    
    for(j in 1:h){
      j
      #if(TRUE){
      #if( ((x[i]<(1/3)) & (y[j]<(1/3)))){
      if( x[j]<(1/3) & y[j]<(1/3)){
        a<-a+1
      }
      if( (x[i]<(1/3)) & (y[j]>(1/3)) & (y[j]<(2/3)) ){
        b<-b+1
      }
      if( (x[i]<(1/3)) &  (y[j]>(2/3)) ){
        c<-c+1
      }
      ####
      if( (x[i]>(1/3)) & (x[i]<(2/3)) & (y[j]<(1/3))){
        d<-d+1
      }
      if( (x[i]>(1/3)) & (x[i]<(2/3)) & (y[j]>(1/3)) & (y[j]<(2/3)) ){
        e<-e+1
      }
      if( (x[i]>(1/3)) & (x[i]<(2/3)) &  (y[j]>(2/3)) ){
        f<-f+1
      }
      #
      if( (x[i]>(2/3)) & (y[j]<(1/3))){
        g<-g+1
      }
      if( (x[i]>(2/3)) & (y[j]>(1/3)) & (y[j]<(2/3)) ){
        h<-h+1
      }
      if( (x[i]>(2/3)) &  (y[j]>(2/3)) ){
        i<-i+1
      }
      
    }
  }
  
  
  
  zzz<-c(a,b,c,d,e,f,g,h,i)
  
  return(zzz[])
  
}



x <- runif(1000)



 
spr_rozk2 <- function(x,y){
  
  
  k<-length(x)
  h<- length(y)
  
  
  a<-0
  b<-0
  c<-0
  d<-0
  e<-0
  f<-0
  g<-0
  h<-0
  i<-0
  
  
  for(i in 1:k){
    
    for(j in 1:h){
      j
      #if(TRUE){
      #if( ((x[i]<(1/3)) & (y[j]<(1/3)))){
      if( x[j]<(1/3) & y[j]<(1/3)){
        a<-a+1
      }
      if( (x[i]<(1/3)) & (y[j]>(1/3)) & (y[j]<(2/3)) ){
        b<-b+1
      }
      if( (x[i]<(1/3)) &  (y[j]>(2/3)) ){
        c<-c+1
      }
      ####
      if( (x[i]>(1/3)) & (x[i]<(2/3)) & (y[j]<(1/3))){
        d<-d+1
      }
      if( (x[i]>(1/3)) & (x[i]<(2/3)) & (y[j]>(1/3)) & (y[j]<(2/3)) ){
        e<-e+1
      }
      if( (x[i]>(1/3)) & (x[i]<(2/3)) &  (y[j]>(2/3)) ){
        f<-f+1
      }
      #
      if( (x[i]>(2/3)) & (y[j]<(1/3))){
        g<-g+1
      }
      if( (x[i]>(2/3)) & (y[j]>(1/3)) & (y[j]<(2/3)) ){
        h<-h+1
      }
      if( (x[i]>(2/3)) &  (y[j]>(2/3)) ){
        i<-i+1
      }
      
    }
  }

  
  
  zzz<-c(a,b,c,d,e,f,g,h,i)
  
  return(zzz[])
  
}


spr_rozk <- function(x,y){
  
  kx<-length(x)
  ky<-length(y)
  
  a<-0
  b<-0
  c<-0
  d<-0
  
  
  for(i in 1:kx){
    for( j in 1:ky){
      if( x[i]<(1/2) & y[j]<(1/2)){
        a<-a+1
      }
      
      if( x[i]>(1/2) & y[j]<(1/2)){
        b<-b+1
      }
      
      if( x[i]<(1/2) & y[j]>(1/2)){
        c<-c+1
      }
      
      if( x[i]>(1/2) & y[j]>(1/2)){
        d<-d+1
      }
      
    }
    
    
    
  }
  return(c(a,b,c,d))
}

spr_rozk(parzyste(x),nieparzyste(x))

#...

#dzieki tej funkcji za każdym razem otzrymujemy te same wyniki
set.seed(2)
e<-runif(50)


#zad 3

#.


# p- prawdobienstwo z przedziału [0,1]
#n ilosc generowanych liczb
generator_dwupunktowy<-function(p,n){
  
  x<-c()
  
  y<-runif(n)
  
  for(i in 1:n){
    if(y[i]<=p){
      x<-c(x,1)
    }else{
      x<-c(x,0)
      
      
    }
  }
  
  return(x[])
  
}
generator_dwupunktowy(0.83567,20)

#..

generator_dwumianowy<- function(p,n, ilosc){
  
  
  x<-generator_dwupunktowy(p,ilosc)
  
  
  
  
  for(i in 2:n){
    x<- x+ generator_dwupunktowy(p,ilosc)
  }
    
  
  return(x[])
}
generator_dwumianowy(0.2,9,1000)

hist(generator_dwumianowy(0.5,40,10000))

#...


#lam - lambda parametr który zmieniamy w tym rozkładzie
# ilość generowanych liczb
generator_poison <- function(lam, n){
  
  e<- exp(-lam)
  
  p<-e
  
  S <- p
  
  x<-runif(n)
  
  wynik<-c()
  
  for(i in 1:n){
    
    
    p<- e
    S <- p
    k<- 0
    
    
    while(S<x[i]){
      p<- p*(lam/(k+1))
      S<-S+p
      k<- k+1
    }
    
    wynik<-c(wynik,k)
    
    
  }
  
  return(wynik[])
  
  
  
  
}

generator_poison(10,100)

hist(generator_poison(10,1000))

































