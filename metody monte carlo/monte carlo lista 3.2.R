#zad 1

#.
#mam zdjecie z uzasadnieniem zrobione 17.04.2024

#Y ~ (1-p)N (0, 1)+pN (?, ??)

wgenerator_Y<-function(n, p, mi, gam){
  
  n1 <- rnorm(n, mean=0, sd=1)
  n2 <- rnorm(n, mean=mi, sd=gam)
  
  x <- (1-p)*n1 + (p)*n2
  
  
  return(x[])
}


wgenerator_Y2<-function(n, p, mi, gam){
  u <- runif(n )
  
  x <- c()
  
  for( i in u){
    
    if(i<p){
      x<- c(x,rnorm(1, mean=mi,sd=(gam)**(1/2)))
    } else {
      x<- c(x,rnorm(1, mean=0,sd=1))
    } 
  }
  
  return(x[])
}


p <- 0.2
mi <- 3
gam <- 1


hist(wgenerator_Y2(10000, p, mi  ,gam), freq=FALSE )
x <- seq(-4, 6, by=0.01)
wyk <- ((1-p)*dnorm(x, mean=0,sd=1 ) + p*dnorm(x, mean=mi, sd=gam**(1/2)))*1
lines(x, wyk, type = "l", col='red', lwd= 2)


p <- 0.9
mi <- 10
gam <- 4


hist(wgenerator_Y2(10000, p, mi  ,(gam)), freq=FALSE )
x <- seq(-5, 27, by=0.01)
wyk <- ((1-p)*dnorm(x, mean=0,sd=1 ) + p*dnorm(x, mean=mi, sd=gam**(1/2)))*1
lines(x, wyk, type = "l", col='red', lwd= 2)


p <- 0.5
mi <- -2
gam <- 0.5


hist(wgenerator_Y2(10000, p, mi  ,gam), freq=FALSE )
x <- seq(-4, 4, by=0.01)
wyk <- ((1-p)*dnorm(x, mean=0,sd=1 ) + p*dnorm(x, mean=mi, sd=gam**(1/2)))*1
lines(x, wyk, type = "l", col='red', lwd= 2)


#..
# zwraca macierz gdzie x[i,1] odpowiada współrzednej x a x[i,2] odpowiada współrzednej y punktu i
gener1 <- function(n){
  
  
  
  licznik<- 0
  x <- c()
  y <- c()
  
  while (licznik<n){
    
    a <-runif(1)
    b <-runif(1)
    
    if ((a+b)<1){
      #print(a)
      #print(b)
      #print(a+b)
      #print(3)
      
      licznik <- licznik + 1
      
      x <- c(x,a)
      y <- c(y,b)
      
    }
    
  }
  
  u_matrix <- matrix(c(x,y), ncol=2)
  
  return(u_matrix)
}
yy <- gener1(10000)

#yy
plot(yy[,1], yy[,2])


# zwraca macierz gdzie x[i,1] odpowiada współrzednej x a x[i,2] odpowiada współrzednej y punktu i
gener2 <- function(n){
  
  
  
  licznik<- 0
  x <- c()
  y <- c()
  
  while (licznik<n){
    
    u1 <-runif(1)
    u2 <-runif(1)
    
    x2 <- u1**(1/2)
    y2 <- u2**(1/3)
    
    if ((x2+y2)<1){
      #print(a)
      #print(b)
      #print(a+b)
      #print(3)
      
      licznik <- licznik + 1
      
      x <- c(x,x2)
      y <- c(y,y2)
      
    }
    
  }

  
  u_matrix <- matrix(c(x,y), ncol=2)
  
  return(u_matrix)

}

yy <- gener2(10000)

#yy
plot(yy[,1], yy[,2])

A <- matrix(c(2,2,0,2,5,-3,0,-3,9), ncol=3)

A

B <- t(chol(A))

Bt <- t(B)



C <- B%*%Bt

C










gener_wielow_norm <- function(mean, cov_matrix, num_samples) {
  
  B <- t(chol(cov_matrix))
  
  
  z <- matrix(rnorm(length(mean) * num_samples), nrow = length(mean))
  
  
  samples <- matrix(mean, ncol=num_samples, nrow=length(mean)) + B %*% z
  
  return(samples)
}


A <- matrix(c(2,2,0,2,5,-3,0,-3,9), ncol=3)

eigen(A)


eigenvalues <- eigen(A)$values

# Sprawdzenie czy wszystkie wartości własne są dodatnie
all_positive_eigenvalues <- all(eigenvalues > 0)
all_positive_eigenvalues

x <- gener_wielow_norm(mean = c(1,-1,0), cov_matrix = matrix(c(2,2,0,2,5,-3,0,-3,9), ncol=3), num_samples = 29 )
x

s<-gener_wielow_norm(mean = c(-1,0), cov_matrix = matrix(c(5,-3,-3,9), ncol=2), num_samples = 2900 )
x2<-s[1,]
x3<-s[2,]
plot(x2,x3)


s<-gener_wielow_norm(mean = c(1,0), cov_matrix = matrix(c(2,0,0,9), ncol=2), num_samples = 2900 )
x1<-s[1,]
x3<-s[2,]
plot(x1,x3)



s<-gener_wielow_norm(mean = c(1,-1), cov_matrix = matrix(c(2,2,2,5), ncol=2), num_samples = 2900 )
x1<-s[1,]
x2<-s[2,]
plot(x1,x2)



x1<-gener_wielow_norm(mean = c(1), cov_matrix = matrix(c(2), ncol=1), num_samples = 2900 )
hist(x1)

x2<-gener_wielow_norm(mean = c(-1), cov_matrix = matrix(c(5), ncol=1), num_samples = 2900 )
hist(x2)

x3<-gener_wielow_norm(mean = c(0), cov_matrix = matrix(c(9), ncol=1), num_samples = 2900 )
hist(x3)

#zad 2

#.


I <-0
n <- 1000

fun <- function(n){
  I <-0
  
  
  for (i in 1:n){
    u <- runif(1)
    
    I <- I + exp(-u**2)
    
    
  }
  
  I <- I/n
  
  return(I)
  
  }

# I: jest to przyblizenie wartości całki


# wartość obliczona przez internetowy kalkulator :
#0.7468241328124270253994674361318530053544996868126063290276544989...


# n= 10
n <-10
c <- c()

for (i in 1:10000){
  
  c <-c(c, fun(n))
}



var(c)
mean(c)


# n= 100
n <-100
c <- c()

for (i in 1:10000){
  
  c <-c(c, fun(n))
}



var(c)
mean(c)


# n= 1000
n <-1000
c <- c()

for (i in 1:10000){
  
  c <-c(c, fun(n))
}



var(c)
mean(c)



# n= 10000
n <-10000
c <- c()

for (i in 1:10000){
  
  c <-c(c, fun(n))
}



var(c)

mean(c)



n <-1000
c <- c()

for (i in 1:1000){
  
  c <-c(c, fun(n))
}


confint(c)
#..



fun <- function(n){
  I <-0
  
  
  for (i in 1:n){
    x <- rexp(1, rate=1)
    
    I <- I + (x**5)*cos(x)
    
    
  }
  
  I <- I/n
  
  return(I)
  
}

fun(100000000)

#...

mandelbrot <- function(a,b){
  
  c <-0
  d <- 0
  czy_nalezy <- 0
  i <- 0
  wynik <- 0
  
  #print(c)
  #print(a)
  
 
    
    #print(c)
    
    
  while( i <= 1000 &  wynik < 2  ){
    
    x <- c^2 - d^2 +a
    y <- 2*d*c + b
    
    c<-x
    d <-y
    #print(c)
    #print(d)
    #print(i)
    
    
    i <- i +1
    
    wynik <- (c^2 + d^2)^(1/2)
    #print("wynik")
    #print(wynik)
    
    
    
  }
    
    
  
  
  wynik <- (c^2 + d^2)^(1/2)
  #print(wynik) 
  
  czy_nalezy <- 0
  
  #print(c)
  if( wynik < 2){
    czy_nalezy <- 1
  }
    
  
  return( czy_nalezy)
  
 }

  
  
  
  
  
  
  
  
  
  

fun <- function(n){
  I <-0
  
  
  for (i in 1:n){
    x <- runif(1,min=-2, max=1)
    y <- runif(1,min=-1, max=1)
    
    #print(x)
    #print(y)
    
    I <- I + 6*mandelbrot(x,y)
    
    
  }
  
  I <- I/n
  
  #print("bbbbbbbbbb")
  return(I)
  
}

#print("aaaaaaaaaa")

fun(1000)

mandelbrot(0,0)
#zad 3

#..

sum <- 0
p<-0.9



n <- 1000000


for (i in 1:n){
  
  x <- rbinom(1,size=1, prob=p)
  
  sum <- sum + x 
  
}

sum<- sum/n

sum

#...

#P(U1 + . . . + Un > 0.5n + 0.1√n)

sprawdzanie <- function(n){
  
  h <- 10000
  
  
  x <- 0

  for (i in 1:h){
    
    u <- runif(n)
    
    if ( sum(u) > 0.5*n +0.1*n**(1/2)){
      
     x<- x+1
    }
    
  }
  
  return(x/h)
  
}

#wartość z tablic : 3,669

sprawdzanie(10)

sprawdzanie(100)

sprawdzanie(1000)

sprawdzanie(10000)


sprawdzanie(100000)

sprawdzanie(1000000)

sprawdzanie(10000000)


















