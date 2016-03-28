# Zad 1
# Zaimplementuj algorytm Boxa-Mulera przyklad 3.4 ze skryptu. 
# Porownaj efektywnosc swojej  funkcji z rnorm

norm <- function(sample.size = 1, mean = 0, var = 1)
{
# generate iid gaussian random variables
  X <- runif(ceiling(sample.size/2))
  Y <- runif(ceiling(sample.size/2))
  theta <- 2*pi*X
  R <- sqrt(-2*log(Y))
  X <- R*cos(theta)
  Y <- R*sin(theta)
  sample <- c(X,Y)
  length(sample) <- sample.size
  return (sample)
}

#normality test

test <- norm(1000)
hist(test)
#install.packages("nortest")
library(nortest)
shapiro.test(test)
lillie.test(test)
ad.test(test)

#effectivity test

system.time(rnorm(10^7))
system.time(norm(10^7))

# Zad 2
#Zaimplementuj algorytm generowania z rozkladu Cauchyego
#i porownaj efektywnosc z funkcja rcauchy

cauchy <- function(sample.size = 1, location = 0, scale = 1)
{
  X <- runif(sample.size)
  X <- tan(pi*(X-1/2))
  X <- (scale*X + location)
  return (X)
}

# test if distribution is cauchy
test <- cauchy(1000)
quantile(test,0.05)
qcauchy(0.05)

# effectivity test

system.time(rcauchy(10^6))
system.time(cauchy(10^6))

# Zad 3
# Zaimplementuj algorytm Bermana
# i porownaj z odpowiednia funkcja w R

beta <- function(alpha, beta)
{
  #alpha > 0, Beta > 1)
  sample <- vector("numeric",0)
  while ((length(sample))< 1)
{
  U <- runif(1)
  V <- runif(1)
  X <- (U^(1/alpha))
  Y <- (V^(1/(beta-1)))
  sample <- X[X+Y < 1]
}
return (sample)
}

# Test if we have correct Beta distribution
# if x ~ Beta(alpha, beta) them EX = alpha/(alpha + beta)
test <- replicate(100000,beta(3,3))
mean(test)
quantile(test,0.05)
qbeta(0.05,3,3)
hist(test)
ks.test(test,pbeta(100000,3,3))

# effectivity test 
# big difference
system.time(rbeta(10^6,3,3))
system.time(replicate(10^6,beta(3,3)))

# zad 4
# a) Korzystajac z rozkladu Choleskiego macierzy kowariancji 
#    napisz generator wielowymiarowego rozkladu normalnego
cnorm <- function(covariance , mean = 0)
{
  sample <- rnorm(nrow(covariance))
  sample <- (t(chol(X)) %*% sample)
  sample <- (sample + mean)
  return (sample)
}

# test poprawnosci
X <- matrix(c(2,1,1,2),2,2)
mean(replicate(1000,cov(cnorm(X))))
shapiro.test(replicate(1000,cnorm(X)))


#  b)Korzystajac z rozkladu SVD macierzy kowariancji 
#   napisz generator wielowymiarowego rozkladu normalnego

#install.packages("Matrix")
library("Matrix")

snorm <- function(covariance , mean = 0)
{
  sample <- rnorm(nrow(covariance))
  D <- as.data.frame(svd(covariance)[1])
  D <- as.matrix(D)
  D <- Diagonal(nrow(covariance),D)
  D <- as.matrix(D)
  D <- sqrt(D)
  U <- as.matrix(as.data.frame(svd(covariance)[2]))
  J <- ( U %*% D )
  sample <- (J %*% sample)  
  sample <- (sample + mean)
  return (sample)
}

# test poprawnosci
X <- matrix(c(2,1,1,2),2,2)
b <- snorm(X,1)
mean(replicate(1000,cov(snorm(X))))
shapiro.test(replicate(1000,snorm(X)))

#c) Ktorego algorytmu mozna uzywac?? jezeli macierz kowariancji jest osobliwa?
# SVD

# d) Ktory algorytm jest efektywniejszy?
# Cholesky (slow), SVD (terribly slow)
system.time(replicate(10^5,cnorm(X)))
system.time(replicate(10^5,snorm(X)))




