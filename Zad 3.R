# Exercise 1
# Write a function generating uniform distribution 
# on a n-dimensional ball o radius r


#Generate random sample of size m

runifballs <- function(r = 1, n = 2, m = 1)
{
  runifball <- function( r = "radius", n = "dimension")
  {
    X <- 100
    while ((sum(X^2)) > r^2) 
    {
      X <- r*(2*runif(n)-1)
      
    }
    return (X)
  }  
  sample <- t(replicate(m, runifball(r,n)))
}



### testing
kaj <- runifballs(2,10,1000)
sum(kaj[4,]^2)
kaj[1,]
plot(kaj)
a <- runifballs(r = 2,m = 10000)
a
hist(a[,1])
plot(a)
dim(a)


# Exercise 2
# Generate random sample from uniform distribution on elipsa
# that is on a set of solutions to equation t(x)*A*x
# where x is a vector, and A is symmetric, positive definite

runifelipses <- function(A, m = 1)
{
  runifelisa <- function(A)
  {
    X <- (vevtor("numeric",nrow(A))+ 10^10)
    while (t(X) %*% A %*% > 1) 
    {
      X <- (2*runif(n)-1)
    }
    return (X)
  }  
  sample <- t(replicate(m, runifball(r,n)))
}

X <-  matrix(c(1,4,2,3),2,2)
eigval(x)
??eival
is.positive.definite(X)

nrow(X)
t(X)
a <- (X == t(X))
X %*% X

identical(X,t(X))

runifelipsa <- function(A)
{
  
  test <- 0
  while (test == 0) 
  {
    X <- ((2*runif(nrow(A))-1))
    print(X)
    class(X)
    print(A)
    class(A)
    B <- A
    Y <- X
    print(B)
    tescik <- (t(Y) %*% B %*% Y)
    if ( tescik < 1) (test <- 1) else (test <- 0)
  }
  return (X)
}

A <- matrix(c(2,1,1,2),2,2)
runifelipsa(A)
X
rm(X)
X
X <- c(1,2)
t(X) %*% A %*% X
X <- vector("numeric",nrow(A))
X
X %*% A
t(X) %*% A
X
X <- (X+10)
X
t(X) %*% A
(t(X) %*% A %*% X)  > 1 
class(X)
X
