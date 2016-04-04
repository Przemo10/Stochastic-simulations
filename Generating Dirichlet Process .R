#http://www.mimuw.edu.pl/~bmia/symstoch/Projekty/Procesy%20Dirichleta/opis.pdf
#czesc: Generator procesu Dirichlet

#Task 1
# probab wylicza wagi dla skonczonej liczby punktow w procesie Dirichleta
# trzeba ja zmodyfikowac tak zeby zamiast brac n, brala epsilon
# gdzie epsilon bliski zera (np 0.05) 
# a jako wynik zwracala tyle losowych wag ile akurat bedzie potrzeba
# zeby (1-sum(weights))=epsilon (pewnie jakims while-m, ale wazne zeby
# nie losowac wag od nowa tylko dokladac)
probab <- function(n,alpha)
{
  v <- rbeta(n,1,alpha)
  temp <- (1-v)
  temp <- cumprod(temp)
  temp <- c(1,temp[-n])
  weights <- v*temp
  sum(weights)
  return (weights)
}

# czesc 1
probab1 <- function(epsilon, alpha){
  sum_weights <-0
  weights <-0
  i<-1
  temp <- 1
  temp[1]<-1
  cum<-1
  while( 1- sum_weights> epsilon) 
  {v[i] <- rbeta(1,1, alpha)  
  cum<-cum*(1-v[i])
  temp[i+1] <- cum
  weights[i] <-v[i]*temp[i]
  sum_weights <- sum_weights+ weights[i]
  i <- i+1
  }
  return(weights)
}

#generowanie probki z pierwotnego rozkladu 
parent_sample <-rnorm(n)

#generating one observation from Dirichlet process (with parent distribution Gaussian)
obs <- function(weights)
{
nowe <- Inf
probka <- sample(c(parent_sample,nowe),1,replace = TRUE, prob = c(weights,(1-sum(weights))))
if (probka == c(Inf)) (probka <- rnorm(1))
return (probka)
}
#generating sample 
sample <- replicate(10,obs(weights)) 

#Task 2 
#jezeli bedziemy losowali z rozkladu dyskretnego (np. jednosnajnego na {1,2,..,10})
#to niektore elementy parent_sample beda sie powtarzaly
# trzeba przerobic ta funkcje tak, zeby skleic wagi odpowiadajace powtorzeniom 

#testowanie czy otrzymujemy wlasciwy rozklad (to jest to nad czym ja teraz pracuje)
sum(weights[-1])
plot(dbeta(kwantyle,1,3))
kwantyle <- seq(length = 100, from = 0, to = 1)


