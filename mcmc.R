HMpriori <- function(N,theta1,theta2,phi) {
  cont <- 0
  n=15
  t=15962989
  t1=237217
  s=8
  theta <- matrix(nrow=N, ncol=4)
  for (j in 1:N) {
    eps <- rnorm(2, 0, phi)
    theta1.linha <- theta1+ eps[1]
    theta2.linha <- theta2 + eps[2]
    if (theta1.linha>1 && theta2.linha > 1) {
      a=(s+1)*(theta1 - theta1.linha)+ theta2- theta2.linha
      b=(-n*exp(theta2.linha)+n*t1- t)/exp(theta1.linha)
      c=(-n*exp(theta2)+n*t1- t)/exp(theta1)
      alpha <- min(exp(a + b - c), 1)
    }else {
      alpha <- 0
    }
    if( runif(1) < alpha) {
      theta1 <- theta1.linha
      theta2 <- theta2.linha
      cont <-  cont +1 
    }
    cat("Acceptation rate =", cont/j, "\n")
    theta[j,1] <- theta1
    theta[j,2] <- theta2
    theta[j,3] <- exp(theta1)
    theta[j,4] <- t1- exp(theta2)
  }
  return(theta)
}


phi=.7

theta <- HMpriori(N=100000, theta1= 14,theta2 = 1.1,phi)  #o nível de aceitação está entre 0.15 e 0.4
library(MCMCpack)
par(mfrow=c(2,2))
traceplot(mcmc(theta[seq(1,100000,100),1]),xlab="??")  
traceplot(mcmc(theta[seq(1,100000,100),2]),xlab="??2")  
traceplot(mcmc(theta[seq(1,100000,100),3]),xlab="??")  
traceplot(mcmc(theta[seq(1,100000,100),4]),xlab="??")  
#todos possuem um processo estacionário, o que era de interesse 
hist(mcmc(theta[seq(1,100000,100),1]),xlab="??",main="",ylab="Frequencia") #aparenta ter uma distribuição simétrica gaussiana
hist(mcmc(theta[seq(1,100000,100),2]),xlab="??2",main="",ylab="Frequencia") #com um decaimento rápido, parece uma distribuição exponencial
hist(mcmc(theta[seq(1,100000,100),3]),xlab="??",main="",ylab="Frequencia") #uma distribuição assimétrica 
hist(mcmc(theta[seq(1,100000,100),4]),xlab="??",main="",ylab="Frequencia") #com exceção de alguns dados dispersos, a maior concentração está em 238000 

summary(mcmc(theta[,1])) 
summary(mcmc(theta[,2])) 

HPDinterval(mcmc(theta[seq(1,100000,100),1])) 
HPDinterval(mcmc(theta[seq(1,100000,100),2]))

par(mfrow=c(2,2))
acf(mcmc(theta[seq(1,100000,100),1]),main="??",xlab="defasagens",ylab="FAC")
pacf(mcmc(theta[seq(1,100000,100),1]),main="??",xlab="defasagens",ylab="FACP")
acf(mcmc(theta[seq(1,100000,100),2]),main="??2",xlab="defasagens",ylab="FAC")
pacf(mcmc(theta[seq(1,100000,100),2]),main="??2",xlab="defasagens",ylab="FACP")

acf(mcmc(theta[seq(1,100000,100),3]),main="??",xlab="defasagens",ylab="FAC")
pacf(mcmc(theta[seq(1,100000,100),3]),main="??",xlab="defasagens",ylab="FACP")
acf(mcmc(theta[seq(1,100000,100),4]),main="??",xlab="defasagens",ylab="FAC")
pacf(mcmc(theta[seq(1,100000,100),4]),main="??",xlab="defasagens",ylab="FACP")

#defasagens nulas na Função de autocorrelação e autocorrelação parcial, significando ter comportamento ruído branco em todos os casos

