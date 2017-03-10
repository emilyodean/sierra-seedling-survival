n <- 10
beta0 <- -1.6
beta1 <- 0.03

x <- runif(n=n, min=18, max=60)
pi_x <- exp(beta0 + beta1 * x) / (1 + exp(beta0 + beta1 * x))
y <- rbinom(n=length(x), size=1, prob=pi_x)
data <- data.frame(x, pi_x, y)
names(data) <- c("age", "pi", "y")
print(data)



r = .8
sigma = matrix(c(1,r,r,1), ncol=2)
s = chol(sigma)
n = 100
z = s%*%matrix(rnorm(n*2), nrow=2)
u = pnorm(z)




sim.regression<-function(n.obs=10,coefficients=runif(10,-5,5),s.deviation=.1){
    
    n.var=length(coefficients)  
    M=matrix(0,ncol=n.var,nrow=n.obs)
    
    beta=as.matrix(coefficients)
    
    for (i in 1:n.var){
        M[,i]=rnorm(n.obs,0,1)
    }
    
    y=M %*% beta + rnorm(n.obs,0,s.deviation)
    
    return (list(x=M,y=y,coeff=coefficients))
    
}




library(MASS)
set.seed(100)
m = 3
n = 100
sigma = matrix(c(1,.4,.2,.4,1,-.8,.2,-.8,1), nrow=3)
z = mvrnorm(n, mu=rep(0,m), Sigma=sigma,empirical=T)
cor(z)

library(psych)
pairs.panels(z)
u = pnorm(z)
pairs.panels(u)
