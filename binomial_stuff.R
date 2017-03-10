intercept = 0
beta = 0.5
xtest = rnorm(1000,1,1)
linpred = intercept + (xtest * beta)
prob = exp(linpred)/(1 + exp(linpred))
runis = runif(1000,0,1)
ytest = ifelse(runis < prob,1,0)



c = rep(0:1,each=500)
x = rnorm(1000)
lp = -3 + 2*c*x
link_lp = exp(lp)/(1 + exp(lp))
y = (runif(1000) < link_lp) 

log.int = glm(y~as.factor(c)*x, family=binomial)
summary(log.int)
mycols = c("red","blue")
plot(log.int$fitted.values ~ x, col=mycols)


#Create 30*27 binomial variables (response)

response = rbinom(size=1, n=810, prob=.5)
temp
precip
gdd




n = 10
beta0 = -1.6
beta1 = 0.03
x = runif(n=n, min=18, max=60)
pi_x = exp(beta0 + beta1 * x) / (1 + exp(beta0 + beta1 * x))
y = rbinom(n=length(x), size=1, prob=pi_x)
data = data.frame(x, pi_x, y)
names(data) <- c("temp", "pi", "mortality")
print(data)
