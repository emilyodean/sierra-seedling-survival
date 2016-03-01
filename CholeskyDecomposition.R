#Creating fake data method 2 - MORE COMPLICATED - Using the Cholesky Decomposition

#Create a matrix of how we want our variables to be correlated (variance/covariance matrix)
#More dimensions can be added to this matrix for more explanatory variables
r = matrix(cbind(1,.80,
                 .80,1),nrow=2)

#Take the Cholesky decomposition
u = t(chol(r))

#How many variables we want (we could just specify two here)
nvars = dim(u)[1]

#Get our values from a random normal distribution, populate values for x and y
random.norm = matrix(rnorm(nvars*n,50,10), nrow=nvars, ncol=n); 

#For x, we multiply our u matrix by our values
x = u %*% random.norm;

#Put values in a data frame
data = as.data.frame(t(x));
names(data) = c("y","x");

#Check out the correlation matrix to see our final result
cor(data);

#Visualize
plot(data$y~data$x)