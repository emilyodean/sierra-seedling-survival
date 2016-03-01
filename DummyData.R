####EXERCISE 1####

#Does height of a tree predict its volume?
#volume(cu ft) ~ height(ft)

#Step 1 - generate X, assuming we're drawing from a normal distribution
#### HINT: If you ever have a question about a function in R, you can learn 
####       about it by typing "?function_name" in the console. 
####       So, if we want to know more about "rnorm," we type "?rnorm"

n = 100

height = rnorm(n, mean=50, sd=10)
height = sample(1:100, size=n, replace=TRUE)

#Step 2 - generate y variable

volume = rnorm(n, mean=50, sd=10)

#Step 3 - fit a model
model = lm(volume~height)

#Step 4 - examine coefficients, significance
summary(model)

#Step 5 - visualize
plot(volume~height)
abline(model, col="red")




####EXERCISE 2####
#Manipulating relationships between variables

#Method 1 - SIMPLE example
#Step 1 - We have our volume data, but let's say that we want 
#         volume and height to be highly correlated.

intercept = 0
error = rnorm(n, mean=0, sd=30)
volume = intercept + 1.5*height + error
volume = ifelse(volume>=0, volume, 0)

model = lm(volume~height)
summary(model)
plot(volume~height)
abline(model, col="red")


#Method 2 - MORE COMPLICATED - Using the Cholesky Decomposition
#Create a matrix of how we want our variables to be correlated
R = matrix(cbind(1,.80,
                 .80,1),nrow=2)

#Take the Cholesky decomposition
U = t(chol(R))

#How many variables we want (we could just specify two here)
nvars = dim(U)[1]

#Get our values from a random normal distribution, populate values for x and y
random.norm = matrix(rnorm(nvars*n,50,10), nrow=nvars, ncol=n); 

#For x, we multiply our R2 matrix by our values
X = U %*% random.norm;

#Put values in a data frame
raw = as.data.frame(t(X));
names(raw) = c("volume","height");
cor(raw);

plot(raw$volume~raw$height)



####EXERCISE 3####
#Multivariable problems

#Let's add another variable in. Height describes part of the variation in volume,
#but if we add in DBH we might get a better picture

DBH = rnorm(n, mean=20, sd=5)



####EXERCISE 4####

#Get into pairs, think of a simple question, and create a fake dataset
#### HINT: - rnorm, rbinom, runif will give you random numbers from these distributions
####       - round() will give you integers, rounding up (so, round(runif(50, min=0, max=99) will give me integers from 0 to 100))
####       - don't forget to use "?" to help you

#### Visualizing data
####       - once you've created your data, try making a graph