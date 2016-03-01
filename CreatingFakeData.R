####EXERCISE 1####
#Step 1 
n = 100
branch_diameter = rnorm(n, mean=10, sd=2)
branch_diameter

#Step 2 
num_cones = rnorm(n, mean=5, sd=1)

#Step 3 
model = lm(num_cones~branch_diameter)
summary(model)
plot(num_cones~branch_diameter)
abline(model, col="red")


####EXERCISE 2####
#Step 1 
intercept = 0
num_cones = intercept + 1.5*branch_diameter
plot(num_cones~branch_diameter, pch=20)

#Step 2
error = rnorm(n, mean=0, sd=3)
num_cones = intercept + 1.5*branch_diameter + error

#Step 3
model = lm(num_cones~branch_diameter)
summary(model)
plot(num_cones~branch_diameter, pch=20)
abline(model, col="red")

#Step 4
my_data = data.frame(num_cones, branch_diameter)
write.table(my_data, "dummy_data.csv")


####EXERCISE 3####
#Step 1
temperature = c(25, 27, 30)
moisture = c(.4, .7, .9)
gsd = c(122, 145, 170)

#Step 2
treatments = expand.grid(temperature=temperature,moisture=moisture,gsd=gsd)
treatments
ntreatments = nrow(treatments)

#Step 3
nreplicates = 30
mortality=rbinom(ntreatments*nreplicates, 1, .5)

#Step 4
my_data = data.frame(
    temp=rep(treatments$temperature, nreplicates), 
    moisture=rep(treatments$moisture, nreplicates), 
    growing_days=rep(treatments$gsd, nreplicates), 
    mortality=y,
    treatment=rep(1:ntreatments, nreplicates))

#Step 5
head(my_data)
write.table(my_data, "dummy_data2.csv")



####Other helpful commands####

#Select only a subset of your data 
#All rows where treatment is 1
treatment1 = subset(my_data, my_data$treatment==1)
treatment1

#All rows where mortality is 0
subset(my_data,my_data$mortality==0)

#All rows where mortality is 0 and treatment is 1
subset(my_data, my_data$mortality==0&my_data$treatment==1)

#Take a random sample from treatment 1 (perhaps we want to cut a random portion of them to determine biomass)
treatment1[sample(nrow(treatment1),5),]
