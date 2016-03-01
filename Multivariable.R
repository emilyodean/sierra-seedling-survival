#Multivariable problems

#Let's add another variable in. We know that we are also collecting data on species,
#and we hypothesize that Pinus sabiniana will only produce 1 more cone per inch increase, whereas
#Pinus attenuata will produce 3 more cones per inch increase. We also know that Pinus sabiana has 
#historically produced 40 cones per year at the historical branch_diameter, while Pinus radiata has produced 60.
#Finally, we also have information that there's more variability in Pinus radiata than Pinus sabiniana. 

branch_diameter = rnorm(n, mean=10, sd=2)

pinus_s = branch_diameter[1:50]
pinus_a = branch_diameter[51:100]

#Create our response, number of cones, as a function of the information above, with some error.
num_cones_s = 40 + 1*pinus_s + rnorm(n/2, mean=0, sd=8)
num_cones_a = 60 + 3*pinus_a + rnorm(n/2, mean=0, sd=12)

dataset = data.frame("Species"=append(rep("sabiniana",50), rep("attenuata", 50)), "branch_diameter"=append(pinus_s,pinus_a), "NumCones"=append(num_cones_s,num_cones_a))

#Time for some data visualization
plot(dataset$branch_diameter, dataset$NumCones, pch=21, bg=c("blue","green3")[unclass(dataset$Species)])

#Take a subset of the dataset for each species
sabiniana = dataset[which(dataset$Species=="sabiniana"),]
attenuata = dataset[which(dataset$Species=="attenuata"),]

#Plot lines for regressing each species separately, and plot the line for the regression of both species
abline(lm(sabiniana$NumCones ~ sabiniana$branch_diameter, data=sabiniana)$coefficients, col="green3")
abline(lm(attenuata$NumCones ~ attenuata$branch_diameter, data=attenuata)$coefficients, col="blue")
abline(lm(dataset$NumCones ~ dataset$branch_diameter, data=dataset)$coefficients, col="black")

#Write to an excel file
install.packages("xlsx")
library("xlsx")
write.xlsx(dataset, file="multivariable_data.xlsx")



