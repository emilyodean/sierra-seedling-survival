library("sqldf")
library("XLConnect")
library("xlsx")
library("ggplot2")
library("nls2")

################## Setup #################

#Set working directory
setwd("/Users/emilyodean/CodeProjects/Ecological Modeling/Thesis")

#Utilities
if(!exists("multiplot", mode="function")) source("util.R")

#Connect to database
db = dbConnect(SQLite(), dbname="RFWFv1.sqlite")

#for now just get GP & Core data
gp = dbGetQuery(db,"select * from Overstory where site like 'GP' or site like 'CT' or site like 'HC'")

dbListFields(db, "Overstory")

cores = read.xlsx("/Users/emilyodean/Documents/Research Data/Core Data/CORES.xlsx", sheet=1)

# This model will be used to predict ages of all uncored trees - DOESN'T WORK for small data sets
createCanopyClassModel = function(cc)
{
    df = subset(cores, CanopyClass == cc & DCH != 1) 
    #x = data$DBH
    #y = data$Age
    #model = lm(data$Age ~ dbh + I(dbh^2))
    #model = nls2(y ~ SSlogis(x, Asym, xmid, scal), data = data.frame(x, y), all=TRUE, trace=TRUE)
    model = nls2(Age ~ SSlogis(DBH, Asym, xmid, scal), data = df, all=TRUE)
    plot(df$Age ~ df$DBH)  

    lines(sort(df$DBH), fitted(model)[order(df$DBH)], col='red') 
    summary(model)
    
    return(model)
}

plot(x,y)


#Testing since the function above won't converge
#model = nls(y ~ SSlogis(log(x), Asym, xmid, scal), data = data.frame(x, y))
#model =  nls(y~a/(1 + exp(-b * (x-c))), start = list(a = 140, b = 1.4, c=130))
#m1 = nls2(y ~ a + b * I(x^z), start = list(a = 1, b = 1, z = 1), algorithm="brute-force", trace=TRUE)

#Create a model for each canopy class - DOESN'T WORK for S, C, D - not enough data points
smodel = createCanopyClassModel("S")
imodel = createCanopyClassModel("I")
cmodel = createCanopyClassModel("C")
dmodel = createCanopyClassModel("D")

#Predict age for trees w/o core data
predictAgeFromDBH = function(model, cc)
{
    sub = subset(gp, gp$Canopy.Class == cc)
    
    #For predict, it matches on the name of the predictor used in the model
    names(gp)[6] = "DBH"
    
    predicted = data.frame(age=predict(model, sub, type="response"))
    predicted_values = data.frame(age=predicted, dbh=sub$DBH, species=sub$Species)
    
    #This will just be a sigmoid curve
    plot(predicted_values$age ~ predicted_values$dbh)    
    
    return(predicted_values)
}

spredictions = predictAgeFromDBH(smodel, "S") 
ipredictions = predictAgeFromDBH(imodel, "I")
cpredictions = predictAgeFromDBH(cmodel, "C")
dpredictions = predictAgeFromDBH(cmodel, "D")

# function needed for visualization purposes
sigmoid = function(params, x) {
    params[1] / (1 + exp(-params[2] * (x - params[3])))
}


################ Climate #################

#Load in yosemite climate data, use only date, average summer temp, growing season, and MAP
yose_climate = xlsx::read.xlsx(file="/Users/emilyodean/Documents/Research Data/Historic Climate Data/Yose_Climate_WNA.xlsx", sheetIndex=1)
yose_climate_abbr = data.frame(Year = substr(yose_climate$X.period, 6, 10), Avg_Summer = yose_climate$Tave_sm., GDD = yose_climate$FFP. , MAP = yose_climate$MAP.)

#TODO - add all predictions into data frame
#predictions = 
predictions = data.frame(age = round(ipredictions$age), dbh = ipredictions$dbh, firstyear=round(2016-ipredictions$age), species=ipredictions$species)

#Number established per year
#TODO - Add tree data from predicted values
treesperyear = as.data.frame(table(predictions$firstyear, predictions$species))
names(treesperyear) = c("Year", "Species", "Count")

#Merge the climate data with the tree data
#This will only return all of the years for which there is climate data. 
#To keep all of the trees, use all.x = TRUE as an argument to merge.
treesperyear = merge(treesperyear, yose_climate_abbr, by="Year")

#Fit a model to see which climate variables can predict the establishment of each species
fit = glm(Count~Avg_Summer*GDD*MAP*Species,data=treesperyear,family=poisson())

plot(Count~Year, data=treesperyear)
hist(predictions$firstyear)

#Close connection
dbDisconnect(db)







#### TESTING ####
y = data$Age
x = data$DBH
# fitting code
fitmodel = nls(y~a/(1 + exp(-b * (x-c))), start=list(a=1,b=.5,c=25))

# visualization code
# get the coefficients using the coef function
params=coef(fitmodel)

y2 <- sigmoid(params,data$DBH)
plot(y2,type="l")
points(y)

plot(y ~ x)
fit = nls(y ~ SSlogis(x, Asym, xmid, scal), data = data.frame(x, y))

summary(fit)
lines(sort(data$DBH), fitted(fit)[order(data$DBH)], col='red')

lines(seq(0.5, 4, length.out = 100), 
      predict(fit, newdata = data.frame(x = seq(0.5, 4, length.out = 100))))
################