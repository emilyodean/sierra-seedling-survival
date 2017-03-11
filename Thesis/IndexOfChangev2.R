library("sqldf")
library("XLConnect")
library("ggplot2")

################## Setup #################

#Set working directory
setwd("/Users/emilyodean/CodeProjects/Ecological Modeling/Thesis")

#Utilities
if(!exists("multiplot", mode="function")) source("util.R")

#Connect to database
db = dbConnect(SQLite(), dbname="RFWFv1.sqlite")
albertdb = dbConnect(SQLite(), dbname="Albertv1.sqlite")

#Overstory
overstory2016 = dbGetQuery(db,"select * from Overstory where region like 'Yosemite' and band!=1 and [DBH..cm.] > 10")
aoverstory = dbGetQuery(albertdb,"select * from Overstory")

#Seedlings
#Just yose again
understory =  dbGetQuery(db,"select * from Regeneration where band!=1 and (substr([Plot.St],1,2) like 'TR' or substr([Plot.St],1,2) like 'GP')")
aunderstory = dbGetQuery(albertdb,"select * from Sapling")

names(aunderstory)[1] = "Plot.St"
names(aunderstory)[6] = "Count"
plotsbands = unique(aoverstory[, c('Plot.St', 'Band')])
aunderstory = merge(aunderstory, plotsbands, by="Plot.St", all=FALSE)


################## Helper Functions ##################
#Calculate relative density of seedlings by plot
calculateSeedlingRdByPlot = function(seedlings, overstory, species, year, weighted)
{
    if(year == 1981)
    {
        seedsmult = (10000/120)
        treesmult = (10000/1200)
    } else
    {
        seedsmult = 100
        treesmult = 10
    }
    
    rds = vector(mode="numeric", length=0)
    totalRds = vector(mode="numeric", length=0)
    
    plotList = vector(mode="character", length=0)
    band = vector(mode="character", length=0)
    
    for(plot in unique(seedlings$Plot.St))
    {
        seeds = seedsmult * sum(subset(seedlings, (seedlings$Species == species & seedlings$Plot.St==plot))$Count, na.rm=TRUE) #Sum all seedlings in specified plot of species type
        #trees = treesmult * nrow(subset(overstory, (overstory$Plot.St==plot & overstory$Species == species)))#Sum all trees in specified plot of said species
        trees = treesmult * nrow(subset(overstory, (overstory$Plot.St==plot & overstory$Species == species & overstory$Size.Class==4)))
        
        rds = append(rds, seeds/trees)
        
        totalofspecies = seeds+trees
        totalinplot =  (sum(subset(seedlings, (seedlings$Plot.St==plot))$Count, na.rm=TRUE))*seedsmult + ((nrow(subset(overstory, (overstory$Plot.St==plot))) * treesmult))
        
        totalRds = append(totalRds, totalofspecies/totalinplot)
        
        plotList = append(plotList, plot)
        band = append(band, subset(seedlings, seedlings$Plot.St==plot)$Band[1])
    }
    
    
    
    output = sapply(rds,function(x){ifelse(any(x>1), (x=1/x), x)})
    #output = rds
    
    #Check the flag
    if(weighted==TRUE) {output=output*totalRds}
    
    return(data.frame(plot = plotList, band = band, rds = output, species = species))
}

#Caclulate the average and standard error for each band and put it all in a dataframe. Useful for graphing.
calculateBandAveragesAndSE = function(wfdf, rfdf)
{   
    df = rbind(wfdf, rfdf)
    df[is.na(df)] = 0
    
    means = aggregate(rds~band+species,data=df,mean)
    se = aggregate(rds~band+species,data=df,(function(x){sqrt(var(x)/length(x))}))[3]
    
    rfwf =data.frame(means, se)
    colnames(rfwf) = c("band", "species", "ratio", "se")
    return(rfwf)
}

# wfseedling = calculateSeedlingRdByPlot(aunderstory, aoverstory, "WF", 1981, FALSE)
# rfseedling = calculateSeedlingRdByPlot(aunderstory, aoverstory, "RF", 1981, FALSE)
# aunweighted = calculateBandAveragesAndSE(wfseedling, rfseedling)
# 
# wfseedling = calculateSeedlingRdByPlot(understory, overstory2016, "WF", 2016, FALSE)
# rfseedling = calculateSeedlingRdByPlot(understory, overstory2016, "RF", 2016, FALSE)
# unweighted = calculateBandAveragesAndSE(wfseedling, rfseedling)

awfseedling = calculateSeedlingRdByPlot(aunderstory, aoverstory, "WF", 1981, TRUE)
arfseedling = calculateSeedlingRdByPlot(aunderstory, aoverstory, "RF", 1981, TRUE)
aweighted = calculateBandAveragesAndSE(awfseedling, arfseedling)

wfseedling = calculateSeedlingRdByPlot(understory, overstory2016, "WF", 2016, TRUE)
rfseedling = calculateSeedlingRdByPlot(understory, overstory2016, "RF", 2016, TRUE)
weighted = calculateBandAveragesAndSE(wfseedling, rfseedling)

###### Wilcox ######
rfp = numeric(0)
wfp = numeric(0)
for(i in 3:5)
{   
    wfp = append(wfp, wilcox.test(subset(awfseedling, band==i)$rds, subset(wfseedling, band==i)$rds))
    rfp = append(rfp, wilcox.test(subset(arfseedling, band==i)$rds, subset(rfseedling, band==i)$rds))
}
wfp
rfp



#Calculate relative density for each size class by plot, return list of plots & rds
calculateRdByPlot = function(overstory, species, weighted)
{
    rds = vector(mode="numeric", length=0)
    totalRds = vector(mode="numeric", length=0)
    
    plotList = vector(mode="character", length=0)
    band = vector(mode="character", length=0)
    
    for(plot in unique(overstory$Plot.St))
    {
        rds = append(rds, nrow(subset(overstory, (overstory$Species == species & overstory$Size.Class==2 & overstory$Plot.St==plot)))/
                         nrow(subset(overstory, (overstory$Plot.St==plot & overstory$Species == species & overstory$Size.Class==4))))
        totalRds = append(totalRds, nrow(subset(overstory, (overstory$Species == species & overstory$Plot.St==plot)))/
                              nrow(subset(overstory, overstory$Plot.St==plot)))
        
        plotList = append(plotList, plot)
        band = append(band, subset(overstory, overstory$Plot.St==plot)$Band[1])
    }
    
    
    #Convert to the reciprocal if the value exceeds 1
    rds = sapply(rds,function(x){ifelse(any(x>1), (x=1/x), x)})
    
    #Weighted by total relative density if flag==TRUE
    if(weighted==TRUE) {rds=rds*totalRds} 
       
    return(data.frame(plotList, band, rds, species))
}

################## Run the functions ##################

#Current
wfRatios = calculateRdByPlot(overstory2016, "WF", TRUE)
rfRatios = calculateRdByPlot(overstory2016, "RF", TRUE)

#Albert
awfRatios = calculateRdByPlot(aoverstory, "WF", TRUE)
arfRatios = calculateRdByPlot(aoverstory, "RF", TRUE)

#### Non-weighted ####
#Current
#wfRatios = calculateRdByPlot(overstory, "WF")
#rfRatios = calculateRdByPlot(overstory, "RF")

#Albert
#awfRatios = calculateRdByPlot(aoverstory, "WF")
#arfRatios = calculateRdByPlot(aoverstory, "RF")

df = calculateBandAveragesAndSE(wfRatios, rfRatios)
adf = calculateBandAveragesAndSE(awfRatios, arfRatios)

####### Wilcox tests #######
rfp = numeric(0)
wfp = numeric(0)
for(i in 3:5)
{   
    wfp = append(wfp, wilcox.test(subset(awfRatios, band==i)$rds, subset(wfRatios, band==i)$rds))
    rfp = append(rfp, wilcox.test(subset(arfRatios, band==i)$rds, subset(rfRatios, band==i)$rds))
}

# > rfp
# [1] 0.3926585 0.5788737 0.6623377
# > wfp
# [1] 0.7325469 0.1873780 0.1595120



#Close connection
dbDisconnect(db)
dbDisconnect(albertdb)
