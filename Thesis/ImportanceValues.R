#Importance values
library("sqldf")
library("XLConnect")
library("ggplot2")

#Set working directory
setwd("/Users/emilyodean/CodeProjects/Ecological Modeling/Thesis")

#Utilities
if(!exists("multiplot", mode="function")) source("util.R")

#Connect to database
db = dbConnect(SQLite(), dbname="RFWFv1.sqlite")
albertdb = dbConnect(SQLite(), dbname="Albertv1.sqlite")

dbListTables(albertdb)
dbListFields(albertdb, "PlotData")
data = dbGetQuery(db,"select Aspect, [Slope..] from Plot_Notes")

all_2016 = dbGetQuery(db,"select [Plot.St], Species, [DBH..cm.] as DBH, Band, Region, [Size.Class]
                      from Overstory
                      where Status!=4
                      and [DBH..cm.]>10
                      and Band!=1 and Band!=2 and Band!=5")

all_1981 = dbGetQuery(albertdb, "select a.[Plot.St], Species, [DBH..cm.] as DBH, a.Band, a.[Size.Class]
                      from Overstory a
                      join PlotData b
                      on (a.[Plot.St] = b.[Plot.St])
                      where Direction = 'N' and [DBH..cm.]!='NA' and a.Band!=5 and a.Band!=2")

BA_factor = 0.00007854

all_2016$BA = ((all_2016$DBH)^2)*BA_factor
all_1981$BA = ((all_1981$DBH)^2)*BA_factor

#Importance value - sum of relative density, frequency, and basal area per species
#Relative Density = # per HA/total HA
#Relative Frequency = number of plots w/ species present/total plots
#Relative Dominance = sum of BA/ total BA
# = x/300

yose = subset(all_2016, Region=="Yosemite")
stani = subset(all_2016, Region=="Stanislaus")


########Functions########
############################################################

calculateImportanceValueByPlot = function(data, sp, sizeClass)
{
    plots = character(0)
    bands = numeric(0)
    IVs = numeric(0)
    
    for(b in 1:5)
    {
        #We need to calculate relative frequency by plot because this is a measure of occurence in sampling units
        
        bandonly = subset(data, Band == b)
        rfrequency = length(unique(subset(bandonly, Species==sp)$Plot.St))/length(unique(bandonly$Plot.St)) * 100
        
        for(plot in unique(bandonly$Plot.St))
        {
            plotonly = subset(bandonly, Plot.St == plot)
            if(sizeClass!=10)
            {
                plotonly = subset(plotonly, Size.Class==sizeClass)
            }
            
            species_only = subset(plotonly, Species==sp)
            
            rdensity = nrow(species_only)/nrow(plotonly) * 100
            rdominance = sum(species_only$BA)/sum(plotonly$BA) * 100

            IVs = append(IVs, rdensity+rdominance+rfrequency)
            bands = append(bands, b)
            plots = append(plots, plot)
            
        }
    }
    
    return(data.frame(bands, plots, IVs))
}


calculateImportanceValue = function(data, sp, e_band, sizeClass)
{
    data = subset(data, Band==e_band)
    if(sizeClass!=10)
    {
        data = subset(data, Size.Class==sizeClass)
    }
    
    species_only = subset(data, Species==sp)
    
    rdensity = nrow(species_only)/nrow(data) * 100
    rdominance = sum(species_only$BA)/sum(data$BA) * 100
    rfrequency = length(unique(species_only$Plot.St))/length(unique(data$Plot.St)) * 100
    
    iv = rdensity+rdominance+rfrequency
    return(iv)
}

############################################################

#Run the functions 
############################################################

#Calculate importance values by band for each year and species
rfIV = numeric(0)
wfIV = numeric(0)
yoserfIV = numeric(0)
yosewfIV = numeric(0)
stanirfIV = numeric(0)
staniwfIV = numeric(0)
albertrfIV = c(0,0)
albertwfIV = c(0,0)

for(band in 1:5)
{
    if(band!=1 && band!=2)
    {
        albertrfIV = append(albertrfIV, calculateImportanceValue(all_1981, "RF", band, 10))
        albertwfIV = append(albertwfIV, calculateImportanceValue(all_1981, "WF", band, 10))
    }
    
    rfIV = append(rfIV, calculateImportanceValue(all_2016, "RF", band, 10))
    wfIV = append(wfIV, calculateImportanceValue(all_2016, "WF", band, 10))
    yoserfIV = append(yoserfIV, calculateImportanceValue(yose, "RF", band, 10))
    yosewfIV = append(yosewfIV, calculateImportanceValue(yose, "WF", band, 10))
    stanirfIV = append(stanirfIV, calculateImportanceValue(stani, "RF", band, 10))
    staniwfIV = append(staniwfIV, calculateImportanceValue(stani, "WF", band, 10))
}
df = data.frame(Band = 1:5,
                RFIV = rfIV, 
                WFIV = wfIV,
                yoserfIV,
                yosewfIV,
                stanirfIV,
                staniwfIV,
                albertrfIV,
                albertwfIV)

#Calculate importance values by Plot instead
rfIVByPlot = calculateImportanceValueByPlot(yose, "RF", 10)
names(rfIVByPlot)[3] = "rfIV"
wfIVByPlot = calculateImportanceValueByPlot(yose, "WF", 10)
names(wfIVByPlot)[3] = "wfIV"
ivs_2016 = merge(rfIVByPlot, wfIVByPlot)

rfIVByPlot1981 = calculateImportanceValueByPlot(all_1981, "RF", 10)
names(rfIVByPlot1981)[3] = "rfIV"
wfIVByPlot1981 = calculateImportanceValueByPlot(all_1981, "WF", 10)
names(wfIVByPlot1981)[3] = "wfIV"
ivs_1981 = merge(rfIVByPlot1981, wfIVByPlot1981)

#dbListFields(db, "Plot_Notes")
#dbListFields(db, "UL")
#dbListFields(albertdb, "PlotData")


##### Get averages and SE for each plot importance value, then run a wilcoxon test to determine differences
rfbandavg = aggregate(data = ivs_2016, rfIV~bands, FUN=mean)
rfbandse = aggregate(data = ivs_2016, rfIV~bands, FUN=function(x) sqrt(var(x)/length(x)))
wfbandavg = aggregate(data = ivs_2016, wfIV~bands, FUN=mean)
wfbandse = aggregate(data = ivs_2016, wfIV~bands, FUN=function(x) sqrt(var(x)/length(x)))

rfbandavg1981 = aggregate(data = ivs_1981, rfIV~bands, FUN=mean)
rfbandse1981 = aggregate(data = ivs_1981, rfIV~bands, FUN=function(x) sqrt(var(x)/length(x)))
wfbandavg1981 = aggregate(data = ivs_1981, wfIV~bands, FUN=mean)
wfbandse1981 = aggregate(data = ivs_1981, wfIV~bands, FUN=function(x) sqrt(var(x)/length(x)))

###### Wilcox ######
rfp = numeric(0)
wfp = numeric(0)
for(i in 3:4)
{   
    rfp = append(rfp, wilcox.test(subset(ivs_2016, bands==i)$rfIV, subset(ivs_1981, bands==i)$rfIV))
    wfp = append(wfp, wilcox.test(subset(ivs_2016, bands==i)$wfIV, subset(ivs_1981, bands==i)$wfIV))
}
wfp
rfp
####


####Put the importance data and plot data into a dataframe to assess importance values
# as a function of aspect, slope, UL
plotdata_2016 = dbGetQuery(db,"select a.[Plot.St] as plots, a.OpenCnpy, b.Aspect, b.[Slope..] as Slope, b.Elevation 
                from UL a
                join Plot_Notes b
                on (a.[Plot.St] = b.[Plot.St])")

ivs_2016 = merge(ivs_2016, plotdata_2016)
ivs_2016$Elevation = ivs_2016$Elevation*.3048

plotdata_1981 = dbGetQuery(albertdb, "select [Plot.St] as plots, Aspect, Slope, Elevation
                           from PlotData")
ivs_1981 = merge(ivs_1981, plotdata_1981)


#W/ understory light
rfmodel = glm(formula=rfIV~Elevation+OpenCnpy+Slope, data = ivs_2016, family=gaussian)
summary(rfmodel)
wfmodel = glm(formula=wfIV~Elevation+OpenCnpy+Slope, data = ivs_2016, family=gaussian)
summary(wfmodel)

#W/o understory light
rfmodel = glm(formula=rfIV~Elevation+Slope, data = ivs_2016, family=gaussian)
summary(rfmodel)
wfmodel = glm(formula=wfIV~Elevation+Slope, data = ivs_2016, family=gaussian)
summary(wfmodel)

#Just elevation
rfmodel = glm(formula=rfIV~Elevation, data = ivs_2016, family=gaussian)
summary(rfmodel)
wfmodel = glm(formula=wfIV~Elevation, data = ivs_2016, family=gaussian)
summary(wfmodel)

#1981 data
arfmodel = glm(formula=rfIV~Aspect+Elevation+Slope, data = ivs_1981, family=gaussian)
summary(arfmodel) 
awfmodel = glm(formula=wfIV~Aspect+Elevation+Slope, data = ivs_1981, family=gaussian)
summary(awfmodel)

arfmodel = glm(formula=rfIV~Elevation, data = ivs_1981, family=gaussian)
summary(arfmodel) 
awfmodel = glm(formula=wfIV~Elevation, data = ivs_1981, family=gaussian)
summary(awfmodel)


###### Regression of importance values versus year
ivs_1981$year = 1981
ivs_2016$year = 2016
all=rbind(ivs_1981, ivs_2016[,-5])
#plot(ivs_2016$Elevation, ivs_2016$rfIV)
#plot(ivs_1981$Elevation, ivs_1981$rfIV)

all$year = as.factor(all$year)
noband5 = subset(all, bands!=5)
ggplot(data=all, aes(x=Elevation, y=rfIV, group=year, colour=year)) +
    geom_point() +
    geom_smooth(method = "lm") + 
    xlab("Plot elevation (m)")+
    ylab("IV") +
    ggtitle("Abies magnifica importance values by plot versus elevation, >40cm dbh") 

ggplot(data=all, aes(x=Elevation, y=wfIV, group=year, colour=year)) +
    geom_point() +
    geom_smooth(method = "lm") + 
    xlab("Plot elevation (m)")+
    ylab("IV") +
    ggtitle("Abies concolor importance values by plot versus elevation, >40cm dbh") 


stacked = rbind(data.frame(plots=all$plots, bands=all$bands, iv=all$rfIV, aspect=all$Aspect, 
                           slope=all$Slope, elevation=all$Elevation, year=all$year, Species=rep("RF",31)),
      data.frame(plots=all$plots, bands=all$bands, iv=all$wfIV, aspect=all$Aspect, 
                 slope=all$Slope, elevation=all$Elevation, year=all$year, Species=rep("WF",31)))

stacked$Sp_yr = paste(stacked$species, stacked$year, sep="_")
levels(stacked$Species) = c("Abies magnifica", "Abies concolor")

ggplot(data=stacked, aes(x=elevation, y=iv, group=Sp_yr, colour=Sp_yr)) +
    geom_point() +
    geom_smooth(method = "lm") + 
    xlab("Plot elevation (m)")+
    ylab("IV") +
    ggtitle("Importance values as a function of elevation, 2016 and 1981") 


ggplot(data=stacked, aes(x=elevation, y=iv, group=Species, colour=Species)) +
    geom_point() +
    geom_smooth(method = "lm") + 
    facet_grid(~year)+
    xlab("Plot elevation (m)")+
    ylab("IV") +
    ggtitle("Importance values as a function of elevation, 2016 and 1981") +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


t = lm(data=ivs_1981,rfIV~Elevation)
summary(t)
