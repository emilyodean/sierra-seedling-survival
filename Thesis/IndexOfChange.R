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
overstory2016 = dbGetQuery(db,"select * from Overstory where region like 'Yosemite' and band!=1")
#overstory2016 = dbGetQuery(db,"select * from Overstory where region like 'Stanislaus' and band!=1")
#overstory2016 = dbGetQuery(db,"select * from Overstory where band!=1")
aoverstory = dbGetQuery(albertdb,"select * from Overstory")

#Seedlings
#Just yose again
understory =  dbGetQuery(db,"select * from Regeneration where band!=1 and (substr([Plot.St],1,2) like 'TR' or substr([Plot.St],1,2) like 'GP')")
#understory =  dbGetQuery(db,"select * from Regeneration where band!=1 and (substr([Plot.St],1,2) like 'CT' or substr([Plot.St],1,2) like 'HC')")
understory =  dbGetQuery(db,"select * from Regeneration where band!=1")
aunderstory = dbGetQuery(albertdb,"select * from Sapling")

names(aunderstory)[1] = "Plot.St"
names(aunderstory)[6] = "Count"
plotsbands = unique(aoverstory[, c('Plot.St', 'Band')])
aunderstory = merge(aunderstory, plotsbands, by="Plot.St", all=FALSE)


################## Helper Functions ##################

#Calculate relative density of seedlings by plot
calculateSeedlingRdByPlot1981 = function(seedlings, overstory, species, weighted, reciprocal)
{

    seedsmult = (10000/120)
    saplingsmult = (10000/600)
    treesmult = (10000/1200)

    rds = vector(mode="numeric", length=0)
    totalRds = vector(mode="numeric", length=0)
    
    plotList = vector(mode="character", length=0)
    band = vector(mode="character", length=0)
    
    for(plot in unique(seedlings$Plot.St))
    {
        seeds = seedsmult * sum(subset(seedlings, 
                                       (seedlings$Species == species & seedlings$Plot.St==plot))$Count, na.rm=TRUE) #Sum all seedlings in specified plot of species type
        #trees = treesmult * nrow(subset(overstory, (overstory$Plot.St==plot & overstory$Species == species & overstory$Size.Class==4)))#Sum all trees in specified plot of said species
        trees = treesmult * nrow(subset(overstory, (overstory$Plot.St==plot & overstory$Species == species)))#Sum all trees in specified plot of said species
        
        allspeciesx = seeds + 
            (treesmult * nrow(subset(overstory, (overstory$Plot.St==plot & overstory$Species == species)))) +
            (saplingsmult * sum(subset(seedlings,(seedlings$Species == species & seedlings$Plot.St==plot))$Lg.Sapl, na.rm=TRUE)) + 
            (saplingsmult * sum(subset(seedlings,(seedlings$Species == species & seedlings$Plot.St==plot))$Sm.Sapl, na.rm=TRUE))
        
        allinplot = (seedsmult * sum(subset(seedlings, (seedlings$Plot.St==plot))$Count, na.rm=TRUE)) + 
            (treesmult * nrow(subset(overstory, (overstory$Plot.St==plot)))) +
            (saplingsmult * sum(subset(seedlings,(seedlings$Plot.St==plot))$Lg.Sapl, na.rm=TRUE)) + 
            (saplingsmult * sum(subset(seedlings,(seedlings$Plot.St==plot))$Sm.Sapl, na.rm=TRUE))
       
        rds = append(rds, (seeds/allspeciesx)/(trees/allspeciesx))
        
        totalRds = append(totalRds, allspeciesx/allinplot)
        
        plotList = append(plotList, plot)
        band = append(band, subset(seedlings, seedlings$Plot.St==plot)$Band[1])
    }
    
    if(reciprocal==TRUE) {
        output = sapply(rds,function(x){ifelse(any(x>1), (x=1/x), x)})
    } else
    {
        output = rds
    }
    
    #Check the flag
    if(weighted==TRUE) {output=output*totalRds}
    
    return(data.frame(plot = plotList, band = band, ratio = output))
}


#Calculate relative density of seedlings by plot
calculateSeedlingRdByPlot = function(seedlings, overstory, species, weighted, reciprocal)
{
    seedsmult = 100
    treesmult = 10
    
    rds = vector(mode="numeric", length=0)
    totalRds = vector(mode="numeric", length=0)
    
    plotList = vector(mode="character", length=0)
    band = vector(mode="character", length=0)
    
    for(plot in unique(seedlings$Plot.St))
    {
        seeds = seedsmult * sum(subset(seedlings, 
                                       (seedlings$Species == species & seedlings$Plot.St==plot))$Count, na.rm=TRUE) #Sum all seedlings in specified plot of species type
        #trees = treesmult * nrow(subset(overstory,(overstory$Plot.St==plot & overstory$Species == species & overstory$Size.Class==4)))#Sum all trees in specified plot of said species
        trees = treesmult * nrow(subset(overstory, 
                                        (overstory$Plot.St==plot & overstory$Species == species & overstory$DBH..cm.>10)))#Sum all trees in specified plot of said species
        
        
        allspeciesx = seeds + (treesmult * nrow(subset(overstory,(overstory$Plot.St==plot & overstory$Species == species))))
        allinplot = (seedsmult * sum(subset(seedlings, (seedlings$Plot.St==plot))$Count, na.rm=TRUE)) + 
            (treesmult * nrow(subset(overstory,(overstory$Plot.St==plot))))
           
        
        rds = append(rds, (seeds/allspeciesx)/(trees/allspeciesx))
        totalRds = append(totalRds, allspeciesx/allinplot)
        
        plotList = append(plotList, plot)
        band = append(band, subset(seedlings, seedlings$Plot.St==plot)$Band[1])
    }
    
    
    
    if(reciprocal==TRUE) {
        output = sapply(rds,function(x){ifelse(any(x>1), (x=1/x), x)})
    } else
    {
        output = rds
    }
    #output = rds
    
    #Check the flag
    if(weighted==TRUE) {output=output*totalRds}
    
    return(data.frame(plot = plotList, band = band, ratio = output))
}


#Caclulate the average and standard error for each band and put it all in a dataframe. Useful for graphing.
calculateBandAveragesAndSE = function(wfdf, rfdf)
{   
    wfdf$species = "wf"
    rfdf$species = "rf"
    
    df = rbind(wfdf, rfdf)
    df[is.na(df)] = 0
    
    means = aggregate(ratio~band+species,data=df,mean)
    se = aggregate(ratio~band+species,data=df,(function(x){sqrt(var(x)/length(x))}))[3]
    
    rfwf =data.frame(means, se)
    colnames(rfwf) = c("band", "species", "ratio", "se")
    return(rfwf)
}

#Not weighted, no reciprocal
awfseedling = calculateSeedlingRdByPlot1981(aunderstory, aoverstory, "WF", FALSE, FALSE)
arfseedling = calculateSeedlingRdByPlot1981(aunderstory, aoverstory, "RF", FALSE, FALSE)
aunweighted = calculateBandAveragesAndSE(awfseedling, arfseedling)

wfseedling = calculateSeedlingRdByPlot(understory, overstory2016, "WF", FALSE, FALSE)
rfseedling = calculateSeedlingRdByPlot(understory, overstory2016, "RF", FALSE, FALSE)
unweighted = calculateBandAveragesAndSE(wfseedling, rfseedling)

#Weighted by species importance, but no reciprocal
awfseedling = calculateSeedlingRdByPlot1981(aunderstory, aoverstory, "WF", TRUE, FALSE)
arfseedling = calculateSeedlingRdByPlot1981(aunderstory, aoverstory, "RF", TRUE, FALSE)
anorecip = calculateBandAveragesAndSE(awfseedling, arfseedling)

wfseedling = calculateSeedlingRdByPlot(understory, overstory2016, "WF", TRUE, FALSE)
rfseedling = calculateSeedlingRdByPlot(understory, overstory2016, "RF", TRUE, FALSE)
norecip = calculateBandAveragesAndSE(wfseedling, rfseedling)

#Not weighted, with reciprocal
awfseedling = calculateSeedlingRdByPlot1981(aunderstory, aoverstory, "WF", FALSE, FALSE)
arfseedling = calculateSeedlingRdByPlot1981(aunderstory, aoverstory, "RF", FALSE, FALSE)
aunweighted = calculateBandAveragesAndSE(awfseedling, arfseedling)

wfseedling = calculateSeedlingRdByPlot(understory, overstory2016, "WF", FALSE, FALSE)
rfseedling = calculateSeedlingRdByPlot(understory, overstory2016, "RF", FALSE, FALSE)
unweighted = calculateBandAveragesAndSE(wfseedling, rfseedling)

#Both weighted and reciprocal
awfseedling = calculateSeedlingRdByPlot1981(aunderstory, aoverstory, "WF", TRUE, TRUE)
arfseedling = calculateSeedlingRdByPlot1981(aunderstory, aoverstory, "RF", TRUE, TRUE)
aweighted = calculateBandAveragesAndSE(awfseedling, arfseedling)

wfseedling = calculateSeedlingRdByPlot(understory, overstory2016, "WF", TRUE, TRUE)
rfseedling = calculateSeedlingRdByPlot(understory, overstory2016, "RF", TRUE, TRUE)
weighted = calculateBandAveragesAndSE(wfseedling, rfseedling)

###### Wilcox ######
rfp = numeric(0)
wfp = numeric(0)
for(i in 3:5)
{   
    wfp = append(wfp, wilcox.test(subset(awfseedling, band==i)$ratio, subset(wfseedling, band==i)$ratio))
    rfp = append(rfp, wilcox.test(subset(arfseedling, band==i)$ratio, subset(rfseedling, band==i)$ratio))
}
wfp
rfp
# 
# > wfp
# [1] 0.24747574 0.09039768 0.46871658
# > rfp
# [1] 0.004848093 0.124654882 0.117305564



#Calculate relative density for each size class by plot, return list of plots & rds
calculateRdByPlot = function(overstory, species, sizeClass)
{
    rds = vector(mode="numeric", length=0)
    totalRds = vector(mode="numeric", length=0)
    
    plotList = vector(mode="character", length=0)
    band = vector(mode="character", length=0)
    
    for(plot in unique(overstory$Plot.St))
    {
        rds = append(rds, nrow(subset(overstory, (overstory$Species == species & overstory$Size.Class==sizeClass & overstory$Plot.St==plot)))/
                         nrow(subset(overstory, (overstory$Plot.St==plot & overstory$Species == species))))
        totalRds = append(totalRds, nrow(subset(overstory, (overstory$Species == species & overstory$Plot.St==plot)))/
                              nrow(subset(overstory, overstory$Plot.St==plot)))
        
        plotList = append(plotList, plot)
        band = append(band, subset(overstory, overstory$Plot.St==plot)$Band[1])
    }
    
    return(data.frame(plotList, band, rds, totalRds))
}


#Calculate the ratio of small to large relative densities, convert to the reciprocal if >1 (re:Albert's methods)
#Weighted by the total RD of the species in the plot
calculateRdRatios = function(small, large, weighted=TRUE)
{
    ratio = (small$rds/large$rds)
    
    output = sapply(ratio,function(x){ifelse(any(x>1), (x=1/x), x)})
    
    #Check the flag
    if(weighted==TRUE) {ratio=ratio*small$totalRds} #small$totalRds == large$totalRds
    
    return(data.frame(plot = small$plotList, band=small$band, ratio=output))
}



################## Run the functions ##################

#Current
wfRatios = calculateRdRatios(calculateRdByPlot(overstory2016, "WF", 2), calculateRdByPlot(overstory2016,"WF", 4))
rfRatios = calculateRdRatios(calculateRdByPlot(overstory2016,"RF", 2), calculateRdByPlot(overstory2016,"RF", 4))

#Albert
awfRatios = calculateRdRatios(calculateRdByPlot(aoverstory, "WF", 2), calculateRdByPlot(aoverstory,"WF", 4))
arfRatios = calculateRdRatios(calculateRdByPlot(aoverstory,"RF", 2), calculateRdByPlot(aoverstory,"RF", 4))

#### Non-weighted ####
#Current
wfRatios = calculateRdRatios(calculateRdByPlot(overstory2016, "WF", 2), calculateRdByPlot(overstory2016,"WF", 4), FALSE)
rfRatios = calculateRdRatios(calculateRdByPlot(overstory2016,"RF", 2), calculateRdByPlot(overstory2016,"RF", 4), FALSE)

#Albert
awfRatios = calculateRdRatios(calculateRdByPlot(aoverstory, "WF", 2), calculateRdByPlot(aoverstory,"WF", 4), FALSE)
arfRatios = calculateRdRatios(calculateRdByPlot(aoverstory,"RF", 2), calculateRdByPlot(aoverstory,"RF", 4), FALSE)



df = calculateBandAveragesAndSE(wfRatios, rfRatios)
adf = calculateBandAveragesAndSE(awfRatios, arfRatios)

###### Wilcox ######
rfp = numeric(0)
wfp = numeric(0)
for(i in 3:5)
{   
    wfp = append(wfp, wilcox.test(subset(awfRatios, band==i)$ratio, subset(wfRatios, band==i)$ratio))
    rfp = append(rfp, wilcox.test(subset(arfRatios, band==i)$ratio, subset(rfRatios, band==i)$ratio))
}
wfp
rfp

# > wfp
# [1] 0.76288362 0.09645276 0.15951201
# > rfp
# [1] 0.2471855 0.3564634 0.7143792







################## Visualizations ##################


######## Compare 1980s to 2016 ########
limits = aes(ymax = df$ratio + df$se, ymin=df$ratio - df$se)
alimits = aes(ymax = adf$ratio + adf$se, ymin=adf$ratio - df$se)

#Current
p1 = ggplot(df, aes(x=factor(band), y=ratio, fill=factor(species)), colour=c("red", "green"))+
    geom_bar(stat="identity",position="dodge", width=0.5)+
    geom_errorbar(limits, position=position_dodge(0.5), width=0.08) + 
    scale_fill_discrete(name="Species",
                        breaks=c("wf", "rf"),
                        labels=c("Abies Concolor", "Abies Magnifica"))+
    xlab("Elevation Band (meters)")+
    ylab("Relative Density Ratio") +
    ggtitle("2016 Mean Weighted Relative Density Ratio by Elevation Band") + 
    scale_x_discrete(labels=c("1830-1980", "1980-2130", "2130-2285", "2285-2440"))
 
#Parker
p2 = ggplot(adf, aes(x=factor(band), y=ratio, fill=factor(species)), colour=c("red", "green"))+
    geom_bar(stat="identity",position="dodge", width=0.5)+
    geom_errorbar(alimits, position=position_dodge(0.5), width=0.08) + 
    scale_fill_discrete(name="Species",
                       breaks=c("wf", "rf"),
                        labels=c("Abies Concolor", "Abies Magnifica"))+
    xlab("")+
    ylab("") +
    ggtitle("1980s Mean Weighted Relative Density Ratio by Elevation Band") + 
    scale_x_discrete(labels=c("1830-1980", "1980-2130", "2130-2285", "2285-2440"))
#+scale_fill_manual(values=c("#CC6666", "#9999CC"))

multiplot(p2, p1, cols=1)

######## Compare by species instead of by year #########

df = calculateBandAveragesAndSE(wfRatios, awfRatios)
adf = calculateBandAveragesAndSE(rfRatios, arfRatios)
alimits = aes(ymax = adf$ratio + adf$se, ymin=adf$ratio - adf$se)

#WF
p3 = ggplot(df, aes(x=factor(band), y=ratio, fill=factor(species)), colour=c("red", "green"))+
    geom_bar(stat="identity",position="dodge", width=0.5)+
    geom_errorbar(limits, position=position_dodge(0.5), width=0.08) + 
    scale_fill_discrete(name="Year",
                        breaks=c("wf", "rf"),
                        labels=c("2016", "1980s"))+
    xlab("Elevation Band (meters)")+
    ylab("Relative Density Ratio") +
    ggtitle("Abies concolor mean weighted relative density ratio by elevation") + 
    scale_x_discrete(labels=c("1830-1980", "1980-2130", "2130-2285", "2285-2440"))

#RF
p4 = ggplot(adf, aes(x=factor(band), y=ratio, fill=factor(species)), colour=c("red", "green"))+
    geom_bar(stat="identity",position="dodge", width=0.5)+
    geom_errorbar(alimits, position=position_dodge(0.5), width=0.08) + 
    scale_fill_discrete(name="Year",
                        breaks=c("wf", "rf"),
                        labels=c("2016", "1980s"))+
    xlab("")+
    ylab("") +
    ggtitle("Abies magnifica mean weighted relative density ratio by elevation") + 
    scale_x_discrete(labels=c("1830-1980", "1980-2130", "2130-2285", "2285-2440"))

multiplot(p4, p3, cols=1)

#Close connection
dbDisconnect(db)
dbDisconnect(albertdb)
