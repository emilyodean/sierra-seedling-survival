library("XLConnect")
library("xlsx")
library("ggplot2")
library("nls2")

################## Setup #################

#Set working directory
setwd("/Users/emilyodean/CodeProjects/Ecological Modeling/Thesis")

#Utilities
if(!exists("multiplot", mode="function")) source("util.R")

cores = read.xlsx("/Users/emilyodean/Documents/Research Data/Core Data/CORES.xlsx", sheet=1)

cores = subset(cores, FirstYear >1900)

cores$fRegion = as.factor(cores$Region)
cores$fBand = as.factor(cores$Band)
cores$fSpecies = as.factor(cores$Species)




print(ggplot(cores, aes(x=FirstYear)) +
         geom_bar(stat="bin", colour="gray")+
         facet_grid(fSpecies~ .) + 
         xlab("Establishment Year")+
         ylab("Trees per hectare") +
         scale_y_continuous(labels=function(x)x*10) + 
         ggtitle("") + 
         theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")))




#Number established per year
numberEstablishedPerYear = function(elevation, whichspecies, location)
{
    #Elevation band 0 will output all data, any other elevation will subset by band
    coredata = cores
    ifelse(elevation!=0, (coredata=subset(cores, Band==elevation)), coredata)
    
    #Subset by species
    coredata = subset(coredata, Species==whichspecies)
    
    #Subset by location
    ifelse(location!="None", (coredata=subset(coredata, coredata$Region==location)), coredata)
    
    #Different titles depending on what data is being shown
    title0 = paste0("Germination dates of ", getSpeciesName(whichspecies))
    title = paste0("Germination dates of ", getSpeciesName(whichspecies), " (", getElevationBandText(elevation), "m)")
    ifelse(elevation!=0, title, (title = title0))
    ifelse(location!="None", (title = paste0(title, "\n in ", getFullLocation(location))), title)
           
    #Visualization 
    plot = ggplot(coredata, aes(x=FirstYear)) +
        geom_bar(stat="bin", colour="gray")+
        xlab("Establishment Year")+
        ylab("Trees per hectare") +
        scale_y_continuous(labels=function(x)x*10) + 
        ggtitle(title) + 
        theme_bw() + theme(plot.title = element_text(size=9), panel.border = element_blank(), panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
    
    return(plot)

}

#Grouped plots

printGroupedPlots = function()
{
    levels(cores$fRegion) = c("Stanislaus National Forest", "Yosemite National Park")
    levels(cores$fBand) = c("1675-1829m","1830-1979m", "1980-2129m", "2130-2284m", "2285-2440m")
    
    levels(cores$fSpecies) = c("Abies magnifica", "Abies concolor")
    cores$fSpecies = factor(cores$fSpecies,levels(cores$fSpecies)[c(2,1)])

    coredata = cores
    coredata = subset(coredata, Species=="WF") 
    print(ggplot(coredata, aes(x=FirstYear)) +
              geom_bar(stat="bin", colour="gray")+
              facet_grid(fBand ~ fRegion) + 
              xlab("Establishment Year")+
              ylab("Trees per hectare") +
              scale_y_continuous(labels=function(x)x*10) + 
              ggtitle("Germination dates of Abies concolor") + 
              theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                 panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")))
    
    
    coredata = cores
    coredata = subset(coredata, Species=="RF")
    
    print(ggplot(coredata, aes(x=FirstYear)) +
              geom_bar(stat="bin", colour="gray")+
              facet_grid(fBand ~ fRegion) + 
              xlab("Establishment Year")+
              ylab("Trees per hectare") +
              scale_y_continuous(labels=function(x)x*10) + 
              ggtitle("Germination dates of Abies magnifica") + 
              theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                 panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")))
    
    
    #Each species by elevation band 
    print(ggplot(cores, aes(x=FirstYear)) +
              geom_bar(stat="bin", colour="gray")+
              facet_grid(fBand ~ fSpecies) + 
              xlab("Establishment Year")+
              ylab("Trees per hectare") +
              scale_y_continuous(labels=function(x)x*10) + 
              ggtitle("Germination dates of Abies concolor and Abies magnifica") + 
              theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                 panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")))
    

}


#Write it to disk
pdf("EstablishmentYears.pdf")
printGroupedPlots()

for(region in c("None", "YOSE", "STANISLAUS"))
{
    plots = list()
    for(i in 0:5)
    {
        plots[[length(plots) + 1]] = numberEstablishedPerYear(i, "WF", region)
    }
    multiplot(plotlist=plots, cols = 2)
    
    i=0
    plots = list()
    for(i in 0:5)
    {
        plots[[length(plots) + 1]] = numberEstablishedPerYear(i, "RF", region)
    }
    i=0
    multiplot(plotlist=plots, cols = 2)
}

dev.off()






################ Climate #################

#Load in yosemite climate data, use only date, average summer temp, growing season, and MAP
yose_climate = xlsx::read.xlsx(file="/Users/emilyodean/Documents/Research Data/Historic Climate Data/Yose_Climate_WNA.xlsx", sheetIndex=1)
yose_climate_abbr = data.frame(Year = substr(yose_climate$X.period, 6, 10), Avg_Summer = yose_climate$Tave_sm., GDD = yose_climate$FFP. , MAP = yose_climate$MAP.)

#Merge the climate data with the tree data
#This will only return all of the years for which there is climate data. 
#To keep all of the trees, use all.x = TRUE as an argument to merge.

treesperyear = as.data.frame(table(cores$FirstYear, cores$Species))
names(treesperyear) = c("Year", "Species", "Count")
treesperyear = merge(treesperyear, yose_climate_abbr, by="Year")

wf = subset(treesperyear, Species=="WF")
rf = subset(treesperyear, Species=="RF")

#Fit a model to see which climate variables can predict the establishment of each species
all = glm(Count~Avg_Summer+GDD+MAP+Species,data=treesperyear,family=poisson())
summary(all)
wffit = glm(Count~Avg_Summer*GDD*MAP,data=wf,family=poisson())
summary(wffit)
rffit = glm(Count~Avg_Summer*GDD*MAP,data=rf,family=poisson())
summary(rffit)

wffit = lm(Count~MAP, data=wf)
summary(wffit)
rffit = lm(Count~MAP, data=rf)
summary(rffit)


plot(Count~Year, data=treesperyear)
hist(predictions$firstyear)







