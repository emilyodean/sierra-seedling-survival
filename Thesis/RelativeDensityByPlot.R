library("sqldf")
library("XLConnect")

#Set working directory
setwd("/Users/emilyodean/CodeProjects/Ecological Modeling/Thesis")

#Connect to database
db = dbConnect(SQLite(), dbname="RFWFv1.sqlite")

overstory = dbGetQuery(db,"select * from Overstory")

#Calculate relative density for each size class by plot, return list of plots & rds
calculateRdByPlot = function(species, sizeClass)
{
    rds = vector(mode="numeric", length=0)
    totalRds = vector(mode="numeric", length=0)
    
    plotList = vector(mode="character", length=0)
    band = vector(mode="character", length=0)
    
    for(plot in unique(overstory$Plot.St))
    {
        rds = append(rds, nrow(subset(overstory, (overstory$Species == species & overstory$Size.Class==sizeClass & overstory$Plot.St==plot)))/
                         nrow(subset(overstory, overstory$Plot.St==plot)))
        totalRds = append(totalRds, nrow(subset(overstory, (overstory$Species == species & overstory$Plot.St==plot)))/
                              nrow(subset(overstory, overstory$Plot.St==plot)))
        
        plotList = append(plotList, plot)
        band = append(band, substr(plot, 4, 4))
    }

    return(data.frame(plotList, band, rds, totalRds))
}

#Calculate the ratio of small to large relative densities, convert to the reciprocal if >1 (re:Albert's methods)
#Weighted by the total RD of the species in the plot
calculateRdRatios = function(small, large)
{
    ratio = (small$rds/large$rds)*small$totalRds
    output = sapply(ratio,function(x){ifelse(any(x>1), (x=1/x), x)})
    return(data.frame(small$plotList, small$band, output))
}

wfsmall = calculateRdByPlot("WF", 2)
wflarge = calculateRdByPlot("WF", 4)
rfsmall = calculateRdByPlot("RF", 2)
rflarge = calculateRdByPlot("RF", 4)

calculateRdRatios(calculateRdByPlot("WF", 2), calculateRdByPlot("WF", 4))
calculateRdRatios(calculateRdByPlot("RF", 2), calculateRdByPlot("RF", 4))

#Close connection
dbDisconnect(db)