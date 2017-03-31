#Index of change using a cleaner dataset
library("xlsx")

all = read.xlsx(file="TPHByPlotSzSpecies.xlsx", 1)

#Nplots = c(1,3,4,5,6,7,8,11,12,13,14,15,16,17,18,19,21,22,23,24,29)
Nplots = c(2,9,10,20,25,26,27,27,30)

all = subset(all, !Plot.St%in%Nplots & substr(Plot.St, 1,2)!= "CT" & substr(Plot.St, 1,2)!= "HC")
all = subset(all, Band==3 || Band==4)
all


getRds = function(data, largesz, smallsz, sp, reciprocal, weighted)
{
        rds = vector(mode="numeric", length=0)
        totalRds = vector(mode="numeric", length=0)
        
        plotList = vector(mode="character", length=0)
        band = vector(mode="character", length=0)

        for(plot in unique(data$Plot.St))
        {
            large = sum(subset(data, Species == sp & SizeClass == largesz & Plot.St == plot)$avgperHA.x)
            small = sum(subset(data, Species == sp & SizeClass == smallsz & Plot.St == plot)$avgperHA.x)
      
            species_in_plot =  sum(subset(data, Plot.St == plot & Species == sp)$avgperHA)       
            all_in_plot = sum(subset(data, Plot.St == plot)$avgperHA)
            
            rds = append(rds, ((small/species_in_plot)/(large/species_in_plot)))
            #rds = append(rds, small/large)
            
            totalRds = append(totalRds, species_in_plot/all_in_plot)
            
            plotList = append(plotList, plot)
            band = append(band, subset(data,Plot.St==plot)$Band[1])
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

#getRds = function(year, largesz, smallsz, Species, reciprocal, weighted)
rf1981 = getRds(subset(all, Year==1981), 4, 0, "RF", TRUE, TRUE)
wf1981 = getRds(subset(all, Year==1981), 4, 0, "WF", TRUE, TRUE)
aweighted = calculateBandAveragesAndSE(wf1981, rf1981)

rf2016 = getRds(subset(all, Year==2016), 4, 0, "RF", TRUE, TRUE)
wf2016 = getRds(subset(all, Year==2016), 4, 0, "WF", TRUE, TRUE)
weighted = calculateBandAveragesAndSE(wf2016, rf2016)


###### Wilcox ######
rfp = numeric(0)
wfp = numeric(0)
for(i in 3:5)
{   
    rfp = append(rfp, wilcox.test(subset(rf1981, band==i)$ratio, subset(rf2016, band==i)$ratio))
    wfp = append(wfp, wilcox.test(subset(wf1981, band==i)$ratio, subset(wf2016, band==i)$ratio))
}
wfp
rfp
