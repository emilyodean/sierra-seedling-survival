#Visualize climate over time
library("xlsx")
library("ggplot2")

#Utilities
if(!exists("multiplot", mode="function")) source("util.R")

#For graphing - defines how many tick marks are on my x-axis
number_ticks = function(n) {function(limits) pretty(limits, n)}


#Load in yosemite climate data, use only date, average summer temp, growing season, and MAP
yose_climate = xlsx::read.xlsx(file="/Users/emilyodean/Documents/Research Data/Historic Climate Data/Yose_Climate_WNA.xlsx", sheetIndex=1)
yose_climate_abbr = data.frame(Year = as.numeric(substr(yose_climate$X.period, 6, 10)),
                               Avg_Summer = yose_climate$Tave_sm.,
                               GDD = yose_climate$FFP.,
                               MAP = yose_climate$MAP.,
                               stringsAsFactors = FALSE)

yose_future = xlsx::read.xlsx(file="/Users/emilyodean/Documents/Research Data/Historic Climate Data/Yose_Future_WNA_CanESM.xlsx", sheetIndex=1)
yose_future_abbr = data.frame(Year = as.numeric(substr(yose_future$X.period, 15, 19)),
                               Avg_Summer = yose_future$Tave_sm.,
                               GDD = yose_future$FFP.,
                               MAP = yose_future$MAP.,
                               stringsAsFactors = FALSE)


#yose_climate_abbr = yose_future_abbr

yose_climate_abbr = rbind(yose_climate_abbr, yose_future_abbr)


#yose_climate_abbr = subset(yose_climate_abbr, Year<1990)

###### Plots #######
#pdf("ClimateOverTime.pdf") 
pdf("PastAndFutureClimate.pdf") 


#Avg Summer Temp
avg_summer = ggplot(aes(x = Year, y = Avg_Summer, group = 1), data = yose_climate_abbr) + 
    geom_smooth()+ #Comment this out and switch to geom_line if you want to see more detail
    #geom_line()+
    xlab("Year")+
    ylab(expression(paste("Temperature (",degree,"C)"))) +
    #scale_x_continuous(breaks = number_ticks(4)) + 
    ggtitle("Average Summer Temperature, historic and predicted") + 
    #ggtitle("Average Summer Temperature, 1900-Present") + 
    #ggtitle("Average Summer Temperature, 2017-2100") + 
    theme_bw() + 
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#GDD
gdd = ggplot(aes(x = Year, y = GDD, group = 1), data = yose_climate_abbr) + 
    geom_smooth()+
    #geom_line()+
    xlab("Year")+
    ylab("Season Length in Days") +
    #scale_x_continuous(breaks = number_ticks(10)) + 
    #ggtitle("Growing Season Length, 1900-Present") + 
    #ggtitle("Growing Season Length, 2017-2100") + 
    ggtitle("Growing Season Length, historic and predicted") + 
    theme_bw() + 
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#Precip
map = ggplot(aes(x = Year, y = MAP, group = 1), data = yose_climate_abbr) + 
    geom_smooth()+
    #geom_line()+
    xlab("Year")+
    ylab("Precipitation (mm)") +
    #scale_x_continuous(breaks = number_ticks(10)) + 
    #ggtitle("Mean Annual Precipitation, 1900-Present") + 
    #ggtitle("Mean Annual Precipitation, 2017-2100") +
    ggtitle("Mean Annual Precipitation, historic and predicted") +
    theme_bw() + 
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

multiplot(avg_summer, gdd, map, cols=1)
dev.off()


