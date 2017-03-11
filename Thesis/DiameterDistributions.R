#Diameter distributions with mortality

library("sqldf")
library("XLConnect")
library("ggplot2")
library("lattice")


################## Setup ##################
#Set working directory
setwd("/Users/emilyodean/CodeProjects/Ecological Modeling/Thesis")
    
#Utilities
if(!exists("multiplot", mode="function")) source("util.R")

#Connect to database
db = dbConnect(SQLite(), dbname="RFWFv1.sqlite")
albertdb = dbConnect(SQLite(), dbname="Albertv1.sqlite")

sqldf("pragma table_info(Overstory)", dbname = "RFWFv1.sqlite")$name   

#I am removing size class 1 here to keep my data consistent with Albert's
overstory = dbGetQuery(db,"select * from Overstory where (species like 'RF' or species like 'WF')")
rfoverstory = dbGetQuery(db,"select * from Overstory where species like 'RF' and [DBH..cm.] > 10")
wfoverstory = dbGetQuery(db,"select * from Overstory where species like 'WF' and [DBH..cm.] > 10")


rfoverstory = dbGetQuery(db,"select * from Overstory where species like 'RF' and [DBH..cm.] > 10 and region like 'Yosemite'")
wfoverstory = dbGetQuery(db,"select * from Overstory where species like 'WF' and [DBH..cm.] > 10 and region like 'Yosemite'")
arfoverstory = dbGetQuery(albertdb,"select * from Overstory where species like 'RF'")
awfoverstory = dbGetQuery(albertdb,"select * from Overstory where species like 'WF'")

rfsaplings = dbGetQuery(db,"select * from Overstory where species like 'RF' and [DBH..cm.] < 10")
wfsaplings = dbGetQuery(db,"select * from Overstory where species like 'WF' and [DBH..cm.] < 10")


#Write all of this to disk
pdf("DiameterDistributions.pdf")
#RF
for(i in 1:5)
{
    overstory = subset(rfoverstory, Band==i)
    print(ggplot(overstory, aes(x=overstory$DBH..cm, fill=Status)) +
        geom_bar(stat="bin", colour="black")+
        xlab("Diameter (cm)")+
        ylab("Trees per hectare") +
        scale_y_continuous(labels=function(x)x*10) + 
        ggtitle(paste0("Abies magnifica diameter distribution by status (", getElevationBandText(i), "m)")) + 
        theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")))

}

#WF
for(i in 1:5)
{
    overstory = subset(wfoverstory, Band==i)
    print(ggplot(overstory, aes(x=overstory$DBH..cm, fill=Status)) +
        geom_bar(stat="bin", colour="black")+
        xlab("Diameter (cm)")+
        ylab("Trees per hectare") +
        scale_y_continuous(labels=function(x)x*10) + 
        ggtitle(paste0("Abies concolor diameter distribution by status (", getElevationBandText(i), "m)")) + 
        theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")))
}

dev.off()

#Everything together now
overstory$fBand = as.factor(overstory$Band)
overstory$fSpecies = as.factor(overstory$Species)
levels(overstory$fBand) = c("1675-1829m","1830-1979m", "1980-2129m", "2130-2284m", "2285-2440m")
levels(overstory$fSpecies) = c("Abies magnifica", "Abies concolor")
overstory$fSpecies = factor(overstory$fSpecies,levels(overstory$fSpecies)[c(2,1)])



pdf("PooledDiameterDistributions.pdf")

print(ggplot(overstory, aes(x=overstory$DBH..cm, fill=Status)) +
          geom_bar(stat="bin", colour="black")+
          facet_grid(fBand ~ fSpecies) + 
          xlab("Diameter (cm)")+
          ylab("Trees per hectare") +
          scale_y_continuous(labels=function(x)x*10) + 
          ggtitle(paste0("Abies concolor and Abies magnifica diameter distribution by status")) +
          scale_fill_manual(values=c("#009933", "orange", "red", "#999999", "#333333"), 
                            labels=c("Healthy", "Flagging", "Red Stage", "Gray Stage", "Snag")) +
          theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")))

dev.off()

#Everything less than 10cm & more than 10cm
less10cm = subset(overstory, DBH..cm.<10)
over10cm = subset(overstory, DBH..cm.>=10)

print(ggplot(less10cm, aes(x=less10cm$DBH..cm, fill=Status)) +
          geom_bar(stat="bin", colour="black")+
          facet_grid(fBand ~ fSpecies) + 
          xlab("Diameter (cm)")+
          ylab("Trees per hectare") +
          scale_y_continuous(labels=function(x)x*10) + 
          ggtitle(paste0("Abies concolor and Abies magnifica\n diameter distribution by status (<10cm)")) +
          scale_fill_manual(values=c("#009933", "orange", "red", "#999999", "#333333"), 
                            labels=c("Healthy", "Flagging", "Red Stage", "Gray Stage", "Snag")) +
          theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")))

print(ggplot(overstory, aes(x=overstory$DBH..cm, fill=Status)) +
          geom_bar(stat="bin", colour="black")+
          facet_grid(fBand ~ fSpecies) + 
          xlab("Diameter (cm)")+
          ylab("Trees per hectare") +
          scale_y_continuous(labels=function(x)x*10) + 
          ggtitle(paste0("Abies concolor and A. magnifica\n diameter distribution by status  (>10cm)")) +
          scale_fill_manual(values=c("#009933", "orange", "red", "#999999", "#333333"), 
                            labels=c("Healthy", "Flagging", "Red Stage", "Gray Stage", "Snag")) +
          theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")))



#Plot percentage of each species in each band that are healthy vs dead
    ggplot(overstory) + 
    geom_bar(data = overstory, aes(x = factor(overstory$Species),fill = factor(overstory$Status)), position = "fill", stat="bin")  
    #+facet_grid(.~Band)

rf2 =    ggplot() + 
    geom_bar(data = rfoverstory, aes(x = factor(rfoverstory$Band),fill = factor(rfoverstory$Status)), position = "fill")+
    xlab("Elevation (m)")+
    ylab("Percent") +
    ggtitle("Percent of Abies magnifica in each health status by elevation band") + 
    scale_x_discrete(labels=c("1830-1980", "1980-2130", "2130-2285", "2285-2440")) + 
    scale_y_continuous(labels = scales::percent) + 
    scale_fill_discrete(name="Status",
                        labels=c("1 (Healthy)", "1.5 (Flagging)", "2 (Red Stage)", "3 (Gray Stage)", "4 (Snag)"))
wf2 =    ggplot() + 
    geom_bar(data = wfoverstory, aes(x = factor(wfoverstory$Band),fill = factor(wfoverstory$Status)), position = "fill")+
    xlab("Elevation (m)")+
    ylab("Percent") +
    scale_y_continuous(labels = scales::percent) + 
    ggtitle("Percent of Abies concolor in each health status by elevation band") + 
    scale_x_discrete(labels=c("1830-1980", "1980-2130", "2130-2285", "2285-2440")) 

multiplot(wf2, rf2, cols=1)



#####Albert's versus Present#######
library("splitstackshape")


df = unique(arfoverstory[,c('Plot.St', 'Band')])
#table(df$Band)
#2  3  4  5 
#1 11 13  5 
#sample(1:11, 3)
#4, 6, 11
#sample(1:13, 3)
#1, 12, 8
#sample(1:5, 3)
#2, 4, 5


#Need these all to be the same scale as my data, so I'm using 3 plots per band and 1/10 hectare size
band2 = subset(arfoverstory, Band==2)
band2 = rbind(band2,band2,band2,band2,band2,band2)
band3 = subset(arfoverstory, (Plot.St==16 | Plot.St==22 | Plot.St ==30) & Quadrat!=4 & Quadrat!=12)
band3 = rbind(band3,band3)
band4 = subset(arfoverstory, (Plot.St==16 | Plot.St==10 | Plot.St ==20) & Quadrat!=7 & Quadrat!=9)
band4 = rbind(band4,band4)
band5 = subset(arfoverstory, (Plot.St==2 | Plot.St==26 | Plot.St ==27) & Quadrat!=4 & Quadrat!=5)
band5 = rbind(band5,band5)

arfoverstory = rbind(band2, band3, band4, band5)

combinedrf = data.frame(DBH = c(rfoverstory$DBH..cm., arfoverstory$DBH..cm.), 
                        Band = c(rfoverstory$Band, arfoverstory$Band),
                        Year = c(rep(2016, nrow(rfoverstory)), rep(1981, nrow(arfoverstory))))

combinedrf$Band = as.factor(combinedrf$Band)
levels(combinedrf$Band) = c("1830-1979m", "1980-2129m", "2130-2284m", "2285-2440m")


#RF
ggplot(combinedrf, aes(x=combinedrf$DBH)) +
    geom_bar(stat="bin", colour="black")+
    facet_grid(Band ~ Year) + 
    xlab("Diameter (cm)")+
    ylab("Trees per hectare") +
    scale_y_continuous(labels=function(x)x*10) + 
    ggtitle(paste0("Abies magnifica diameter distribution")) +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


#Need these all to be the same scale as my data, so I'm using 3 plots per band and 1/10 hectare size
band2 = subset(awfoverstory, Band==2)
band2 = rbind(band2,band2,band2,band2,band2,band2)
band3 = subset(awfoverstory, (Plot.St==16 | Plot.St==22 | Plot.St ==30) & Quadrat!=4 & Quadrat!=12)
band3 = rbind(band3,band3)
band4 = subset(awfoverstory, (Plot.St==16 | Plot.St==10 | Plot.St ==20) & Quadrat!=7 & Quadrat!=9)
band4 = rbind(band4,band4)
band5 = subset(awfoverstory, (Plot.St==2 | Plot.St==26 | Plot.St ==27) & Quadrat!=4 & Quadrat!=5)
band5 = rbind(band5,band5)

awfoverstory = rbind(band2, band3, band4, band5)


wfoverstory = subset(wfoverstory, Band!=1)
combinedwf = data.frame(DBH = c(wfoverstory$DBH..cm., awfoverstory$DBH..cm.), 
                        Band = c(wfoverstory$Band, awfoverstory$Band),
                        Year = c(rep(2016, nrow(wfoverstory)), rep(1981, nrow(awfoverstory))))

combinedwf$Band = as.factor(combinedwf$Band)
levels(combinedwf$Band) = c("1830-1979m", "1980-2129m", "2130-2284m", "2285-2440m")


#WF
ggplot(combinedwf, aes(x=combinedwf$DBH)) +
    geom_bar(stat="bin", colour="black")+
    facet_grid(Band ~ Year) + 
    xlab("Diameter (cm)")+
    ylab("Trees per hectare") +
    scale_y_continuous(labels=function(x)x*10) + 
    ggtitle(paste0("Abies concolor diameter distribution")) +
    theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


                      
#Albert RF
print(ggplot(arfoverstory, aes(x=arfoverstory$DBH..cm.)) +
          geom_bar(stat="bin", colour="black", binwidth=20)+
          facet_wrap(~Band, ncol=1) + 
          xlab("Diameter (cm)")+
          ylab("Trees per hectare") +
          scale_y_continuous(labels=function(x)x*(10000/120)) + 
          ggtitle(paste0("Abies magnifica diameter distribution by status (>10cm)")) +
          theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
                             axis.title.y=element_blank(),
                             axis.text.y=element_blank(),
                             axis.ticks.y=element_blank()))



#RF
print(ggplot(rfoverstory, aes(x=rfoverstory$DBH..cm.)) +
          geom_bar(stat="bin", colour="black", binwidth=20)+
          facet_wrap(~Band, ncol=1) + 
          xlab("Diameter (cm)")+
          ylab("Trees per hectare") +
          scale_y_continuous(labels=function(x)x*10) + 
          ggtitle(paste0("Abies magnifica diameter distribution by status (>10cm)")) +
          theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
                             #axis.title.y=element_blank(),
                             #axis.text.y=element_blank(),
                             #axis.ticks.y=element_blank()))

m <- mtcars[1:20, ];
m = combinedrf

m$br <- cut(m$DBH,hist(m$DBH,5,plot=F)$breaks);
mean.wt <- tapply(m$DBH, list(m$Year, m$Band),mean);


m2 <- data.frame(mpg.bin=names(mean.wt),mean.wt);
ggplot(m2,aes(x=mpg.bin,y=mean.wt)) + geom_bar();





#Close connection
dbDisconnect(db)
dbDisconnect(albertdb)





#Unused


# #rf = ggplot(rfoverstory, aes(x=rfoverstory$DBH..cm, fill=Status)) +
#     #geom_histogram() + 
#     #geom_bar(stat="bin", colour="black", show_guide=FALSE)+
#     geom_bar(stat="bin", colour="black")+
#     #facet_grid(.~fBand) +
#     xlab("Diameter (cm)")+
#     ylab("Count") +
#     ggtitle("Abies magnifica diameter distribution by status") + 
#     theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#                        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
# 
# wf= ggplot(wfoverstory, aes(x=wfoverstory$DBH..cm, fill=Status)) +
#     geom_bar(stat="bin", colour="black")+
#     #facet_grid(.~Band) +
#     xlab("Diameter (cm)")+
#     ylab("Count") +
#     ggtitle("Abies concolor diameter distribution by status") + 
#     theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
#                        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


#multiplot(wf, rf, cols=1)
