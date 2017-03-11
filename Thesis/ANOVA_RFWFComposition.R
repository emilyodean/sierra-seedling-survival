library("sqldf")
library("XLConnect")
library("xlsx")
library("ggplot2")


################## Setup ##################
#Set working directory
setwd("/Users/emilyodean/CodeProjects/Ecological Modeling/Thesis")

#Utilities
if(!exists("multiplot", mode="function")) source("util.R")

#Connect to database
db = dbConnect(SQLite(), dbname="RFWFv1.sqlite")
dbListTables(db)

#Load in seedlings, aggregate by plot and species, add size class (0), rename columns
seedlings = dbGetQuery(db,"select * from Regeneration where Species like 'WF' or Species like 'RF'")
seedlingsums = as.data.frame(aggregate(seedlings$Count~seedlings$Plot.St + seedlings$Species, FUN=sum))
seedlingsums$SizeClass=0
names(seedlingsums) = c("Plot.St", "Species", "Freq", "SizeClass")

#Load in overstory, table by plot and species
overstory = dbGetQuery(db,"select * from Overstory where Species like 'WF' or Species like 'RF'")
overstory = as.data.frame(table(Plot.St=overstory$Plot.St, Species=overstory$Species, SizeClass=overstory$Size.Class))

#Make one dataframe with overstory and understory
seedlingsums$avgperHA = (seedlingsums$Freq)*100
overstory$avgperHA = (overstory$Freq)*10

all = rbind(seedlingsums, overstory)


albertdb = dbConnect(SQLite(), dbname="Albertv1.sqlite")
aoverstory = dbGetQuery(albertdb,"select * from Overstory where Species like 'WF' or Species like 'RF'")


aunderstory = dbGetQuery(albertdb,"select * from Sapling where Species like 'WF' or Species like 'RF'")
aunderstory$saplings = aunderstory$Lg.Sapl + aunderstory$Sm.Sapl
names(aunderstory)[1] = "Plot.St"

aseedlingsums = as.data.frame(aggregate(aunderstory$Seedl~aunderstory$Plot.St + aunderstory$Species, FUN=sum))
asaplingsums = as.data.frame(aggregate(aunderstory$saplings~aunderstory$Plot.St + aunderstory$Species, FUN=sum))
asaplingsums$SizeClass = 1
aseedlingsums$SizeClass = 0
names(asaplingsums) = c("Plot.St", "Species", "Freq", "SizeClass")
names(aseedlingsums) = c("Plot.St", "Species", "Freq", "SizeClass")

amature = as.data.frame(table(Plot.St=aoverstory$Plot.St, Species=aoverstory$Species, SizeClass=aoverstory$Size.Class))

aseedlingsums$avgperHA = aseedlingsums$Freq*83.33
asaplingsums$avgperHA = asaplingsums$Freq*16.67
amature$avgperHA = amature$Freq*8.33

a_all = rbind(aseedlingsums, asaplingsums, amature)
plotsbands = unique(aoverstory[, c('Plot.St', 'Band')])
a_all = merge(a_all, plotsbands, by="Plot.St", all=FALSE)


#Just want to verify that I'm averaging correctly --- yep
#test = subset(a_all, Band == 3 & SizeClass == 0 & Species=="RF")
#sum(test$avgperHA)/nrow(test)

#aall = aggregate(a_all$avgperHA~a_all$Band + a_all$Species + a_all$SizeClass, FUN=mean)
#names(aall) = c("Band", "Species", "SizeClass", "avgperHA")

all$Year = 2016
all$Band = substr(all$Plot.St, 4, 4)
a_all$Year = 1981

t = aggregate(data = all, avgperHA~SizeClass+Plot.St, FUN=sum)
t_2016 = merge(all, t, by=c("Plot.St", "SizeClass"))
t_2016$ratio = t_2016$avgperHA.x / t_2016$avgperHA.y

u = aggregate(data = a_all, avgperHA~SizeClass+Plot.St, FUN=sum)
t_1981 = merge(a_all, u, by=c("Plot.St", "SizeClass"))
t_1981$ratio = t_1981$avgperHA.x / t_1981$avgperHA.y

all_data = rbind(t_1981, t_2016)
all_data$Band = as.factor(all_data$Band)
all_data$Species = as.factor(all_data$Species)
all_data$Year = as.factor(all_data$Year)
all_data$SizeClass = as.factor(all_data$SizeClass)
levels(all_data$Species) = c("Abies magnifica", "Abies concolor")

rf = subset(all_data, Species=="Abies magnifica")
wf = subset(all_data, Species=="Abies concolor")

aov(ratio~SizeClass*Band*Year, data = rf)

wilcox.test(ratio~Year, data=rf)

for(b in 2:5)
{
        print(b)
        bandx = subset(rf, Band==b)
        print(wilcox.test(ratio~Year, data=bandx))
}


#prop_rfwf = aggregate(data = all_data, avgperHA~Year+SizeClass+Band+Species, FUN=sum)
#allstuff = merge(all_data, prop_rfwf, by= c("Species", "Plot.St", "Year", "SizeClass"))

# library("vegan")
# 
# for(b in 2:5)
# {
#     bandx = subset(all_data, Band==b)
#     print(paste(b, wilcox.test(avgperHA~Year, data=bandx)))
# }
# 
# fit = aov(avgperHA~Year*Band*SizeClass, data = all_data)
# 
# rfpmav = with(all_data, adonis(avgperHA~Year*Band, data=all_data, distance="bray"))


#Getting percentage of total population that is in each size class
#Not for visualizations
# sumstuff = aggregate(data = all_data, avgperHA~Species+Band+Year, FUN=sum)
# sumstuff$SumHA = sumstuff$avgperHA
# allstuff = merge(all_data, sumstuff, by=c("Species","Band","Year"))
# allstuff$PercentageOf = allstuff$avgperHA.x/allstuff$SumHA*100
# write.xlsx(x = allstuff, file = "PercentageEachSizeClass.xlsx",sheetName = "sheet1", row.names = FALSE)



ggplot(data = all_data, aes(x = Band)) + 
    geom_bar(aes(y=avgperHA, fill=factor(SizeClass)),stat = "identity", position="fill")+
    xlab("Elevation Band (m)")+
    ylab("Percent") +
    ggtitle("Percent of total individuals in each size class by species and year") + 
    facet_grid(Year~Species) +
    scale_x_discrete(labels=getAllElevationBands()) +
    scale_y_continuous(labels=scales::percent) + 
    scale_fill_discrete(name="Size Class",
                        labels=c("Seedlings", "Saplings <1.37m tall", "10-20cm DBH", "20-40cm DBH", ">40cm DBH"))



#Species
all_data$SizeClassSpecies = paste0(all_data$Species, all_data$SizeClass)
all_data_no_0 = subset(all_data, Band!=1)
ggplot(data = all_data_no_0, aes(x = Band)) + 
    geom_bar(aes(y=avgperHA, fill=factor(Species)),stat = "identity", position="fill")+
    xlab("Elevation Band (m)")+
    ylab("Percent") +
    ggtitle("Percent of total individuals in each size class by species and year") + 
    facet_grid(SizeClass~Year) +
    scale_x_discrete(labels=getAllElevationBands()) +
    scale_y_continuous(labels=scales::percent) +
    scale_fill_discrete(name="Species")
#                    labels=c("Seedlings", "Saplings <1.37m tall", "10-20cm DBH", "20-40cm DBH", ">40cm DBH"))




ggplot(data = all_data, aes(x = Band)) + 
    geom_bar(aes(y=(avgperHA/sum(avgperHA)), fill=factor(SizeClass)),stat = "identity", position="fill")+
    xlab("Elevation Band (m)")+
    ylab("Percent") +
    ggtitle("Percent of total individuals in each size class by species and year") + 
    facet_grid(Species~Year) +
    coord_flip() + 
    scale_x_discrete(labels=getAllElevationBands()) +
    scale_y_continuous(labels=scales::percent) + 
    scale_fill_discrete(name="Size Class",
                        labels=c("Seedlings", "Saplings <1.37m tall", "10-20cm DBH", "20-40cm DBH", ">40cm DBH"))


all_data = subset(all_data, Band!=1)
seedlings = subset(all_data, SizeClass ==0)
other = subset(all_data, SizeClass !=0)

ggplot(data = seedlings, aes(x = Band)) + 
    geom_bar(aes(y=avgperHA, fill=factor(Year)), stat = "identity", position="dodge")+
    xlab("Elevation Band (m)")+
    ylab("Stems/HA") +
    ggtitle("Mean number of seedlings/HA by species and year") + 
    facet_grid(Species~SizeClass) +
    scale_x_discrete(labels=getAllElevationBands()) +
    scale_fill_discrete(name="Year")

ggplot(data = other, aes(x = Band)) + 
    geom_bar(aes(y=avgperHA, fill=factor(Year)), stat = "identity", position="dodge")+
    xlab("Elevation Band (m)")+
    ylab("Stems/HA") +
    ggtitle("Mean number of individuals/HA in each size class by species and year") + 
    facet_grid(Species~SizeClass) +
    scale_x_discrete(labels=getAllElevationBands()) +
    scale_fill_discrete(name="Year")

ggplot(data = all_data, aes(x = Band)) + 
    geom_bar(aes(y=avgperHA, fill=factor(SizeClass)), stat = "identity", position="dodge")+
    xlab("Elevation Band (m)")+
    ylab("Stems/HA") +
    ggtitle("Mean number of stems/HA by species, year, and size class") + 
    facet_grid(Species~Year) +
    scale_x_discrete(labels=getAllElevationBands()) +
    scale_fill_discrete(name="Size Class",
                        labels=c("Seedlings","Saplings <1.37m tall", "10-20cm DBH", "20-40cm DBH", ">40cm DBH"))




all_data$SizeYear = paste0(all_data$SizeClass, all_data$Year)

ggplot(data = all_data, aes(x = Band)) + 
    geom_bar(aes(y=(avgperHA/sum(avgperHA)), fill=factor(SizeYear)),stat = "identity", position="fill")+
    xlab("Elevation Band (m)")+
    ylab("Percent") +
    ggtitle("Percent of total individuals in each size class by species and year") + 
    facet_grid(~Species) +
    coord_flip() + 
    scale_x_discrete(labels=getAllElevationBands()) +
    scale_y_continuous(labels=scales::percent) + 
    scale_fill_discrete(name="Size Class",
                        labels=c("Seedlings", "Saplings <1.37m tall", "10-20cm DBH", "20-40cm DBH", ">40cm DBH"))


write.xlsx(x = all_data, file = "tabledSizeClass.xlsx",sheetName = "sheet1", row.names = FALSE)


#####
############# Close Connections ##################
#dbDisconnect(db)
#dbDisconnect(albertdb)