library("sqldf")
library("XLConnect")
library("ggplot2")
library("lattice")
library("reshape2")

################## Setup ##################
#Set working directory
setwd("/Users/emilyodean/CodeProjects/Ecological Modeling/Thesis")

#Utilities
if(!exists("multiplot", mode="function")) source("util.R")

#Connect to database
db = dbConnect(SQLite(), dbname="RFWFv1.sqlite")
dbListTables(db)


#SEEDLINGS#
understory = dbGetQuery(db,"select * from Regeneration where Species like 'WF' or Species like 'RF'")

#####for just YOSE#####
understory = subset(understory, substr(Plot.St, 1, 2) == "TR" | substr(Plot.St, 1, 2) == "GP")
###
#####for just Stanislaus#####
understory = subset(understory, substr(Plot.St, 1, 2) == "CT" | substr(Plot.St, 1, 2) == "HC")
######

understorysums = as.data.frame(aggregate(understory$Count~understory$Plot.St + understory$Species, FUN=sum))

############## How many WF to RF SEEDLINGS ###################
df = reshape(understorysums, idvar="understory$Plot.St", timevar="understory$Species", direction="wide")
names(df) = c("Plot.St", "RF", "WF")
df$ratio = df$RF/df$WF
df$band = substr(df$Plot.St, 4,4)
df = na.omit(df)


aggregate(ratio ~ band, df, mean)
aggregate(ratio ~ band, df, se)
mean(df$ratio)
se(df$ratio)

mean(df$RF)*100
se(df$RF)*100
mean(df$WF)*100
se(df$WF)*100

#SAPLINGS#
overstory = dbGetQuery(db,"select * from Overstory where Species like 'WF' or Species like 'RF'")
overstory=subset(overstory, DBH..cm. <10)

#for just YOSE
overstory = subset(overstory, Region == "Yosemite")
#for just Stanislaus
overstory = subset(overstory, Region == "Stanislaus")

overstorysums = as.data.frame(table(Plot.St=overstory$Plot.St, Species=overstory$Species))
df = reshape(overstorysums, idvar="Plot.St", timevar="Species", direction="wide")
names(df) = c("Plot.St", "RF", "WF")
df$ratio = df$RF/df$WF
df$band = substr(df$Plot.St, 4,4)

df = do.call(data.frame,lapply(df, function(x) replace(x, is.infinite(x) | x==0,NA)))
df = na.omit(df)

aggregate(ratio ~ band, df, mean)
aggregate(ratio ~ band, df, se)

mean(df$RF)*10
se(df$RF)*10
mean(df$WF)*10
se(df$WF)*10



################## Close Connections ##################
dbDisconnect(db)
dbDisconnect(albertdb)