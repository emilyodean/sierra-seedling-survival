#Database Joins
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
dbListTables(db)


### Organize overstory and understory data ###
overstory = dbGetQuery(db,"select * from Overstory")
understory = dbGetQuery(db,"select * from Regeneration")

understorysums = as.data.frame(aggregate(understory$Count~understory$Plot.St + understory$Species, FUN=sum))
overstorysums = as.data.frame(table(overstory$Plot.St, overstory$Species))

#Only do this once
#dbWriteTable(conn = db, name = "UnderstorySums", value = understorysums, row.names = FALSE)
#dbWriteTable(conn = db, name = "OverstorySums", value = overstorysums, row.names = FALSE)

dbListFields(db, "UnderstorySums")
dbListFields(db, "UL_Values")

dbGetQuery(db,"select * from UnderstorySums")
ul = dbGetQuery(db,"select * from UL_Values")

### Join UL data with overstory and understory data ###
#joinedunderstory = dbGetQuery(db,"SELECT *
#                    FROM UnderstorySums u
#                   JOIN UL_Values ul
#                    ON u.[understory$Plot.St]=ul.[Plot.St.........];")

names(understorysums) = c("Plot.St", "Species", "TreeCount")
names(overstorysums) = c("Plot.St", "Species", "TreeCount")

#UL data is annoyingly organized and has to have whitespace trimmed
names(ul)[1] = "Plot.St"
ul.df = data.frame(Plot.St = ul$Plot.St, CanopyOpen = ul$X..Cnpy.Open...)
ul.df = as.data.frame(apply(ul.df,2,function(x)gsub('\\s+', '',x)))

umerge = merge(understorysums, ul.df, by="Plot.St")
omerge = merge(overstorysums, ul.df, by="Plot.St")

omerge$CanopyOpen = as.numeric(omerge$Canopy)
umerge$CanopyOpen = as.numeric(umerge$Canopy)

#throw the band back in there
omerge$Band = substr(omerge$Plot.St, 4, 4)
umerge$Band = substr(umerge$Plot.St, 4, 4)

################### Do some analysis #####################
plot(TreeCount ~ CanopyOpen, data = umerge)

uwf = subset(umerge, Species=="WF")
urf = subset(umerge, Species=="RF")
owf = subset(omerge, Species=="WF")
orf = subset(omerge, Species=="RF")

rfwf = subset(umerge, Species=="WF" | Species=="RF")

uwfmodel = lm(uwf$TreeCount ~ uwf$CanopyOpen)
urfmodel = lm(urf$TreeCount ~ urf$CanopyOpen)


# Both species on the same plot
urfwfplot =    ggplot(rfwf, aes(CanopyOpen, TreeCount, colour = factor(Species))) +
        geom_point() + 
        ggtitle("Abies concolor and magnifica regeneration vs canopy openness") +
        facet_grid(. ~Band)

# Each species on different plots
uwfplot =    ggplot(uwf, aes(CanopyOpen, TreeCount)) +
        geom_point() + 
        ggtitle("Abies concolor regeneration vs canopy openness") +
        facet_grid(. ~Band)


urfplot =    ggplot(urf, aes(CanopyOpen, TreeCount)) +
        geom_point() + 
        ggtitle("Abies magnifica regeneration vs canopy openness")

owfplot =    ggplot(owf, aes(CanopyOpen, TreeCount)) +
        geom_point() + 
        ggtitle("Abies concolor overstory vs canopy openness")

orfplot =    ggplot(orf, aes(CanopyOpen, TreeCount)) +
        geom_point() + 
        ggtitle("Abies magnifica overstory vs canopy openness")


multiplot(uwfplot, urfplot)




################## Close Connections ##################
dbDisconnect(db)
dbDisconnect(albertdb)