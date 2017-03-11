setwd("/Users/emilyodean/CodeProjects/Ecological Modeling/Thesis")

library(ggplot2)
library(vegan)
library(xlsx)
library("sqldf")
library("XLConnect")

#Load and organize the data

#Utilities
if(!exists("multiplot", mode="function")) source("util.R")

#Connect to database
db = dbConnect(SQLite(), dbname="RFWFv1.sqlite")
albertdb = dbConnect(SQLite(), dbname="Albertv1.sqlite")


######MATURE TREES######
overstorymature = dbGetQuery(db,"select * from Overstory where [DBH..cm.]>10 and band!=1")
aoverstorymature = dbGetQuery(albertdb,"select * from Overstory")

#overstorymature = dbGetQuery(db,"select * from Overstory where [DBH..cm.]>10 and (band !=1 and band !=2)")
#aoverstorymature = dbGetQuery(albertdb,"select * from Overstory where (band !=2)")


#######SEEDLINGS#######
#Just yose again
#seedlings =  dbGetQuery(db,"select * from Regeneration where band!=1 and (substr([Plot.St],1,2) like 'TR' or substr([Plot.St],1,2) like 'GP')")
seedlings =  dbGetQuery(db,"select * from Regeneration where Band!=1")
aseedlings = dbGetQuery(albertdb,"select * from Sapling")

names(aseedlings)[1] = "Plot.St"
names(aseedlings)[6] = "Count"
plotsbands = unique(aoverstorymature[, c('Plot.St', 'Band')])
aseedlings = merge(aseedlings, plotsbands, by="Plot.St", all=FALSE)

#######SAPLINGS#######
aseedlings[is.na(aseedlings)] = 0

asaplings = data.frame(Plot.St = aseedlings$Plot.St,
                       Species = aseedlings$Species,
                       Count = aseedlings$Lg.Sapl+aseedlings$Sm.Sapl)
saplings = dbGetQuery(db,"select * from Overstory where band!=1")
saplings$Plot.St = as.factor(saplings$Plot.St)
saplings = subset(saplings, saplings$DBH..cm.<=10) #Have to do it this way so we have all levels of a factor

aseedlings = data.frame(Plot.St = aseedlings$Plot.St,
                        Species = aseedlings$Species,
                        Count = aseedlings$Count)

#GPB1P1, GPB5P2, TRB3P1


#####Checking how many Albert's plots in each band
df = unique(aoverstorymature[,c('Plot.St', 'Band')])
table(df$Band)
#2  3  4  5 
#1 11 13  5 

df = unique(overstorymature[,c('Plot.St', 'Band')])
df = df[order(df$Plot.St),]
######

#1981
amature = as.data.frame.matrix(table(aoverstorymature$Plot.St, aoverstorymature$Species))
aSumRFMature = amature$RF#*(10000/2700) #This puts it in TPH
aSumWFMature = amature$WF#*(10000/2700)
aSumOther = amature$JP + amature$LP + amature$PM + amature$SP

#aSumJPMature = amature$JP
#aSumSPMature = amature$SP

asaplings = as.data.frame.matrix(table(asaplings$Plot.St, asaplings$Species))
aSumRFSapling = asaplings$RF
aSumWFSapling = asaplings$WF
aSumOtherSapling = asaplings$JP + asaplings$LP + asaplings$PM + asaplings$SP

aseedlings = as.data.frame.matrix(table(aseedlings$Plot.St, aseedlings$Species))
aSumRFSeedling = aseedlings$RF
aSumWFSeedling = aseedlings$WF
aSumOtherSeedling = aseedlings$JP + aseedlings$LP + aseedlings$PM + aseedlings$SP

#2016
presentmature = as.data.frame.matrix(table(overstorymature$Plot.St, overstorymature$Species))
SumRFMature = presentmature$RF
SumWFMature = presentmature$WF
SumOther = as.numeric(presentmature$BO) + as.numeric(presentmature$IC) + as.numeric(presentmature$JP) + 
    as.numeric(presentmature$LP) + as.numeric(presentmature$PP) + as.numeric(presentmature$QA) + 
    as.numeric(presentmature$SAF) + as.numeric(presentmature$SP) + as.numeric(presentmature$WWP)

#SumJPMature = presentmature$JP
#SumSPMature = presentmature$SP


saplings = as.data.frame.matrix(table(saplings$Plot.St,saplings$Species))
SumRFSapling = saplings$RF
SumWFSapling = saplings$WF
SumOtherSapling = as.numeric(presentmature$BO) + as.numeric(presentmature$IC) + as.numeric(presentmature$JP) + 
    as.numeric(presentmature$LP) + as.numeric(presentmature$PP) + as.numeric(presentmature$QA) + 
    as.numeric(presentmature$SAF) + as.numeric(presentmature$SP) + as.numeric(presentmature$WWP)

merge(aggregate(value ~ code, dat, min), dat, by = c("code", "value"))

aggregate(Count ~ Plot.St, data = subset(seedlings, Species=='RF'), FUN=sum)
seedlings = as.data.frame.matrix(table(seedlings$Plot.St, seedlings$Species))
SumRFSeedling = seedlings$RF
SumWFSeedling = seedlings$WF
SumOtherSeedling = seedlings$BO + seedlings$IC + seedlings$JP + seedlings$LP + seedlings$PP + seedlings$SP + seedlings$WWP


#DFs for each
mature = data.frame(RFMature = c(aSumRFMature, SumRFMature), 
                  WFMature = c(aSumWFMature, SumWFMature),
                  Other = c(aSumOther, SumOther))

# mature = data.frame(RFMature = c(aSumRFMature, SumRFMature), 
#                     WFMature = c(aSumWFMature, SumWFMature),
#                     JPMature = c(aSumJPMature, SumJPMature),
#                     WFMature = c(aSumSPMature, SumSPMature))



sapling = data.frame(RFSapling = c(aSumRFSapling, SumRFSapling),
                  WFSapling = c(aSumWFSapling, SumWFSapling),
                  Other = c(aSumOtherSapling, SumOtherSapling))
                  
seedling = data.frame(RFSeedling = c(aSumRFSeedling, SumRFSeedling),
                  WFSeedling = c(aSumWFSeedling, SumWFSeedling),
                  Other = c(aSumOtherSeedling, SumOtherSeedling))

#groups = data.frame(Year=c(rep(1981, length(aSumRFMature)), rep(2016, length(SumRFMature))),
 #                   Plot=c(sort(unique(aoverstorymature$Plot.St)), sort(unique(overstorymature$Plot.St))),
 #                   Band=c(unique(aoverstorymature[,c('Plot.St', 'Band')])$Band, df$Band))

groups = data.frame(Plot=c(sort(unique(aoverstorymature$Plot.St)), sort(unique(overstorymature$Plot.St))),
                    Year=c(paste0("1981_", unique(aoverstorymature[,c('Plot.St', 'Band')])$Band), paste0("2016_", df$Band)))


#MRPP won't run if all entries are 0
blank = which(seedling$RFSeedling==0 & seedling$WFSeedling==0 & seedling$Other == 0)
seedling = seedling[-blank,]
seedlinggroups = groups[-blank,]

blank = which(sapling$RFSapling==0 & sapling$WFSapling==0 & sapling$Other == 0)
sapling = sapling[-blank,]
saplinggroups = groups[-blank,]

groups$Year = as.factor(groups$Year)
saplinggroups$Year = as.factor(saplinggroups$Year)
seedlinggroups$Year = as.factor(seedlinggroups$Year)


#MRPP
mature.mrpp = with(groups,mrpp(dat=mature,grouping=Year,distance = "bray")) 
mature.mrpp  

sapling.mrpp = with(saplinggroups,mrpp(dat=sapling,grouping=Year,distance = "bray")) 
sapling.mrpp  

seedling.mrpp = with(seedlinggroups,mrpp(dat=seedling,grouping=Year,distance = "bray")) 
seedling.mrpp  

#Permutational Multivariate Analysis of Variance
#https://sites.ualberta.ca/~ahamann/teaching/renr690/Lab6.pdf
mature.pmav = with(groups, adonis(mature~Year*Band, data=mature, distance="bray"))
sapling.pmav = with(saplinggroups, adonis(sapling~Year*Band, data=sapling, distance="bray"))
seedling.pmav = with(seedlinggroups, adonis(seedling~Year*Band, data=seedling, distance="bray"))

#Below: plot the results as NMDS  

#For saplings
mature = sapling
groups = saplinggroups

#For seedlings
mature = seedling
groups = seedlinggroups


#Step 1: run an ordination procedure so we have something to plot
mature.ord=metaMDS(mature, try = 100) #100 iterations

ordiplot(mature.ord,type='t')
stressplot(mature.ord)


##################Using Base R Here###########################
colvec = c("red", "green3", "blue", "darkgoldenrod1", "darkturquoise", "black", "purple", "deeppink1")
scl = 3
groups$fYear = groups$Year
levels(groups$fYear) = c("1981, 1830-1979m", "1981, 1980-2129m", "1981, 2130-2284m", "1981, 2285-2440m",
                         "2016, 1830-1979m", "2016, 1980-2129m", "2016, 2130-2284m", "2016, 2285-2440m")
palette(colvec)

# plot NMDS using basic plot function and color points by "amt" from MyMeta
#plot(mature.ord$points, col=groups$Year, pch=groups$Year)

plot(mature.ord, type="n")
#points(mature.ord$points, col = groups$fYear, pch =  c(1:8)[as.numeric(groups$fYear)])
points(mature.ord$points, col = rgb(0, 0, 0, max = 255, alpha = 125), pch =  c(1:8)[as.numeric(groups$fYear)])
title(main = "Nonmetric Multidimensional Scaling - Seedlings", cex.main=.9)
with(mature.ord, legend("bottomright", legend = levels(groups$fYear), bty = "n",
                      col = colvec, pch =c(1:8), pt.bg=colvec, cex=0.7))
# draw dispersion ellipses around data points

lev = levels(groups$fYear)

for (i in seq_along(lev)) {
   ordiellipse(mature.ord, groups$fYear, kind="se", conf=0.95,
                               lwd=2,
                               lty=2,
                               col=colvec[i], ## ith colour
                               show.groups = lev[i]) ## for ith group
}



##########Matt's Code##############


#originally had mature.ord = metaMDS(mature) per example in ?MRPP() but failed 
#so rewrote as ord = mature.ord...

#Code below adapted from: https://oliviarata.wordpress.com/2014/04/17/ordinations-in-ggplot2/

#envfit >>> Honestly don't know what's going on in this part
mature.envfit <- envfit(mature.ord, env = groups, perm = 999) #standard envfit
mature.envfit

#data for plotting 
##NMDS points
mature.NMDS.data<-groups 
mature.NMDS.data$NMDS1<-mature.ord$points[ ,1] #this puts the NMDS scores for the plots into a new dataframe. you could put them into an existing one if you preferred.
mature.NMDS.data$NMDS2<-mature.ord$points[ ,2] 

##species data
stems<-colSums(mature) #total abundances for each species
spps <- data.frame(scores(mature.ord, display = "species")) #dataframe of species scores for plotting
spps$species <- row.names(spps) # making a column with species names
spps$colsums <- stems #adding the colSums from above
spps<-spps[!is.na(spps$NMDS1) & !is.na(spps$NMDS2),] #removes NAs
spps.colmedian <- median(spps$colsums) #create an object that is the median of the abundance of the measured species
spps.colmean <- mean(spps$colsums) #creates a mean instead if you wish to use

#spps2 <- subset(spps,spps$colsums > spps.colmean) #select the most abundant species. Could discard fewer by going something like - spps$colsums>(spps.colmedian/2) instead
spps2 = spps
spps2$species <- factor(spps2$species) #otherwise factor doesn't drop unused levels and it will throw an error

# data for the envfit arrows
env.scores.mature <- as.data.frame(scores(mature.envfit, display = "vectors")) #extracts relevant scores from envifit
env.scores.mature <- cbind(env.scores.mature, env.variables = rownames(env.scores.mature)) #and then gives them their names

# function for ellipsess - just run this, is used later
#taken from the excellent stackoverflow Q+A: http://stackoverflow.com/questions/13794419/plotting-ordiellipse-function-from-vegan-package-onto-nmds-plot-created-in-ggplo
veganCovEllipse <- function (cov, center = c(0, 0), scale = 1, npoints = 100) 
{
    theta <- (0:npoints) * 2 * pi/npoints
    Circle <- cbind(cos(theta), sin(theta))
    t(center + scale * t(Circle %*% chol(cov)))
}

#data for ellipse, in this case using the management factor
df_ell.mature.groups <- data.frame() #sets up a data frame before running the function.
mature.NMDS.data$Year = as.factor(mature.NMDS.data$Year)
for(g in levels(mature.NMDS.data$Year)){
    df_ell.mature.groups <- rbind(df_ell.mature.groups, cbind(as.data.frame(with(mature.NMDS.data [mature.NMDS.data$Year==g,],
                                 veganCovEllipse(cov.wt(cbind(NMDS1,NMDS2),wt=rep(1/length(NMDS1),length(NMDS1)))$cov,center=c(mean(NMDS1),mean(NMDS2)))))
                                                              ,Year=g))
}

for(g in levels(NMDS$group)){
    df_ell <- rbind(df_ell, cbind(as.data.frame(with(NMDS[NMDS$group==g,],
                                veganCovEllipse(ord[[g]]$cov,ord[[g]]$center,ord[[g]]$scale))),group=g))
}


# data for labelling the ellipse
NMDS.mean.mature=aggregate(mature.NMDS.data[ ,c("NMDS1", "NMDS2")], 
                           list(group = mature.NMDS.data$Year), mean)

# data for labelling the ellipse
NMDS.mean=aggregate(mature.NMDS.data[,c("NMDS1", "NMDS2")], 
                    list(group = mature.NMDS.data$Year), mean)

df_ell.mature.groups$Year = as.factor(df_ell.mature.groups$Year)
mature.NMDS.data$Year

## finally plotting. 
mult <- 2 #multiplier for the arrows and text for envfit below. You can change this and then rerun the plot command.
png(file="mature_ord_test.png",width = 700,height=700)
(mature.nmds.gg1 <- ggplot(data = mature.NMDS.data, aes(y = NMDS2, x = NMDS1))+ #sets up the plot. brackets around the entire thing to make it draw automatically
     geom_path(data = df_ell.mature.groups, aes(x = df_ell.mature.groups$NMDS1, y = df_ell.mature.groups$NMDS2, alpha=Year))+ #this is the ellipse, seperate ones by Site. If you didn't change the "alpha" (the shade) then you need to keep the "group 
     scale_alpha_manual(guide = FALSE,values=c(1,1,1,1,1,1,1,1,1))+ #sets the shade for the ellipse
     geom_point(aes(shape =Year), size = 3) + #puts the site points in from the ordination, shape determined by site, size refers to size of point
     #geom_text(data=spps2, aes(x=spps2$NMDS1, y=spps2$NMDS2, label=species), size = 3.3, hjust=1.1)+ #labelling the species. hjust used to shift them slightly from their points
     #annotate("text",x = NMDS.mean$NMDS1,y = NMDS.mean$NMDS2+.05,label=NMDS.mean$group,size=6) + #labels for the centroids - I haven't used this since we have a legend. but you could also dithc the legend, but plot will get v messy
     #geom_point(data=spps2, alpha = .6, shape = 4)+ #these are the species points, made lighter and a specific shape
     scale_shape_manual(values = c(1,2,3,4,5,6,7,8))+ #sets the shape of the plot points instead of using whatever ggplot2 automatically provides
     coord_cartesian(xlim = c(-1.5,2))+  ## NB this changes the visible area of the plot only (this is a good thing, apparently). Can also specify ylim. Here in case you want to set xaxis manually.
     theme_bw()
#theme(legend.text=element_text(size=17),legend.position="none")#removed legend for space
dev.off()    



ggplot(data = mature.NMDS.data, aes(y = NMDS2, x = NMDS1))+ #sets up the plot. brackets around the entire thing to make it draw automatically
    geom_path(data = df_ell.mature.groups, aes(x = df_ell.mature.groups$NMDS1, y = df_ell.mature.groups$NMDS2, colour=Year))+ #this is the ellipse, seperate ones by Site. If you didn't change the "alpha" (the shade) then you need to keep the "group 
    scale_alpha_manual(guide = FALSE,values=c(1,1,1,1,1,1,1,1,1))+ #sets the shade for the ellipse
    geom_point(aes(shape=Year), size = 7) + #puts the site points in from the ordination, shape determined by site, size refers to size of point
    #geom_text(data=spps2, aes(x=spps2$NMDS1, y=spps2$NMDS2, label=species), size = 3.3, hjust=1.1)+ #labelling the species. hjust used to shift them slightly from their points
    annotate("text",x = NMDS.mean$NMDS1,y = NMDS.mean$NMDS2+.05,label=NMDS.mean$group,size=6) + #labels for the centroids - I haven't used this since we have a legend. but you could also dithc the legend, but plot will get v messy
    scale_shape_manual(values = c(1,2,3,4,5,6,7,8,9))
    #geom_point(data=spps2, alpha = .6, shape = 4)+ #these are the species points, made lighter and a specific shape
    #scale_shape_manual(values = c(1,8))+ #sets the shape of the plot points instead of using whatever ggplot2 automatically provides
    #coord_cartesian(xlim = c(-1.5,2))+  ## NB this changes the visible area of the plot only (this is a good thing, apparently). Can also specify ylim. Here in case you want to set xaxis manually.
    #theme_bw()
    #scale_shape_identity() 


plot(mature.ord)


