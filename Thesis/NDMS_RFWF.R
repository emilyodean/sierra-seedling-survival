setwd("/Users/emilyodean/CodeProjects/Ecological Modeling/Thesis")

library(ggplot2)
library(vegan)
library(xlsx)
library("sqldf")
library("XLConnect")

#Load and organize the data

#Utilities
if(!exists("multiplot", mode="function")) source("util.R")


#all = read.xlsx("/Users/emilyodean/CodeProjects/Ecological Modeling/Thesis/NMDSData.xlsx", sheet=1)

#Scaled to TPH
all = read.xlsx("/Users/emilyodean/CodeProjects/Ecological Modeling/Thesis/NMDSData.xlsx", sheet=2)


seedling = all[,4:6]
sapling = all[,7:9]
mature = all[,10:12]
    
groups = all[,c(1,13)]
#or
#groups = all[,c(1:3)]

#Clean up 0 entries  - MRPP won't run if all entries are 0
blank = which(seedling$RFSeedlingSum==0 & seedling$WFSeedlingSum==0 & seedling$OtherSeedlingSum == 0)
seedling = seedling[-blank,]
seedlinggroups = groups[-blank,]

blank = which(sapling$RFSaplingSum==0 & sapling$WFSaplingSum==0 & sapling$OtherSaplingSum == 0)
sapling = sapling[-blank,]
saplinggroups = groups[-blank,]

#Make Year a factor for all groups
groups$Year = as.factor(groups$YearBand)
saplinggroups$Year = as.factor(saplinggroups$YearBand)
seedlinggroups$Year = as.factor(seedlinggroups$YearBand)


#If we use the other grouping type
#groups$Year = as.factor(groups$Year)
#saplinggroups$Year = as.factor(saplinggroups$Year)
#seedlinggroups$Year = as.factor(seedlinggroups$Year)

#groups$Band = as.factor(groups$Band)
#saplinggroups$Band = as.factor(saplinggroups$Band)
#seedlinggroups$Band = as.factor(seedlinggroups$Band)


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

#For saplings - the lazy way of doing this
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
points(mature.ord$points, col = groups$fYear, pch =  c(1:8)[as.numeric(groups$fYear)])
#points(mature.ord$points, col = rgb(0, 0, 0, max = 255, alpha = 125), pch =  c(1:8)[as.numeric(groups$fYear)])
title(main = "Nonmetric Multidimensional Scaling - Mature trees (>10cm DBH)", cex.main=.9)
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


