#Appendix 1. R code to calculate the distance from a grid cell with a climate value for the present 
#to climate match in the future (forward calculation). This code is easiest to understand but fairly
#slow (about 5 hours to process 1 million grid cells).

library(SDMTools)       # install package to read and write ESRI ASCII grids
present <- asc2dataframe("C:\Your Path\MAT6190.asc")
future  <- asc2dataframe("C:\Your Path\MAT2020s.asc")

t <- 0.25               # plus/minus threshold to define climate match

x <- present$x                    # vector of grid cell x coordinates
y <- present$y                    # vector of grid cell y coordinates
p <- present$var.1                # vector of present climate values for xy coordinates
f <- future$var.1                 # vector of future climate values for xy coordinates 
d <- vector(length=length(p))     # empty vector to write distance to climate match
id <- 1:length(p)                 # index number from first to last grid cell

for (i in 1:length(p)) {                 # loop for all grid cells of p
    ti <- id[abs(f-p[i])<t]               # index of future grid cells within threshold
    di <- (x[ti]-x[i])^2+(y[ti]-y[i])^2   # calculates distances for indexed cells
    d[i] <- sqrt(min(di))                 # finds the shortest geographic distance
}

# writes out log10 speed and distance multiplied by 100 in ESRI ASCII format
# conversion: -200=0.01km, -100=0.1km, 0=1km, 100=10km, 200=100km etc.
d[d==Inf] <- 100000  # sets no analogue to 10,000km
out=cbind(y,x,logDist=round(log10(d)*100),logSpeed=round(log10(d/50)*100))
dataframe2asc(out)


#Appendix 2. R code to calculate the distance from a grid cell with a climate value 
#for the present to climate match in the future (forward calculation). This code is more 
#efficient using a rounding operation on the data to implement thresholds, and subsequently 
#creating list of unique climate values with their climate matches (about 20 minutes to process 1 million cells).


library(SDMTools)       # install package to read and write ESRI ASCII grids
present <- asc2dataframe("C:\Your Path\MAT6190.asc")
future  <- asc2dataframe("C:\Your Path\MAT2020s.asc")

t <- 0.25               # plus/minus threshold to define climate match
t <- 1/(t*2)            # inverse for rounding, double for plus/minus

x <- present$x                    # vector of grid cell x coordinates
y <- present$y                    # vector of grid cell y coordinates
p <- round(present$var.1*t)/t     # vector of rounded present climate values 
f <- round(future$var.1*t)/t      # vector of rounded future climate values 
d <- vector(length=length(p))     # empty vector to write distance to climate match

u     <- unique(p)[order(unique(p))]    # list of unique climate values in p
match <- function(u){c(which(u==f))}    # function finding climate matches of u with f
m     <- sapply(u, match)               # list of climate matches for unique values

for(i in 1:length(p)){                  # loop for all grid cells of p
    mi   <- m[[which(u==p[i])]]          # recalls list of climate matches for p[i]
    d[i] <- sqrt(min((x[i]-x[mi])^2 + (y[i]-y[mi])^2))    # distance to closest match
}

# writes out log10 speed and distance multiplied by 100 in ESRI ASCII format
# conversion: -200=0.01km, -100=0.1km, 0=1km, 100=10km, 200=100km etc.
d[d==Inf] <- 100000  # sets no analogue to 10,000km
out=cbind(y,x,logDist=round(log10(d)*100),logSpeed=round(log10(d/50)*100))
dataframe2asc(out)

#Appendix 3. Multivariate extension of the R code shown in Appendix 2. This sample also 
#uses a more efficient k-nearest neighbor search, and writes out source and target coordinates
#in a table for further analysis. The variables p1 and p2 represent principle components, 
#but they could stand for any climate variable (about 5 minutes to process 1 million grid cells). 


library(SDMTools)     # install package to read and write ESRI ASCII grids
library(yaImpute)     # install package for k-nearest neighbour (kNN) search

setwd("/Users/emilyodean/CodeProjects/Ecological Modeling/Thesis/Climate Velocity/gcb12736-sup-0004-AppendixS4")

present1 <- asc2dataframe("PC1-6190.asc") # principal component grids
present2 <- asc2dataframe("PC2-6190.asc")
future1  <- asc2dataframe("PC1-2020s.asc")
future2  <- asc2dataframe("PC2-2020s.asc")

idxy <- cbind(id=1:nrow(present1),present1[,1:2])   # data frame of IDs and XY coords
b <- (max(present1$var.1)-min(present1$var.1))/120  # bin size for 120 PC1 bins

p1 <- round(present1$var.1/b)              # convert PC1 to 120 bins via rounding
p2 <- round(present2$var.1/b)              # convert PC2 to <120 bins via rounding
f1 <- round(future1$var.1/b)               # same for future PC1
f2 <- round(future2$var.1/b)               # same for future PC2
p  <- paste(p1,p2)                         # PC1/PC2 combinations in present climate
f  <- paste(f1,f2)                         # PC1/PC2 combinations in future climate
u  <- unique(p)[order(unique(p))]          # list of unique PC1/PC2 combinations

sid <- c()                                 # empty vector for source IDs
tid <- c()                                 # empty vector for target IDs
d   <- c()                                 # empty vector for distances

for(i in u){                          # loop for each unique PC1/PC2 combination
    pxy <- idxy[which(p==i),]           # coordinates of i-th combination in present
    fxy <- idxy[which(f==i),]           # coordinates of i-th combination in future
    sid <- c(sid, pxy$id)               # append i-th PC1/PC2 combination to previous 
    
    if(nrow(fxy)>0){                    # kNN search unless no-analogue climate
        knn <- data.frame(ann(as.matrix(fxy[,-1]), as.matrix(pxy[,-1]), k=1)$knnIndexDist)      
        tid <- c(tid, fxy[knn[,1],"id"]) # the IDs of the closest matches  
        d <- c(d, sqrt(knn[,2]))         # their corresponding geographic distances
    }
    else {                              # else statement for no-analogue climates
        tid <- c(tid, rep(NA,nrow(pxy))) # flag destinations as missing for no analogues
        d <- c(d, rep(Inf,nrow(pxy)))    # flag distances as infinity for no analogues
    }
}

sxy <- merge(sid, idxy, by.y="id", all.x=T, all.y=F, sort=F)[2:3]  # source coordinates
txy <- merge(tid, idxy, by.y="id", all.x=T, all.y=F, sort=F)[2:3]  # target coordinates
names(txy)=c("target_y","target_x")


# write output table in CSV format with source and target coordinates and distances
outtab <- cbind(id=sid, sxy, txy, distance=d)   
write.csv(outtab, "output.csv", row.names=F)


# writes out log10 velocities and distances multiplied by 100 in ESRI ASCII format
# conversion: -200=0.01km, -100=0.1km, 0=1km, 100=10km, 200=100km etc.
out=merge(present1[,1:2], outtab[,c(2,3,6)], by=c("y","x"), sort=F)
out$distance[out$distance==Inf] <- 10000  # sets no analogue to 10,000km
out$distance[out$distance==0] <- 0.5  # sets zero distance to 0.5km (1/2 cell size)
out$logDist=round(log10(out$distance)*100)
out$logSpeed=round(log10(out$distance/50)*100)
dataframe2asc(out)

#Appendix S5. R code to calculate distance-based climate change velocities 
#(forward calculation – for reverse velocities swap present and future datasets). 
#This code is for a multiple variables (here: principal components), and uses a 
#fast k-nearest neighbor search algorithm. This version of the program uses multiple 
#runs with slightly offset bins of climate matches to reduce boundary artifacts 
#(about 20 minutes to process 1 million grid cells). 

library(SDMTools)     # install package to read and write ESRI ASCII grids
library(yaImpute)     # install package for k-nearest neighbor (kNN) search

present1 <- asc2dataframe("C:\Your Path\PC1_6190.asc") # principal component grids
present2 <- asc2dataframe("C:\Your Path\PC2_6190.asc")
future1  <- asc2dataframe("C:\Your Path\PC1_2020s.asc")
future2  <- asc2dataframe("C:\Your Path\PC2_2020s.asc")

idxy=cbind(id=1:nrow(present1),present1[,1:2])   # data frame of IDs and XY coordinates
b=(max(present1$var.1)-min(present1$var.1))/120  # bin size for 120 PC1 bins

runs <- 3                                  # number of smoothing runs (user defined)
offsets <- seq(0,b,b/runs)[1:runs]         # offset values for runs
results <- data.frame(id=1:nrow(present1)) # empty data frame for results

for (o in offsets) {
    
    p1 <- round((present1$var.1+o)/b)        # convert PC1 to 120 bins via rounding
    p2 <- round((present2$var.1+o)/b)        # convert PC2 to <120 bins via rounding
    f1 <- round((future1$var.1+o)/b)         # same for future PC1
    f2 <- round((future2$var.1+o)/b)         # same for future PC2
    p  <- paste(p1,p2)                       # PC1/PC2 combinations in present climate
    f  <- paste(f1,f2)                       # PC1/PC2 combinations in future climate
    u  <- unique(p)[order(unique(p))]        # list of unique PC1/PC2 combinations
    
    sid <- c()                               # empty vector for source IDs
    tid <- c()                               # empty vector for target IDs
    d   <- c()                               # empty vector for distances
    
    for(i in u){                         # loop for each unique PC1/PC2 combination
        pxy <- idxy[which(p==i),]          # coordinates of i-th combination in present
        fxy <- idxy[which(f==i),]          # coordinates of i-th combination in future
        sid <- c(sid, pxy$id)              # append i-th PC1/PC2 combination to previous 
        
        if(nrow(fxy)>0){                   # kNN search unless no-analogue climate
            knn <- data.frame(ann(as.matrix(fxy[,-1]), as.matrix(pxy[,-1]), k=1)$knnIndexDist)      
            tid <- c(tid, fxy[knn[,1],"id"]) # the IDs of the closest matches  
            d <- c(d, sqrt(knn[,2]))         # their corresponding geographic distances
        }
        else {                             # else statement for no-analogue climates
            tid <- c(tid, rep(NA,nrow(pxy))) # flag destinations as missing for no analogues
            d <- c(d, rep(Inf,nrow(pxy)))    # flag distances as infinity for no analogues
        }
    }
    
    d <- cbind(id=sid, distance=d)  
    results <- merge(results, d, by=c("id"))
}

results$mean=rowMeans(results[2:runs+1])

# writes out log10 velocities and distances multiplied by 100 in ESRI ASCII format
# conversion: -200=0.01km, -100=0.1km, 0=1km, 100=10km, 200=100km etc.
out=merge(idxy, results[,c(1,runs+2)], by=c("id"), sort=F)
out$mean[out$mean==Inf] <- 10000  # sets no analogue to 10,000km
out$mean[out$mean==0] <- 0.5  # sets zero distance to 0.5km (1/2 cell size)
out$logDist=round(log10(out$mean)*100)
out$logSpeed=round(log10(out$mean/50)*100)
dataframe2asc(out[,c(2:3,5:6)])




