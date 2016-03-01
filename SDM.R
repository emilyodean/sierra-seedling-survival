#SDM

#Using file that comes w/ the dismo library
library(dismo)
file = paste(system.file(package="dismo"), "/ex/bradypus.csv", sep="")

#Read in the file
bradypus <- read.table(file, header=TRUE, sep=",")

#Global biodiversity package
wf = gbif("abies", "concolor", geo=FALSE)
rf = gbif("abies", "magnifica", geo=FALSE)

#Strip out records w/o lon/lat coordinates
wf.geo = subset(wf, !is.na(lon) & !is.na(lat) & country == "United States")
rf.geo = subset(rf, !is.na(lon) & !is.na(lat))

#Plot the world (simple)
library(maptools)
data(wrld_simpl)
plot(wrld_simpl, xlim=c(-130,-90), ylim=c(30,50), axes=TRUE, col="light yellow")
#plot(wrld_simpl, axes=TRUE, col="light yellow")

#Plot points
points(wf.geo$lon, wf.geo$lat, col='orange', pch=20, cex=0.75)
points(rf.geo$lon, rf.geo$lat, col='red', cex=0.75)

#Remove duplicates
dups = duplicated(wf.geo[, c('species', 'lon', 'lat')])
wfg <- wf.geo[!dups, ]
dups2 = duplicated(rf.geo[, c('species', 'lon', 'lat')])
rfg <- wf.geo[!dups2, ]

library(raster)
climate = getData('worldclim', var='tmax', res=0.5, lon=120, lat=38.17)

file = paste(system.file(package="dismo"), "/ex/bio1.grd", sep="")
