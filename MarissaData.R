#I removed the description of what age classes meant in the historical data file (just for ease of data cleaning). Easier to just manually enter that back in at the end.

#install.packages("xlsx") #Uncomment this line if you don't have this package installed already
library("xlsx")

#Read in historical data
historical = read.xlsx("Clean Historical Data Complete.xlsx", 6, header=TRUE, colClasses=NA)

#Read in plot8 data. NOTE:I hard coded which row to stop for this, so if you had more datasheets you'd need to change it
plot8 = read.xlsx("MOC Plot 8 Quadrat Tree Growth.xlsx", 1, header=TRUE, endRow=781, colClasses=NA) 

#Height Class 1=current year, 2=older but <10cm, 3 = 10-24.99cm, 4=25-49.99 cm, 
#5=50-137 cm, 6=sapling (<10cm dbh), 7=tree(>=10cm dbh)

numrows = nrow(plot8)
data = matrix(nrow=numrows, ncol=6) #store data in a matrix, and then merge w/dataframe at the end (more efficient than appending to DF)

for (x in 1:numrows) #go through each row
{
    #extract quadrat, year, prelogging, species, heightclass, count 
    quadrat = plot8[x, 1]
    year = plot8[x,4]
    prelogging = TRUE #This is an assumption on my part - are all of the old data points from pre-logging?
    species = plot8[x,3]
     
    for (i in 1:12) #go through each year in each row
    {
        col = i*3 + 2 #keep track of which column we're working in
        heightclass
        count
    }
    
    i=1 #start over w/ columns
}
