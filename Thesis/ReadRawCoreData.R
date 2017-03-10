library(xlsx)

#Set your working directory to a folder that only holds the core csv's
setwd("/Users/emilyodean/Documents/Research Data/Core Data/Tioga")

#Get the names of all the files in the directory
file_names = dir()

#Create an empty data frame (fill this with your variables that you want to extract)
core_data = data.frame(Site = character(0),
                       Plot.St = character(0),
                       Region = character(0),
                       CanopyClass = character(0),
                       DBH = character(0), #I'm doing this as a string in case we have some weird characters or typos - don't want it to error out
                       Species = character(0),
                       FirstYear = numeric(0),
                       DCH = numeric(0),
                       Age = numeric(0),
                       Band = numeric(0),
                       stringsAsFactors=FALSE)

#Loop through each file in the folder
for(file in file_names)
{
    #Read in each csv
    data = read.csv(file)
    
    #The variables extracted here are from the name of the file. For me, I want to use these in 
    #each row that is associated with this particular file, so I extract them outside of my
    #loop that goes through each column
    
    ####Emily's Variables####
    #Exctract sampling location
    site = substr(file, 1, 2)
    
    #Extract elevation band
    band = substr(file, 4, 4)
    
    #Extract plot name
    plot = substr(file, 1, 6)
    
    #For each column in the CSV, extract some information
    #Start on the second column - first is the years
    for (i in 2:ncol(data))
    {
        #Get the column name
        name = colnames(data)[i] 
        
        ####Emily's Variables####
        #Extract Species
        species = substr(name, 1, 2)
        
        #Extract DBH
        dbh = substr(name, 4, nchar(name))  
        
        #Extract Canopy Class
        canopy = substr(name, 3, 3)
        
        #Has DCH (if the last character of the dbh is an asterisk, which gets interpreted as a period)
        dch = 0
        if(substr(dbh, nchar(dbh), nchar(dbh)) == "."){
            dch = 1
            dbh = substr(dbh, 1, nchar(dbh)-1)
        } 
        
        
        #####This is probably the most useful part - find the first row that isn't NA in each column,
        #####and then extract the corresponding date from the first column   
        date = data[min(which(!is.na(data[,i]), arr.ind=TRUE)), 1]
        
        #We may want the age too
        age = 2016 - date
        
        #Fill up the data frame
        core_data = rbind(core_data, data.frame(Site = site,
                                                Plot.St = plot,
                                                Region = "YOSE", #hard coded for now
                                                CanopyClass = canopy,
                                                DBH = dbh,
                                                Species = species,
                                                FirstYear = date,
                                                DCH = dch,
                                                Age = age,
                                                Band = band))
    }        
}

#Write all of this to disk
write.xlsx(x = core_data, file = "cores.xlsx")


