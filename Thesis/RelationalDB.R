#install.packages("sqldf")
#install.packages("XLConnect")

library("sqldf")
library("XLConnect")

#Set working directory and load excel sheets
setwd("/Users/emilyodean/CodeProjects/Ecological Modeling/Thesis")

#This code is for creating the db
data = loadWorkbook("AlbertsData.xlsx")
tables = readWorksheet(data, sheet=getSheets(data))

#Set up database
db = dbConnect(SQLite(), dbname="Albertv1.sqlite")

#Get the current names of the tables (for use in the following function)
names(tables)

#Insert data into database
#with(tables, {
#    dbWriteTable(conn = db, name = "Overstory", value = Overstory, row.names = FALSE)
#    dbWriteTable(conn = db, name = "Regeneration", value = Regeneration, row.names = FALSE)
#    dbWriteTable(conn = db, name = "Plot_Notes", value = Plot_Notes, row.names = FALSE)
#    dbWriteTable(conn = db, name = "UL_Info", value = UL_Info, row.names = FALSE)
#})

with(tables, {
    dbWriteTable(conn = db, name = "Sapling", value = Sapl_Seedl, row.names = FALSE)
    dbWriteTable(conn = db, name = "Overstory", value = Tree_Layer, row.names = FALSE)

})

#Check that it worked
dbListTables(db)
dbListFields(db, "Overstory")
dbReadTable(db, "Plot_Notes")

#Example db query
p1 = dbGetQuery(db,"select * from Overstory where Species like '%WF%'")



#Database Joins
db = dbConnect(SQLite(), dbname="RFWFv1.sqlite")
dbListTables(db)

dbGetQuery(db,"select * from UL_Values")

#Close connection before leaving
dbDisconnect(db)

