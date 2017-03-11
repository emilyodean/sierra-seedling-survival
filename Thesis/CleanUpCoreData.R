library("XLConnect")
library("openxlsx")

setwd("/Users/emilyodean/CodeProjects/Ecological Modeling/Thesis")

trees = openxlsx::read.xlsx("/Users/emilyodean/Documents/Research Data/RFWFData - 10_19_16.xlsx", sheet=1)
cores = openxlsx::read.xlsx("/Users/emilyodean/Documents/Research Data/RFWFData012417.xlsx", sheet = 3)

names(cores)[2] = "Plot-St"
names(cores)[7] = "Core"

cores = subset(cores, cores$)

merged = merge(cores, trees, by=c("Plot-St", "Core"))
df = data.frame(merged$[Plot-St], merged$Species.x, merged$DBH, merged$#.Rings)