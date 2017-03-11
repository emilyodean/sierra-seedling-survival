library("sqldf")
library("XLConnect")
library("ggplot2")
library("vcd")
library("MASS")

################## Setup ##################

#Set working directory
setwd("/Users/emilyodean/CodeProjects/Ecological Modeling/Thesis")

#Connect to database
db = dbConnect(SQLite(), dbname="RFWFv1.sqlite")
albertdb = dbConnect(SQLite(), dbname="Albertv1.sqlite")

sqldf("pragma table_info(Overstory)", dbname = "RFWFv1.sqlite")$name   

#I am removing size class 1 here to keep my data consistent with Albert's
rfoverstory = dbGetQuery(db,"select * from Overstory where species like 'RF' and [DBH..cm.] > 10")
wfoverstory = dbGetQuery(db,"select * from Overstory where species like 'WF' and [DBH..cm.] > 10")
arfoverstory = dbGetQuery(albertdb,"select * from Overstory where species like 'RF'")
awfoverstory = dbGetQuery(albertdb,"select * from Overstory where species like 'WF'")

################## Probability density function ###################
par(mfrow=c(1,1))

#dinvexp(rfoverstory$DBH..cm)
control = abs(rnorm(1000000))

fit1 = fitdistr(rfoverstory$DBH..cm, "exponential")
fit2 = fitdistr(control, "exponential")

ks.test(rfoverstory$DBH..cm, control)
ks.test(arfoverstory$DBH..cm, "pexp")

ks.test(control, "pexp", fit2$estimate)

hist(rfoverstory$DBH..cm, freq = FALSE, breaks = 100, xlim = c(0, quantile(rfoverstory$DBH..cm, 0.99)))
curve(dexp(x, rate = fit1$estimate), col = "red", add = TRUE)



hist(rfoverstory$DBH..cm, breaks=30)






hist(wfoverstory$DBH..cm, breaks=30)

hist(arfoverstory$DBH..cm, breaks=30)
hist(awfoverstory$DBH..cm, breaks=30)


################## Close Connections ##################
dbDisconnect(db)
dbDisconnect(albertdb)
