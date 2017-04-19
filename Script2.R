# This script is to test the relationship between population growth 
# and my variables
setwd("D:/Files/R Directory/Project")
temp<-read.csv("temp.csv")
visitors<-read.csv("visitors.csv")
Pop<-read.csv("world.csv")
pnt<-merge(Pop, temp, by="Year")
pnv<-merge(Pop, visitors, by="Year")
txp<-lm(Temp ~ Pop, data=pnt)
vxp<-lm(Visitors ~ Pop, data=pnv)
lvxp<-lm(log(Visitors) ~ Pop, data=pnv)
ltxp<-lm(log(Temp) ~ Pop, data=pnt)
summary(txp)$r.squared
summary(ltxp)$r.squared
summary(vxp)$r.squared
summary(lvxp)$r.squared
x11()
with(pnv, plot(Pop, log(Visitors), main="World Pop Drives Florida Tourism"))
# Tourism and World Population have a very strong relationship, 
# and the percent change in World Population does a good job predicting change
# in Temperature
vrate<-read.csv("vrate.csv")
rnt<-merge(vrate, temp, by="Year")
rxt<-lm(Visit.Rate ~ Temp, data=rnt)
summary(rxt)$r.squared
x11()
with(rnt, plot(Temp, Visit.Rate, 
               ylab="Percent of World Population who visit Florida",
               xlab="Average Global Deviation from a refference temperature",
               main="Effects of Climate Change on Tourism"
))
# The last data set I created myself. I took Florida Tourism and divided
# it by world population for every year. This controls for the fact that
# world population growth is a general driver of tourism growth
# Now to take care of the time trend bias
rnt$Year<-(rnt$Year-1979)
time.rxt<-lm(Visit.Rate ~ Temp + Year, data=rnt)
summary(time.rxt)$r.squared
# This is a model I can work with
summary(time.rxt)

# CONLCUSION: World Population Growth has a strong relationship with
#             Tourism, created new data set to remove this effect
