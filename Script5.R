# Given my lack of determining a decent model, I figured I'd look into some
# new data such as staewide sea-level or just raw statewide temperature.

# I have pulled Florida Statewide Average Temperature data from the Florida
# Climate Center and sea level data from the NOAA Center for Operational
# Oceanographic Products and Services. Their data comes from monitoring 
# stations around the state. I havee taken the average of each periods
# measurement across each of the stations to compute a staewide average.
# This is perhaps not the most accurate thing to do from an oceanographic
# perspective but I find it to be a good enough surrogate for my purposes.
# Their data also comes monthly, for the sake of simplicity I am using only
# those data points from the month of January, which I chose randomly.

setwd("D:/Files/R Directory/Project")
temp<-read.csv("temp.csv")
visitors<-read.csv("visitors.csv")
slevel<-read.csv("slagg.csv")
avg.slevel<-data.frame(slevel$Year, slevel$Statewide.Average)
names(avg.slevel)[1]<-"Year"
names(avg.slevel)[2]<-"S.Level"
fl.temp<-read.csv("fltemp.csv")
tnv<-merge(temp, visitors, by="Year")
snv<-merge(slevel, visitors, by="Year")
fnv<-merge(fl.temp, visitors, by="Year")
snf<-merge(avg.slevel, fl.temp, by="Year")
all<-merge(snf, tnv, by="Year")
all$Year<-(all$Year-1980)

# Lets start with all the Simple Linear Ones

vxs<-lm(Visitors ~ S.Level, data=all)
summary(vxs)
# Good
time.vxs<-lm(Visitors ~ S.Level + Year, data=all)
summary(time.vxs)
# Out - Insignifigant Coeff
vxf<-lm(Visitors ~ Fl.Temp, data=all)
summary(vxf)
# Out - Insignifigant Coeff
time.vxf<-lm(Visitors ~ Fl.Temp + Year, data=all)
summary(time.vxf)
# Out - Insignifigant Coeff
vxt<-lm(Visitors ~ Temp, data=all)
summary(vxt)
# Good Enough - Insignifigant Intercept
vxsf<-lm(Visitors ~ S.Level + Fl.Temp, data=all)
summary(vxsf)
# Out - Insignifigant Coeff
vxst<-lm(Visitors ~ S.Level + Temp, data=all)
summary(vxst)
# Out - Insignifigant Coeff

# vxs is a good model. It has a high r.squared, it doesnt improve at all by
# adding a time term, both the intercept and the coefficient are signifigant,
# and the overall regression scores a very high f statistic. Ironically,
# this is the data set that underwent the most manipulation by me.

exp.vxf<-lm(Visitors ~ exp(Fl.Temp), data=all)
summary(exp.vxf)
# This was to follow Grdic and Nizic as closely as I can. Visitors regressed
# on air temps. Low r.squared and insignifigant coefficient. This is
# Out