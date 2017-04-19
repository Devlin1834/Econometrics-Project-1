# In order to deal with the problem of spurrious regression I would like to
# return to looking at Visit.Rate.

setwd("D:/Files/R Directory/Project")
temp<-read.csv("temp.csv")
visitors<-read.csv("visitors.csv")
vrate<-read.csv("vrate.csv")
plot(vrate)

# Visit.Rate still trends upwards, even after accounting for population growth
# Now I want to construct a data set that uses the Temp anomies frome earlier
# as a percent of florids annual average temperature

fl.temp<-read.csv("fltemp.csv")
tnf<-merge(temp, fl.temp, by="Year")
tnf$PCT<-(tnf$Temp/tnf$Fl.Temp)
rates<-merge(tnf, vrate, by="Year")
plot(rates$PCT)
rxp<-lm(Visit.Rate ~ PCT, data=rates)
summary(rxp)

slevel<-read.csv("slagg.csv")
avg.slevel<-data.frame(slevel$Year, slevel$Statewide.Average)
names(avg.slevel)[1]<-"Year"
names(avg.slevel)[2]<-"S.Level"
all<-merge(rates, avg.slevel, by="Year")
rxall<-lm(Visit.Rate ~ PCT + S.Level, data=all)
summary(rxall)
rxs<-lm(Visit.Rate ~ S.Level, data=all)
summary(rxs)
all$Year<-(all$Year-1980)

time.rxs<-lm(Visit.Rate ~ S.Level + Year, data=all)
summary(time.rxs)

# while RxP seems promising, adding a trend term negates the signifigance of
# PCT and brings me back to a spurrious regression.