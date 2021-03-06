setwd("D:/Files/R Directory/Project")
visitors<-read.csv("visitors.csv")
slevel<-read.csv("slagg.csv")
avg.slevel<-data.frame(slevel$Year, slevel$Statewide.Average)
names(avg.slevel)[1]<-"Year"
names(avg.slevel)[2]<-"S.Level"
snv<-merge(avg.slevel, visitors, by="Year")
temp <-read.csv("temp.csv")
fl.temp<-read.csv("fltemp.csv")
tnf<-merge(temp, fl.temp, by="Year")
tnf$Delta.Ratio<-(tnf$Delta/tnf$Temp)
vnd<-merge(visitors, tnf, by="Year")
vrate<-read.csv("vrate.csv")
rnd<-merge(vrate, vnd, by="Year")
vxdr<-lm(Visitors ~ Delta.Ratio, data=vnd)
summary(vxdr)
vxd<-lm(Visitors ~ Delta, data=vnd)
summary(vxd)
vnd$Year<-(vnd$Year-1980)
time.vxdr<-lm(Visitors ~ Delta.Ratio + Year, data=vnd)
summary(time.vxdr)
rxdr<-lm(Visit.Rate ~ Delta.Ratio, data=rnd)
summary(rxdr)