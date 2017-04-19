# This is for various forms of transformation on VxS and some noodling with
# LVxT

setwd("D:/Files/R Directory/Project")
temp <-read.csv("temp.csv")
visitors<-read.csv("visitors.csv")
slevel<-read.csv("slagg.csv")
avg.slevel<-data.frame(slevel$Year, slevel$Statewide.Average)
names(avg.slevel)[1]<-"Year"
names(avg.slevel)[2]<-"S.Level"
snv<-merge(avg.slevel, visitors, by="Year")
tnv <-merge(temp, visitors, by="Year")
tnv$Year<-(tnv$Year-1980)

time.lvxt<-lm(log(Visitors) ~ Temp + Year, data=tnv)
summary(time.lvxt)

# Again, adding a time term makes the Temp term completely irrelevant

lvxs<-lm(log(Visitors) ~ S.Level, data=snv)
vxls<-lm(Visitors ~ log(S.Level), data=snv)
lvxls<-lm(log(Visitors) ~ log(S.Level), data=snv)
summary(lvxs)
summary(vxls)
summary(lvxls)

# Log(S.Level) is producings NaN's so I am uncomfortable in using it 

# LVxT and LVxS are both good models, however
