# Lets look into using Sea Level to do Vector Autoregression

setwd("D:/Files/R Directory/Project")
visitors<-read.csv("visitors.csv")
slevel<-read.csv("slagg.csv")
avg.slevel<-data.frame(slevel$Year, slevel$Statewide.Average)
names(avg.slevel)[1]<-"Year"
names(avg.slevel)[2]<-"S.Level"
snv<-merge(avg.slevel, visitors, by="Year")

shift<-function(x, n){ c(x[-(seq(n))], rep(NA, n))}

snv$S.Level1 = snv$S.Level
snv$S.Level2 = snv$S.Level
snv$Visitors1 = snv$Visitors
snv$Visitors2 = snv$Visitors
snv$S.Level1 <-shift(snv$S.Level1, 1)
snv$S.Level2 <-shift(snv$S.Level2, 2)
snv$Visitors1 <-shift(snv$Visitors1, 1)
snv$Visitors2 <-shift(snv$Visitors2, 2)

vec1.vxs<-lm(Visitors ~ Visitors1 + S.Level1, data=snv)
summary(vec1.vxs)
vec2.vxs<-lm(Visitors ~ Visitors1 + Visitors2 + S.Level1 + S.Level2, data=snv)
summary(vec2.vxs)

# These are all useless, full of insignifigant coefficients