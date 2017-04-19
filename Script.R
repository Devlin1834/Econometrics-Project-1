# My data on visitors comes from The Bureau of Economic and Business
# Research. My data on Temp Anomalies comes from the National
# Climate Data Center, which is run by NOAA.

# When it comes to naming I have chosen to designate ever data.frame by
# joining the first letter of their relevent data sets with the letter n
# i.e. tnv is read T and V for Temp and Visitors
# Regressions are denoted similarly but with an x
# i.e. vxt is V by T for Visitors regressed on Temp
# i.e. vxlt is V by log(T) for Visitors regreesed on log(Temp)

setwd("D:/Files/R Directory/Project")
temp <-read.csv("temp.csv")
visitors<-read.csv("visitors.csv")
tnv <-merge(temp, visitors, by="Year")
vxt <-lm(Visitors ~ Temp, data=tnv)
summary(vxt)
lvxt <-lm(log(Visitors) ~ Temp, data=tnv)
summary(lvxt)
vxlt <-lm(Visitors ~ log(Temp), data=tnv)
summary(vxlt)
lvxlt <-lm(log(Visitors) ~ log(Temp), data=tnv)
summary(lvxlt)
#What I have found is that the basic level-level model best describes the 
#relationship between Temp and Visitors
with(tnv, plot(Temp, Visitors,
               ylab="Florida Tourism",
               xlab="Temperature",
               main="Effects of Climate Change on Tourism"
))
x11()
with(tnv, hist(Temp))
x11()
with(tnv, hist(Visitors))
#A few notes on what I've done and what I want to do
#1. I have evaluated all the different variable manipulations to see which 
#   best fits the relationship
#2. I would like to use the log-level model as it better handles the larger
#   values in Visitors, but it doesnt describe the relationship as well as
#   the level-level model. I think I'll stick to that one for interpretaional 
#   simplicity
#3. I have come to realize that both temperature and visitors can be driven by
#   population growth. I would like to find population growth data and 
#   run regressions to determine their effects on my variables, as well as
#   learn more about time-series regression.
#4. It strikes me that an easy way to filter out the effects of population
#   growth on tourism is to use a ratio of visitors to world population. 
#   I should look into that

# CONCLUSION: use the level-level VxT, though LVxT is a good second