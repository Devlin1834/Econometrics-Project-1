# And here is where I realize that by including a time trend, my p-value for 
# Temp has skyrocketed and I am better off leaving it out of my model. This,
# obviously, is counterproductive to my investigation so I am looking into
# alternative methods of improving my model

# It is worth noting that Grdic and Nizic used an exponential model without
# a time trend to predict number of tourists based off air temperature
# in Croatia. The following analysis follows their example.

# At this point I have largely decided that using Visit.Rate with a time trend
# term is redundant. I'm looking for a reason to keep using it, if I don't 
# find one, I'm switching back to Visitors. 

setwd("D:/Files/R Directory/Project")
temp<-read.csv("temp.csv")
visitors<-read.csv("visitors.csv")
vrate<-read.csv("vrate.csv")
rnt<-merge(vrate, temp, by="Year")
tnv<-merge(temp, visitors, by="Year")
rnt$Year<-(rnt$Year-1980)
tnv$Year<-(tnv$Year-1980)
time.rxt<-lm(Visit.Rate ~ Temp + Year, data=rnt)
summary(time.rxt)
time.vxt<-lm(Visitors ~ Temp + Year, data=tnv)
summary(time.vxt)
# I'm siwtching back to Visitors away from Vsit.Rate
exp.vxt<-lm(Visitors ~ exp(Temp), data=tnv)
ln.vxt<-lm(log(Visitors) ~ Temp, data=tnv)
summary(exp.vxt)
summary(ln.vxt)

# exp.vxt yeilds a higher r.squared, gives me workable p-values on all my
# coefficients, and more closely follows the model used by Grdic and Nizic.
# My only qualm is that the model itself makes little sense. When graphed
# it forms a right angle that hugs quadrant 3, and with a negative
# intercept estimate it always predicts negative tourist numbers.

# CONCLUSION: Thrown out time trend term and exponential model. Looking to 
#             forecast.

# Next I plan on looking into Vector Autoregression
