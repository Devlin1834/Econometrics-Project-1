# Vector Autoregression
# The plan now is to shift more towards forecasting in my model. After looking
# at how the time trend nullified the usefulness of my Temp term and how the
# exponential approach yeilded un-interpretable results I have begun thinking
# that prediction within the range of my data is useless anyways if we consider
# that the next years data and the years after that will exceed the bounds
# of my current data anyways. What does it matter if I can predict tourism
# in 1995 given temp data if we already have hard data on tourism in 1995?

setwd("D:/Files/R Directory/Project")
temp<-read.csv("temp.csv")
visitors<-read.csv("visitors.csv")
tnv<-merge(temp, visitors, by="Year")
tnv$Year<-(tnv$Year-1980)
tnv$Temp1 = tnv$Temp
tnv$Temp2 = tnv$Temp
tnv$Visitors1 = tnv$Visitors
tnv$Visitors2 = tnv$Visitors


shift<-function(x, n){ c(x[-(seq(n))], rep(NA, n))}
tnv$Temp1 <- shift(tnv$Temp1, 1)
tnv$Temp2 <- shift(tnv$Temp2, 2)
tnv$Visitors1 <- shift(tnv$Visitors1, 1)
tnv$Visitors2 <- shift(tnv$Visitors2, 2)
vec1.vxt<-lm(Visitors ~ Visitors1 + Temp1, data=tnv)
summary(vec1.vxt)
vec2.vxt<-lm(Visitors ~ Visitors1 + Visitors2 + Temp1 + Temp2, data=tnv)
summary(vec2.vxt)

# While both of these models give hefty r.squared, most of the coefficients
# and intercepts aren't signifigant. This does not help me much and there's 
# not much point in estimating the second equation when the first one doesn't
# help me already. 