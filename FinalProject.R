
##load data 
nbaStats = read.csv("NBA Stats 202223.csv")

##see variables in data
names(nbaStats)
head(nbaStats)

### variable selection

###forward selection
library("MASS")
res.full = lm(ORtg~DRtg+VI+P.R.A+P.A+P.R+TPG+BPG+SPG+APG+RPG+PPG+TS.+eFG.+X3P.+X3PA+X2P.+X2PA+FT.+FTA+TO.+USG.+MPG+GP+AGE+POS, data=nbaStats)
res.null = lm(ORtg~1,data=nbaStats)

forward = step(res.null, scope=list(upper=res.full), direction="forward", test="F")

##model after variable selection
resModel = lm(ORtg~X2PA+PPG+SPG+DRtg+GP+FT.+BPG+P.R+P.R.A+AGE+POS, data=nbaStats)

###model summary
summary(resModel)


###model analysis

###extract residuals
err = resid(resModel)
fit.v = resModel$residuals

###residual plots
plot(fit.v,err,xlab="Fitted Values",ylab="Residuals")
plot(nbaStats$X2PA,err,xlab="X2PA",ylab="Residuals")
plot(nbaStats$PPG,err,xlab="PPG",ylab="Residuals")
plot(nbaStats$SPG,err,xlab="SPG",ylab="Residuals")
plot(nbaStats$DRtg,err,xlab="DRtg",ylab="Residuals")
plot(nbaStats$GP,err,xlab="GP",ylab="Residuals")
plot(nbaStats$FT.,err,xlab="FT.",ylab="Residuals")
plot(nbaStats$BPG.,err,xlab="BPG",ylab="Residuals")
plot(nbaStats$P.R,err,xlab="P.R",ylab="Residuals")
plot(nbaStats$P.R.A,err,xlab="P.R.A",ylab="Residuals")
plot(nbaStats$AGE.,err,xlab="AGE",ylab="Residuals")
plot(nbaStats$POS.,err,xlab="POS",ylab="Residuals")




