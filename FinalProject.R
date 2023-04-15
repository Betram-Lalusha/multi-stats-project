
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