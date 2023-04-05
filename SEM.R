satSimData <- read.csv("http://goo.gl/MhghRq")
summary(satSimData)
library(lavaan)
library(semTools)
library(semPlot)

# Let us now define the model
satModel <- " Quality =~ CSat + Value + q1 + q2 + q3 + 0*Cost
Cost =~ Value + Repeat + c1 + c2 + c3
Value =~ CSat + v1 + v2 + v3
CSat =~ Repeat + cs1 + cs2 + cs3
Repeat =~ r1 + r2 + r3 "

sat.fit <- sem(satModel, data= satSimData, std.lv=TRUE)
summary(sat.fit, fit.measures=TRUE)

semPaths(sat.fit, what="est", fade=FALSE, residuals=FALSE,
         layout="tree", structural=TRUE, nCharNodes=7,
         edge.label.cex=1)

satAltModel <- " Quality =~ CSat + q1 + q2 + q3 + 0*Cost
Cost =~ Value + c1 + c2 + c3
Value =~ CSat + v1 + v2 + v3
CSat =~ Repeat + cs1 + cs2 + cs3
Repeat =~ r1 + r2 + r3 "

satAlt.fit <- sem(satAltModel,
                  data=satSimData, std.lv=TRUE)
summary(satAlt.fit , fit.measures=TRUE)

semPaths(satAlt.fit, what="est", fade=FALSE, residuals=FALSE,
         layout="tree", structural=TRUE, nCharNodes=7,
         edge.label.cex=1)

compareFit(sat.fit, satAlt.fit, nested=TRUE)
summary(compareFit(sat.fit, satAlt.fit, nested=TRUE))
