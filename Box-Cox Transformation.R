library(AppliedPredictiveModeling)
data("segmentationOriginal")
segData <- subset(segmentationOriginal, Case == "Train")
library(e1071)
skewness(segData$AngleCh1)
segData <- segData[,-(1:3)]
skewValues <- apply(segData, 2, skewness)
skewValues
install.packages("caret")
library(caret)
ChiAreaTrans <- BoxCoxTrans(segData$AvgIntenCh1)
ChiAreaTrans
head(segData$AvgIntenCh1)
predict(ChiAreaTrans, head(segData$AvgIntenCh1))
skewness(predict(ChiAreaTrans, head(segData$AvgIntenCh1)))

