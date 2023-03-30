# Read the dataset from the link provided below. The dataset is available as a CSV file.
piesSimData <- read.csv("http://goo.gl/yT0XwJ")
summary(piesSimData)
# Now let us estimate the PIES CFA model
# Install the following packages
install.packages(c("lavaan", "semTools", "semPlot"))  
library(lavaan)
library(semTools)
library(semPlot)
# Let us now define the model
# define the hierarchical structural model
piesModel <- "General =~ i1 + i2 + i3
Feature =~ i4 + i5 + i6 + i7
Image   =~ i8 + i9 + i10 + i11
PIES =~ General + Feature + Image"
# =~ means "is estimated by"
# Now let us perform CFA
pies.fit <- cfa(piesModel, data=piesSimData)
summary(pies.fit, fit.measures=TRUE)
# Now let us plot using the SEMPLOT
library(semPlot)
semPaths(pies.fit, what="est", fade=FALSE, residuals=FALSE, edge.label.cex=0.75)


# using the R script let us perform PCA on the brand perception dataset
# load the data
brand.ratings <- read.csv("http://goo.gl/IQl8nc")
# Let us quickly explore the dataset to see what it contains
summary(brand.ratings)
# Before using the brands dataset you have to first transform the dataset
brand.sc <- brand.ratings
brand.sc[, 1:9] <- scale(brand.ratings[, 1:9])
# Let us now determine the number of factors
install.packages("nFactors")
library(nFactors)
nScree(brand.sc[, 1:9])
# Let us now obtain all the eigenvalues
eigen(cor(brand.sc[, 1:9]))
# Now let us obtain the 3-factor solution
factanal(brand.sc[, 1:9], factors=3)
# The following inferences can be made from this EFA analysis
#Factor 1 is about bargain and value and can be coined as "value"
#Factor 2 is about leader, perform and serious and cab coined as "leader".
#Factor 3 is about "latest" and "trendy" and can be coined as "trendy" 
#Factor 1, 2 and 3 together can explain 56.8% variance in the dataset.
# Now let us focus on EFA rotation. By default, the rotation is "varimax". "varimax" means that # the dimensional axes are perpendicular. "oblique" means that factors can be correlated also 
# known as "oblimin".
# Now let us perform the "oblimin" rotation
install.packages("GPArotation")
library(GPArotation)
(brand.fa.ob <- factanal(brand.sc[, 1:9], factors=3, rotation="oblimin"))
# Compared to the previous EFA (with Varimax rotation) this analysis suggests that
#Compared to "varimax" the "oblimin" loading are slightly different.
#However, it is still a 3-factor model with factors "value", "leader" and "latest".
#Factor 1 is negatively correlated with Factor 2 and essentially not correlated with Factor 3.
# Now let us use Heatmaps to visualize the EFA solution
# Make sure you have gplots and RColorBrewer installed in your computer
library(gplots)
library(RColorBrewer)
heatmap.2(brand.fa.ob$loadings, col=brewer.pal(9, "Greens"), trace="none", key=FALSE, dend="none", Colv=FALSE, cexCol = 1.2, main="\n\n\n\n\nFactor loadings for brand adjectives")
# Now let us plot the path diagram
Install.packages("semPlot")
library(semPlot)
semPaths(brand.fa.ob, what="est", residuals=FALSE, cut=0.3, posCol=c("white", "darkgreen"), negCol=c("white", "red"),          edge.label.cex=0.75, nCharNodes=7)

#install.packages("semPlot", dependencies=TRUE)

