# factoextra can be installed and loaded as follow:
# First install the devtools package
install.packages("devtools")
# Now install the factoextra package as shown below
devtools::install_github("kassambara/factoextra")
# After the installation is complete then load the library
library("factoextra")
# Also install and load the FactoMineR package
install.packages("FactoMineR")
library(FactoMineR)

# load the data
data(decathlon2)
#Let us take a subset of the rows and columns from the decathlon2 dataset
decathlon2.active <- decathlon2[1:23, 1:10]
# View the first few instances from this dataset
head(decathlon2.active[, 1:10])
# Now let us perform a PCA on this dataset using the prcomp() - Remember to set scale=TRUE
res.pca <- prcomp(decathlon2.active, scale = TRUE)
# Let us look in to the PCA results
# output sdev: the standard deviations of the principal components (the square roots of the
# eigenvalues)
res.pca$sdev
# display the matrix of variable loadings (columns are eigenvectors) 
res.pca$rotation[, 1:10]
# The variance retained by each principal component can be obtained from the eigenvalues
  # Recall that eigenvalues measure the variability retained by each PC. It's large for the first PC # and small for the subsequent PCs.
  eig <- (res.pca$sdev)^2
eig
# Let us now obtain the variance in percentage
variance <- eig*100/sum(eig)
variance
# Now let us compute the cumulative variance
# Cumulative variances
cumvar <- cumsum(variance)
eig.decathlon2.active <- data.frame(eig = eig, variance = variance, cumvariance = cumvar)
eig.decathlon2.active
# The importance of PCs can be visualized using a scree plot. Scree plot using base graphics:
barplot(eig.decathlon2.active[, 2], names.arg=1:nrow(eig.decathlon2.active), 
        main = "Variances",
        xlab = "Principal Components",
        ylab = "Percentage of variances",
        col ="steelblue")
# Add connected line segments to the plot
lines(x = 1:nrow(eig.decathlon2.active), 
      eig.decathlon2.active[, 2], 
      type="b", pch=19, col = "red")
# Alternative way to obtain Scree plot
fviz_screeplot(res.pca, ncp=10)
# To view the eigenvalues in the scree plot instead of the variance
fviz_screeplot(res.pca, ncp=10, choice="eigenvalue")
var <- get_pca_var(res.pca)
var

#The correlation between a variable and a principal component (PC) is used as the coordinates # of the variable on the PC. So let us plot the variables on the correlation circle
# Create a Bi-plot using the first two PC's. Graph of variables using R base graph
# Plot the correlation circle
# Plot the correlation circle
a <- seq(0, 2*pi, length = 100)
plot( cos(a), sin(a), type = 'l', col="gray",
      xlab = "PC1",  ylab = "PC2")
abline(h = 0, v = 0, lty = 2)
# Add active variables
arrows(0, 0, var$coord[, 1], var$coord[, 2], 
       length = 0.1, angle = 15, code = 2)
# Add labels
text(var$coord, labels=rownames(var$coord), cex = 1, adj=1)

# Obtain the variables factor map using the factoextra package
fviz_pca_var(res.pca)

# cor is the correlations between the variables and the dimensions
var$cor # output will be similar to what you see above (var$coord)
# cos2 is used to determine the quality of the factor map
# The cos2 of variables are calculated as the squared coordinates : var$cos2 # = var$coord * var$coord
var$cos2 

# contrib provides the contributions of the variables to the principal components
var$contrib

# The contribution of a variable to a given principal component is (in percentage) computed as
# (var$cos2 * 100) / (total cos2 of the component)
# R script to manually compute the contribution of the variables to the principal components
var.cos2 <- var$coord^2
Comp.cos2 <- apply(var.cos2, 2, sum)
contrib <- function(var.cos2, comp.cos2){var.cos2*100/comp.cos2}
var.contrib <- t(apply(var.cos2,1, contrib, Comp.cos2))
# print the contributions
var.contrib


# Highlight and distinguish the most important (contributing) variables
# Control variable colors using their contributions
fviz_pca_var(res.pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)

# Let us see the top 10 contributing variables to PC1 and PC2
# Contributions of variables to PC1 
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)

# Now let us see the contributions of variables to PC2 
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)

# Now let us analyze the individuals
ind <- get_pca_ind(res.pca)
ind
# Let us see the factor map of individuals	
fviz_pca_ind(res.pca)

# Now let us construct a biplot of the individual and the variables together
fviz_pca_biplot(res.pca,  geom = "text") +
  theme_minimal()
