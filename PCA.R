#Let us simulate a data that is highly correlated. Execute the following R code below to simulate a dataset

# Set the seed to create reproducible experiments
set.seed(98286)
# Create a sample of 100 values for xvar. Each value of xvar will range between 1 and 10.
xvar <- sample(1:10, 100, replace=TRUE)
# Copy the 100 values of xvar in to yvar
yvar <- xvar
# Let us replace 50 values of yvar as same as the xvar and replace the other 50 values of yvar to # be different from xvar
yvar[sample(1:length(yvar), 50)] <- sample(1:10, 50, replace=TRUE)
# Copy the 100 values of yvar in to zvar
zvar <- yvar
# Let us replace 50 values of zvar as same as the yvar and replace the other 50 values of zvar to # be different from yvar
zvar[sample(1:length(zvar), 50)] <- sample(1:10, 50, replace=TRUE)
# Bind xvar, yvar and zvar together
my.vars <- cbind(xvar, yvar, zvar)

# here one can easily infer the fact that Xvar, Yvar and Zvar are all 50% correlated

# Let us visualize a bivariate plot and correlation matrix between xvar and yvar
plot(yvar ~ xvar, data=jitter(my.vars))
cor(my.vars)

# Now let us perform PCA and obtain the principal components
my.pca <- prcomp(my.vars)
summary(my.pca)

my.pca

# From the rotation matrix we can infer the following

# In PC1 all the three variables are loaded ( negative sign is not important but they are all negative is what is important)
# In PC2, xvar and zvar are loaded in the opposite direction.
# In PC3 only yvar is loaded

# Now let us see if the components are uncorrelated
cor(my.pca$x) # determining the correlation between principal components

# See the off-diagonal values they are as good as zero indicating that the components are
# uncorrelated

# Visualization
biplot(my.pca) # View two-dimensional plot of data points wrt the first two components    
# overlaid with a projection of the variables on the component 

# The following observations can be made from the bi-plot above 
# Labels in the plot are row numbers
# Look for the angle and direction of the arrow
# yvar is closely aligned (strongly influence) to PC1
# xvar is correlated with yvar and zvar is correlated with yvar but xvar and zvar are not.

# Plot the bi-plot with principal components PC2 and PC3 
biplot(my.pca, choices = 2:3)
# The following observations can be made here
# zvar and xvar are closely aligned (strongly influence) to PC2
# yvar is closely aligned to PC3
# xvar, yvar and zvar are not correlated with each other.

# using the R script let us perform PCA on the brand perception dataset
# load the data
brand.ratings <- read.csv("http://goo.gl/IQl8nc")
# Let us quickly explore the dataset to see what it contains
summary(brand.ratings)
# On the raw data always remember to scale the data by subtracting the data points by its mean # and dividing it by its standard deviation
# Now let us transform the brand data
brand.sc <- brand.ratings
brand.sc[, 1:9] <- scale(brand.ratings[, 1:9])
# Let us explore the transformed brand perception dataset
summary(brand.sc)
# Now let us look at the correlation matrix
install.packages("corrplot")
library(corrplot)
corrplot(cor(brand.sc[, 1:9]), order="hclust") # hclust - reorder rows and columns according to # the variables similarity in a hierarchical cluster solution
# It can be inferred that the adjectives such as latest and trendy are highly correlated. Also value # seems to be highly correlated with bargain
# or you can simply call the cor()
cor(brand.sc[, 1:9])

# Now let us perform the PCA
## PCA for brand ratings
brand.pc <- prcomp(brand.sc[, 1:9])
summary(brand.pc)

# Here we have to make a decide on how many components to choose for modelling the brand
# perception dataset?

# To address this question use the Scree plot
plot(brand.pc, type="l")
# Here we see that the plot levels out at 3. So, we will choose the first three principal
# components.
# Let us now explore the first two principal components.
biplot(brand.pc) 
# What can we infer from this biplot shown above
# Serious, leader and perform are placed together which can be termed as leadership
# Rebuy, value and bargain are placed together which can be termed as value
# Trendy and latest are placed together which can be termed as Latest
# Fun is on its own

# The above plot looks very cluttered so let us aggregate the instances to establish the mean ratings for each perceptual attribute across each brand
# aggregate each perceptual attribute by brand
brand.mean <- aggregate(. ~ brand, data=brand.sc, mean)
# use brand for the row names
rownames(brand.mean) <- brand.mean[, 1] 
# remove brand name column
brand.mean <- brand.mean[, -1]      
# Display the brand means    
brand.mean
# Now let us perform PCA on the brand.mean dataset
# Always good to keep scale=TRUE
brand.mu.pc <- prcomp(brand.mean, scale=TRUE)
summary(brand.mu.pc)
# The first three PC's can explain about 90.64% of the variance in the dataset.

# Now let us create a biplot to visualize - Perceptual map
biplot(brand.mu.pc, main="Brand positioning", cex=c(1.5, 1)) 
# Perceptual maps help you see where the brands are positioned with respect to the first two principal components

# What conclusions can I make from the above figure
# Brands f and g are high in value.
# Brands a and j are high in fun.
#. and so on 

# Now let us see some strategic implications of using the Perceptual map

# Adjusting Strategic goals
#I want my brand "e" to be close to "c"
brand.mean["c", ] - brand.mean["e", ]
# I want my brand "e" to be positioned between brands "b", "c", "f" and "g"
colMeans(brand.mean[c("b","c","f","g"), ]) - brand.mean["e", ]










