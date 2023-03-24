library(modeldata)
data(ames, package = "modeldata")
library(earth)
library(dplyr)
# Fit a basic MARS model
mars1 <- earth(
  Sale_Price ~ .,  
  data = ames 
)

# Print model summary
print(mars1)
summary(mars1) %>% .$coefficients %>% head(10)
plot(mars1, which = 1)

# Fit a basic MARS model
mars2 <- earth(
  Sale_Price ~ .,  
  data = ames,
  degree = 2
)

# check out the first 10 coefficient terms
summary(mars2) %>% .$coefficients %>% head(10)

