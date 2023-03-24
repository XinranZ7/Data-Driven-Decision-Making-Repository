exp(0) / (exp(0) + 1)  # equivalent to plogis()
plogis(-Inf)           # infinite dispreference = likelihood 0
plogis(2)              # moderate preference = 88% chance (e.g., of purchase)
plogis(-0.2)           # weak dispreference

log(0.5 / (1-0.5))     # indifference = 50% likelihood = 0 utility
log(0.88 / (1-0.88))   # moderate high likelihood
qlogis(0.88)           # equivalent to hand computation


### season pass data

# Retrieve the amusement park dataset
pass.df <- read.csv("http://goo.gl/J8MH6A")
summary(pass.df)
pass.df$Pass <- as.factor(pass.df$Pass)
pass.m2 <- glm(Pass ~ Promo, data=pass.df, family=binomial)
summary(pass.m2)

plogis(-0.3888) / (1-plogis(-0.3888)) # ratio of outcome % to alternative %
exp(coef(pass.m2))

# We can obtain a confidence interval for the odds ratio as shown below
exp(confint(pass.m2))

table(pass.df$Pass, pass.df$Channel)

install.packages("vcd")
library(vcd)
doubledecker(table(pass.df))

# Model 2: 
pass.m2 <- glm(Pass ~ Promo + Channel, data=pass.df, family=binomial)
summary(pass.m2)

# updated coefs and odds ratios
exp(coef(pass.m2))
exp(confint(pass.m2))


# Model 3: add the interaction of promotion and channel
pass.m3 <- glm(Pass ~ Promo + Channel + Promo:Channel, 
               data=pass.df, family=binomial)
summary(pass.m3)
# updated coefs and odds ratios
exp(confint(pass.m3))


#First let us load the customer online sales transaction dataset
cust.df <- read.csv("http://goo.gl/PmPkaG")
summary(cust.df)

#### lm to predict online spend
spend.m1 <- lm(online.spend ~ ., data=subset(cust.df[ , -1], online.spend > 0))
summary(spend.m1) 


### Automatic data transformation
autoTransform <- function(x) { 
  library(forecast)
  return(scale(BoxCox(x, BoxCox.lambda(x))))
}

cust.df.bc <- cust.df[complete.cases(cust.df), -1]
cust.df.bc <- subset(cust.df.bc, online.spend > 0)
numcols <- which(colnames(cust.df.bc) != "email")
cust.df.bc[ , numcols] <- lapply(cust.df.bc[ , numcols], autoTransform )
summary(cust.df.bc) 


#### lm to predict online spend, after transform
spend.m2 <- lm(online.spend ~ ., data=cust.df.bc)
summary(spend.m2)


library(car)
vif(spend.m2)


# omit the covariates
spend.m3 <- lm(online.spend ~ . -online.trans -store.trans, data=cust.df.bc)
vif(spend.m3)
summary(spend.m3)



