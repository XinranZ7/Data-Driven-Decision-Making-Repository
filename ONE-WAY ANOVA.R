my_data <- PlantGrowth
set.seed(1234)
dplyr::sample_n(my_data, 10)
levels(my_data$group)

my_data$group <- ordered(my_data$group,
                         levels = c("ctrl", "trt1", "trt2"))
library(dplyr)
group_by(my_data, group) %>%
  summarise(
    count = n(),
    mean = mean(weight, na.rm = TRUE),
    sd = sd(weight, na.rm = TRUE)
  )

# Box plot
boxplot(weight ~ group, data = my_data,
        xlab = "Treatment", ylab = "Weight",
        frame = FALSE, col = c("#00AFBB", "#E7B800", "#FC4E07"))
# plotmeans
library("gplots")
plotmeans(weight ~ group, data = my_data, frame = FALSE,
          xlab = "Treatment", ylab = "Weight",
          main="Mean Plot with 95% CI") 
# Compute the analysis of variance
res.aov <- aov(weight ~ group, data = my_data)
# Summary of the analysis
summary(res.aov)

TukeyHSD(res.aov)

pairwise.t.test(my_data$weight, my_data$group,
                p.adjust.method = "BH")

# 1. Homogeneity of variances
plot(res.aov, 1)

# homogenity of variance
library(car)
leveneTest(weight ~ group, data = my_data)

oneway.test(weight ~ group, data = my_data)
pairwise.t.test(my_data$weight, my_data$group,
                p.adjust.method = "BH", pool.sd = FALSE)
# 2. Normality
plot(res.aov, 2)

#Extract the residuals
aov_residuals <- residuals(object = res.aov )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )

# non-parametric equivalent 
kruskal.test(weight ~ group, data = my_data)
