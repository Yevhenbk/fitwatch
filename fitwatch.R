library(foreign)
library(boot)
library(ggplot2)
library(tidyverse)

set.seed(123) # set seed for reproducibility
mydata <- read.dta("fitwatch_univ.dta")
x <- mydata[, "Minutes"]
y <- mydata[, "Cardio"]
ct <- y / x

#finding mean
mean_x <- mean(x)

#finding variance
var_x <- var(x)

#finding standard deviation
sd_x <- sd(x)

#creating histogram minutes
png("histogram.png")
hist(x, main = "Histogram of Minutes", xlab = "Minutes", col = "blue", border = "black")

#generating an image of the histogram
dev.off()

result90 <- t.test(x, mu = mean_x, sd = sd_x, alternative = "less", conf.level = 0.90)
result95 <- t.test(x, mu = mean_x, sd = sd_x, alternative = "less")
result99 <- t.test(x, mu = mean_x, sd = sd_x, alternative = "less", conf.level = 0.99)

#bootstrapping 200 times 'Minutes' column
mean_values <- replicate(200, mean(sample(x, replace = TRUE)))

#calculating mean of bootstrapped values
mean_bootstrap <- mean(mean_values)

#creating an histogram of bootstraped values
ggplot(data.frame(mean_values), aes(x = mean_values)) + 
  geom_histogram(binwidth = 0.1, color = "black", fill = "white") + 
  xlab("Mean Value") + 
  ylab("Count") + 
  ggtitle("Histogram of 200 Bootstrapped Means")

ggsave("bootstrapped_histogram.png")

#derivate 90% confidence interval by mean
CI90 <- quantile(mean_values, c(0.1, 0.9))

#creating histogram ct
png("ct.png")
hist(ct, main = "Histogram of CT",xlim=c(0,10), xlab = "CT", col = "red", border = "black")

#generating an image of the histogram
dev.off()

#bootstrapping 200 times 400 items from CT returning variance
var_values <- replicate(200, var(sample(ct, size = 400, replace = TRUE)))

#calculating variance of bootstrapped values
var_bootstrap <- var(var_values)

#creating histogram of bootstrapped variance of ct
ggplot(data.frame(var_values), aes(x = var_values)) + 
  geom_histogram(binwidth = 0.1, color = "black", fill = "white") + 
  xlab("Variance Value") + 
  ylab("Count") + 
  ggtitle("Histogram of 200 Bootstrapped Variances")

ggsave("bootstrapped_histogram_var.png")

#bootstrapping 200 times 400 items from CT
mean_ct_values <- replicate(200, mean(sample(ct, size = 400, replace = TRUE)))

#creating histogram of bootstrapped means of ct
ggplot(data.frame(mean_ct_values), aes(x = mean_ct_values)) + 
  geom_histogram(binwidth = 0.1, color = "black", fill = "white") + 
  xlab("Mean CT Value") + 
  ylab("Count") + 
  ggtitle("Histogram of 200 Bootstrapped Means CT")

ggsave("bootstrapped_histogram_mean_ct.png")

#calculating median for ct while bootstrapping
med_values <- replicate(200, median(sample(ct, size = 400, replace = TRUE)))

#calculating median of bootstrapped values
med_bootstrap <- median(med_values)

#creating histogram of bootstrapped median
ggplot(data.frame(med_values), aes(x = med_values)) + 
  geom_histogram(binwidth = 0.1, color = "black", fill = "white") + 
  xlab("Median Value") + 
  ylab("Count") + 
  ggtitle("Histogram of 200 Bootstrapped Medianes")

ggsave("bootstrapped_histogram_med.png")

#extra points -> derivate 95% confidence interval by median
CI95 <- quantile(med_values, c(0.05, 0.95))
#The result of CI will be a vector with the 5th and 95th quantiles 
#of the bootstrapped medians, which represents the lower and upper bounds 
#of the 95% confidence interval for the median.

#check for the values
print(mean_x)
print(var_x)
print(sd_x)
print(mean_values)
print(mean_bootstrap)
print(var_values)
print(var_bootstrap)
print(med_values)
print(med_bootstrap)

#run one-sided t-tests
result90
result95
result99
CI90
CI95
