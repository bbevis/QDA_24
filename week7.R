
#################################
# Week 7 - Hypothesis testing
# Imai & Williams 7.2
#################################

library(tidyverse)
# Creating table for tea-tasting experiment: one with
# all 8 cups correctly classified,
# and another with 6 out of the 8 cups correctly classified

x = matrix(c(4,0,0,4), byrow = TRUE, ncol = 2, nrow = 2)
y = matrix(c(3,1,1,3), byrow = TRUE, ncol = 2, nrow = 2)

rownames(x) = colnames(x) = rownames(y) = colnames(y) = c("M", "T")

fisher.test(x, alternative = "greater")

fisher.test(y)

##########################################
# Statistical hypothesis testing
#
# 1. Specify a null hypothesis
# 2. Choose a test statistic and level of alpha
# 3. Derive the sampling distribution of the test statistic
# 4. Compute p-value (one or two sided)
# 5. Reject the null if p-value is less than or equal to the alpha
#
##########################################

n = 1018
x_bar = 550 / n # standard deviation
se = sqrt(0.5 * 0.5 / n)

upper = pnorm(x_bar, mean = 0.5, sd = se, lower.tail = FALSE)
lower = pnorm(0.5 - (x_bar - 0.5), mean = 0.5, sd = se)

# 1 sided p-value
upper

# 2 sided p-value
upper + lower

# since upper and lower areas are the same, we can also do:
2 * upper

# When using the normal distribution, researchers often use the z-score
z_score = (x_bar - 0.5) / se
pnorm(z_score, lower.tail = FALSE) # 1 sided p-value
2 * pnorm(z_score, lower.tail = FALSE) # 2 sided p-value

# Confidence intervals
# Directly related to hypothesis tests

# 99% confidence interval containing 0.5
c(x_bar - qnorm(0.995) * se, x_bar + qnorm(0.995) * se)
# we can reject the null if 1 - alpha confidence levels does not contain mean
# we fail to reject the null when alpha = 0.01

# Using the Star dataset

# data("STAR", package = "qss")
# write.csv(STAR, "STAR.csv", row.names = FALSE)
star = read.csv("STAR.csv")

# Assuming that the null hypothesis that the mean reading test score = 710, we can calculate a
# 2 sided one sample t-test

t.test(star$g4reading, mu = 710)
# we reject at the 0.05 level, the null hypothesis that the population mean(has a test score of 710.


# 2 sample tests allows us to test whether a hypothesis is not zero

# Testing the null hypothesis of zero average treatment effect
star = star %>%
  mutate(classtype = factor(classtype, labels = c("Small", "Regular", "Regular with aide")))


reading_small = filter(star, classtype == "Small")$g4reading
reading_regular = filter(star, classtype == "Regular")$g4reading

t.test(reading_small, reading_regular)
# since P > 0.05, we fail to reject the null

# Failing to find a significant effect because the data is not informative enough
# i.e. too small a sample size
# We can use the power analysis to quantify the degree of informativeness of data
# where Power = 1 - (type 2 error). Remember that type 2 error is a false positive.


# Min sample size
install.packages("pwr")
library(pwr)

# Power analysis for a t-test
# Parameters:
# d = effect size (Cohen's d)
# sig.level = significance level (usually 0.05)
# power = desired power (e.g., 0.80)
# type = type of t-test, e.g., "two.sample" for a two-sample t-test
# alternative = "two.sided" or "one.sided" test

pwr.t.test(d = 0.5, sig.level = 0.05, power = 0.80, type = "two.sample", alternative = "two.sided")

# Calculating Power

# power is often interpreted with a few general guidelines:
#
# < 0.5: Very low power, meaning the test is unlikely to detect an effect, even if one is present. Such tests are at high risk for Type II errors (false negatives).
# 0.5 – 0.7: Low power, providing some ability to detect effects but with a substantial risk of Type II errors.
# 0.7 – 0.8: Moderate power, considered marginally acceptable in some research contexts, but may still miss small effects.
# ≥ 0.8: Good power, widely accepted as the standard threshold. Tests with this level of power are usually adequate for detecting meaningful effects.
# > 0.9: High power, desirable for studies where detecting smaller effects is critical, or for confirmatory studies.

# Sample data for two groups
group1 <- c(12, 14, 15, 13, 14)  # Group 1 data
group2 <- c(10, 12, 11, 9, 11)   # Group 2 data

# Run the t-test
test_result <- t.test(group1, group2, var.equal = TRUE)

# Calculate effect size (Cohen's d)
mean_diff <- abs(mean(group1) - mean(group2))
pooled_sd <- sqrt(((length(group1) - 1) * var(group1) + (length(group2) - 1) * var(group2)) / (length(group1) + length(group2) - 2))
cohen_d <- mean_diff / pooled_sd

# Calculate power
power_result <- pwr.t.test(d = cohen_d, n = min(length(group1), length(group2)), sig.level = 0.05, type = "two.sample", alternative = "two.sided")
round(power_result$power, 4)

# Cohen's d is a measure of effect size that quantifies the difference between two group means in terms of standard deviation.
