
# install.packages('tidyverse')
library(tidyverse)
library(ggplot2)
library(car)

##################################
# B1
##################################

traffic = read.csv('driving.csv')

# It contains data on traffic accidents and traffic related laws for US states for several years. States are identified only by a
# number (in the variable “state”) and years by the variable “year”.

# (a) What is the range of years that is covered by the data? Data for how many different states is contained in
# the dataset? Does the number of years of data available for different states in the dataset vary? The
# variable “totfatrte” records the total fatalities per 100,000 of the population (fatality rate) in a state in a given
# year. What’s the average fatality rate across the sample? (5 marks)


# (b) Run a regression of the fatality rate on the variable “minage”, which records the minimum drinking age in a given
# state at a given point in time (This ranges between 18 and 21 years). Report your regression output. Discuss and
# interpret the regression parameter for drinking age. Is your result statistically significant? Provide a potential explanation
# for your findings.(5 marks)


# (c) Discuss at least two mechanisms that might lead to a biased estimate of the causal effect of a higher drinking age in
# part (b). Also discuss the direction of the bias. (5 marks)



# (d) Can you propose a regression model that could address some potential endogeneity issues using only the variables
# that have so far been mentioned in this exercise? Explain why. Implement the proposed model and discuss what you
# find! (5 marks)


# (e) Can you propose a regression model that would help you explore if there is a difference in the marginal impact of
# the age limit for the different levels of age limit (minage) present in the data? Can you implement this model and discuss
# the findings? Can you propose and implement a formal test of variations in marginal impact? (5 marks)


##################################
# B2
##################################

house = read.csv('house.csv')
# (a) Run a regression of lprice on distkm. Provide an interpretation of the regression coefficient for distkm. Is the sign
# of the coefficient in line with your expectations about the underlying causal mechanism in this regression? (5 marks)
house %>%
  with(summary(lm(lprice ~ distkm)))

# (b) The variable lndist measures the natural logarithm of distance of the house from the incinerator in feet. Run a
# regression of lprice on ldist. Provide an interpretation of this regression. (5 marks)


# (c) Provide at least one reason why the estimates discussed in (a) and (b) could be biased. Discuss whether your
# reason would lead to an upward or downward bias. (5 marks)


# (d) The variable larea records the natural logarithm of surface area of the house in square feet. Run a regression of
# lprice on larea and distkm. Discuss the results. Discuss whether this regression can provide a less biased estimate of
# the causal impact of incinerator distance on house prices? Could there also be a situation where this provides a worse
# estimate? Also comment on the how the coefficient on distkm changes going from the regression output in part (a) to
# part (b). Can you provide a motivation for the observed change? (5 marks)


# (e) Motivate why the distance effect might not be linear. Can you implement a quadratic regression of lprice on the
# distkm variable? Discuss what you find. Can you identify a distance from the incinerator where – according to the
# regression – being further away from the incinerator has no price increasing effect anymore? (5 marks)



