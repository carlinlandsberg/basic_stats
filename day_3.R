# day_3.R
# Distributions
# Carlin Landsberg
# 17 April 2018

# Some notes --------------------------------------------------------------


# load packages -----------------------------------------------------------

library(fitdistrplus)
library(logspline)
library(tidyverse)
library(plotly)
library(ggpubr)


r_norm <- rnorm(n = 1000, mean = 13, sd = 1)

hist(r_norm)
# can see that data is in fact normal
descdist(r_norm, discrete = FALSE, boot = 100)

# uniformly distributed data
y <- runif(100)
par(mfrow = c(1, 1))
# par(mfrow = c(2, 2))
plot(x = c(1:100), y = y)
hist(y)
descdist(y, discrete = FALSE)

# t-tests -----------------------------------------------------------------

r_dat <- data.frame(dat = c(rnorm(n = 1000, mean = 10, sd = 3),
                            rnorm(n = 1000, mean = 8, sd = 2)),
                    sample = c(rep("A", 1000), rep("B", 1000)))


# check assumptions -------------------------------------------------------

# Normality
# For this we may use the Shapiro-Wilk test
shapiro.test(r_dat$dat) 
shapiro.test(r_dat$dat)[1]
shapiro.test(r_dat$dat)[2]# run on total

# but that is testing all of the data together
# we must be a bit more clever about how we make this test
r_dat %>% 
  group_by(sample) %>% # calculating for group A and B
  summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2])) # summarises all data into 2 values
# we are only intersted in the p-value
# how to test if data is normal, usually in biology data is not normal

# NOTE : data normal when p > 0.05
#       data non-normal when p < 0.05

# check homoscedasticity --------------------------------------------------

# there are many ways to check for homoscedasticity
# which is the similarity of variance between sample sets
# for now we will simply say that this assumption is met when
  # the variance of the samples are not more than 2 - 4 times greater than one another

# check variance for everything at once...
# WRONG
var(r_dat$dat)

# or do it the tidy
r_dat %>% 
  group_by(sample) %>% 
  summarise(r_norm_dist = as.numeric(shapiro.test(dat)[2]),
            r_norm_var = var(dat))
# r_norm_dist are p-values
# r_norm_var is variance
  # these are two assumptions that have to be met (see 6.1 in the workbook, last 2 points have to be calculated)

# A one sample t-test -----------------------------------------------------
r_one <- data.frame(dat = rnorm(n = 20, mean = 20, sd = 5),
                    sample = "A")
# Test normality of distribution
shapiro.test(r_one$dat)

# Perhaps a visualisation?

# Run the test
t.test(r_one$dat, mu = 20) # which column we want to run the test on 
# p-value = 0.3849 : not significant

# run a test we know will produce a significant result
t.test(r_one$dat, mu = 30)
# df is usually sample size minus 1
# p-value = 5.353e-08 : significant
# based on p-value, accept or reject alternative hypothesis


# Pick a side -------------------------------------------------------------

# are these data SMALLER/LESS than the population mean
t.test(r_one$dat, mu = 20, alternative = "less")
 
# or greater
t.test(r_one$dat, mu = 20, alternative = "greater")

# But what about for the larger population mean?
# Are the samples less than the poulation of 30?
t.test(r_one$dat, mu = 30, alternative = "less")
# What about greater than?
t.test(r_one$dat, mu = 30, alternative = "greater")


# two sample t-tests ------------------------------------------------------

# create another dataframe
r_two <- data.frame(dat = c(rnorm(n = 20, mean = 4, sd = 1),
                            rnorm(n = 20, mean = 5, sd = 1)),
                    sample = c(rep("A", 20), rep("B", 20)))

# run a default/ basic test
t.test(dat ~ sample, data = r_two, var.equal = TRUE)
# df = 38 because 20+20-2=38

# Pick a side
# Is A less than B?
t.test(dat ~ sample, data = r_two, var.equal = TRUE, alternative = "less")
# is A greater than B?
t.test(dat ~ sample, data = r_two, var.equal = TRUE, alternative = "greater")

# A t-test workflow -------------------------------------------------------

# For stipe mass
ecklonia <- read_csv(file = "ecklonia.csv") %>% 
  gather(key = "variable", value = "value", -species, -site, -ID)

# Visualise the data with a boxplot
ggplot(data = ecklonia, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip()

# Filter out only stipe mass data
ecklonia_sub <- ecklonia %>% 
  filter(variable == "stipe_mass")

# Visualise data looking at stipe mass
ggplot(data = ecklonia_sub, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "stipe mass (kg)", x = "") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# check t-test assumptions: normality (shapiro) and variance/ homoscedasticity (var)
ecklonia_sub %>% 
  group_by(site) %>% 
  summarise(stipe_mass_var = var(value)[1],
            stipe_mass_norm = as.numeric(shapiro.test(value)[2]))

# t-test traditional output
t.test(value ~ site, data = ecklonia_sub, var.equal = TRUE, alternative = "greater")

# dataframe output
compare_means(value ~ site, data = ecklonia_sub, method = "t.test", var.equal = TRUE, alternative = "greater")

# The stipe mass (kg) of the kelp Ecklonia maxima was found to be significantly 
# greater at Batsata Rock than at Boulders Beach (p = 0.03, t = 1.87, df = 24)


# For stipe length
ecklonia <- read_csv(file = "ecklonia.csv") %>% 
  gather(key = "variable", value = "value", -species, -site, -ID)

# Visualise the data with a boxplot
ggplot(data = ecklonia, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip()

# Filter out only stipe length data
stipe_len <- ecklonia %>% 
  filter(variable == "stipe_length")

# Visualise data looking at stipe length
ggplot(data = stipe_len, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "stipe length (cm)", x = "") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# check t-test assumptions: normality (shapiro) and variance/ homoscedasticity (var)
stipe_len %>% 
  group_by(site) %>% 
  summarise(stipe_len_var = var(value)[1],
            stipe_len_norm = as.numeric(shapiro.test(value)[2]))

# t-test traditional output
t.test(value ~ site, data = stipe_len, var.equal = TRUE, alternative = "greater")

# dataframe output
compare_means(value ~ site, data = stipe_len, method = "t.test", var.equal = TRUE, alternative = "greater")
# p = 7.062e-05, t = 4.5187, df = 24
# The stipe length of the kelp Ecklonia maxima was found to be significantly 
# greater at Batsata Rock than at Boulders Beach 

# Exercise 1 --------------------------------------------------------------

# HYPOTHESIS: males are larger in length compared to females

# Creating a random dataset
random <- data.frame(length = c(rnorm(n = 100, mean = 12, sd = 2),
                            rnorm(n = 100, mean = 8, sd = 2)),
                    sample = c(rep("Male", 100), rep("Female", 100)))

# Visualise the data using a boxplot
ggplot(data = random, aes(x = sample, y = length, fill = sample)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "length", x = "") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
  # Boxplots showing difference in lengths of males and females

# Question: are males larger than females?
# H0: Length of males ARE NOT greater than length of females
# H1: Length of males ARE greater than length of females

# For a t-test, we must meet certain assumptions

# To perform a t-test, data must be normal and homoscedastic
  # calculated using shaprio.test and var, respectively
random %>% 
  group_by(sample) %>% 
  summarise(norm_dat = as.numeric(shapiro.test(length)[2]),
            var_dat = var(length)[1])
  # for both male and female, p > 0.05 ie data is normal
  # homoscedastic because variance of one is not more than two to four times greater than the other

# t-test traditional output
t.test(length ~ sample, data = random, var.equal = TRUE, alternative = "greater")

# dataframe output
compare_means(length ~ sample, data = random, method = "t.test", var.equal = TRUE, alternative = "greater")
  # p-value = 2.04e-33 (p < 0.05)
  # because p < 0.05, we can reject the null hypothesis and accept the alternative hypothesis

# In conclusion, length of males was found to be significantly greater 
# than the length of females 

# BUT t.test and compare_means give two different p-values...
  # t.test: p = 1, t = -14.556, df = 198 (shows no significant difference?)


