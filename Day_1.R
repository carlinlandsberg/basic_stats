# Day_1.R
# First day of statistics class, biostatistics course
# Purpose: to practice some of the concepts that we will encounter
# Carlin Landsberg
# 12 April 2018 

# Load libraries ----------------------------------------------------------

library(tidyverse)

# Integers ----------------------------------------------------------------

# Generate some integer data
integer_r <- as.integer(seq(5, 14, by = 1)) # numbers 5 to 22 by the interval 1

# Summarise data 
summary(integer_r) # look at brief summary of data, incl mean median etc

# Continuous --------------------------------------------------------------

# Generate a sequence of continuous data
numeric_r <- seq(23, 43, length.out = 10)

# Dates -------------------------------------------------------------------

# one may perform some arithmetic with dates
as.Date("2005-12-31") - as.Date("2005-12-12") # diff in days between this time
# or for example
dates_r <- seq(as.Date("2005-12-16"), as.Date("2005-12-25"), by = "day") # take note of corect format for date (iso date)
# There is much more
summary(dates_r)

# Dataframes --------------------------------------------------------------

# different types of data combined into one dataframe (the tibble)

# Create the base dataframe
# create new columns (3) with data previously assigned. 
# so, "integers", "numeric", and "dates" can be seen as the column names
df_r <- data.frame(integers = integer_r,
                   numeric = numeric_r,
                   dates = dates_r)
# then upgrade it to a tibble
df_r <- as_tibble(df_r)
summary(df_r)

# Categories --------------------------------------------------------------

# computer brands
brands_r <- as.factor(c("laptops",
                      "desktops",
                      "cellphones"))
# People
people_r <- as.factor(c("funny hair",
                        "beautiful",
                        "beanies"))
# colours
colour_r <- as.factor(c("red", "blue")) # 2 categories belonging to 2 levels ie red and blue (can also see levels in environment)

# factor variables used to creates "buckets" to place things into

# Ordinal data ------------------------------------------------------------

# here we still have qualitative data
# but with some sort of order

colour_qual <- ordered(c("blue", "green",
                         "yellow", "orange",
                         "red"),
                       levels = c("blue", "green",
                                  "yellow", "orange",
                                  "red")) # run in console, red is the warmest. colours "ranked" thus, ordered

# Binary ------------------------------------------------------------------

# measurements taking on 2, generally presented as: TRUE or FALSE
binary_r <- c(TRUE, FALSE, TRUE, TRUE)
summary(binary_r) # logi = logic YES or NO etc

# Characters --------------------------------------------------------------

# strings of words, so QUOTE each of words
sites_r <- c("Yztervarkpunt", "Betty's Bay",
             "Gansbaai", "Sea Point")

# Missing values ----------------------------------------------------------

# number of eggs recorded in a nest
# NA shows nest that was not able to be sampled
chicks_nest <- c(3, 2, 0, 10, 5, 6, 8, 2, 4, NA) # Note, there is a difference between NA and zero! We just dont have a number assigned to it (eg forgot to measure it)
# a summary
summary(chicks_nest)
# the mean
mean(chicks_nest)
# the standard deviation
sd(chicks_nest)

# Descriptive statistics --------------------------------------------------

# first create a dataframe
chicks <- as_tibble(ChickWeight)

# how many counts are there in this data (ie 578 observations)
chicks %>% 
  summarise(chicken_count = n())
# or
nrow(chicks)

# Measures of central tendency --------------------------------------------

# Calculate mean weight
chicks %>% 
  summarise(mean_wt = mean(weight)) # assign "name" to the mean weight

# Be more specific
chicks %>% 
  filter(Time == 21) %>% # filter by specifically the last day
  group_by(Diet) %>% # group it by diets
  summarise(mean_wt = mean(weight), # just copy and paste to prevent mistakes like typos
            median_wt = median(weight)) 
# for some, the means and medians look a bit dofferent (ie at 1 and 3), so lets visualise it

# Visualise the density of the data
ggplot(data = filter(chicks, Time == 21), 
       aes(x = weight, fill = Diet)) + # density only x axis, use filter() in line so dont have to create another dataframe
  geom_density(alpha = 0.4) 
# diet 1 is right-skewed data (remember hand squish example)
# diet 3 is left-skewed

# Skewness ----------------------------------------------------------------

# Calculate the numeric value
# first load the libraries
library(e1071)

# compare the difference in mean and median against skewness
chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mean_wt = mean(weight), 
            median_wt = median(weight),
            skew_wt = skewness(weight)) 
# look to see if the mean if to the LEFT or the RIGHT of the median (left or right skewed)

# 2 and 3 negatively skewed, 1 and 4 positively skewed
# so look at the plot again
# 3 is negatively skewed why? 

# skewness = where does most of the data lie? ie tails


# Kurtosis ----------------------------------------------------------------

# negative kurtosis: VERY SMALL TAILS 

# calculate the kurtosis of the tails of a distribution
chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(mean_wt = mean(weight), 
            median_wt = median(weight),
            skew_wt = skewness(weight),
            kurtosis_wt = kurtosis(weight)) 


# Measures of variability ----------------------------------------------------------------

# Below is a summary of many different statistical properties
wt_summary <- chicks %>% 
  filter(Time == 21) %>% 
  group_by(Diet) %>% 
  summarise(wt_mean = mean(weight),
            wt_median = median(weight),
            wt_var = var(weight),
            wt_sd = sd(weight),
            wt_min = min(weight),
            wt_quart1 = quantile(weight, 0.25),
            wt_quart2 = quantile(weight, 0.5),
            wt_quart3 = quantile(weight, 0.75)) 
# quantile; median is 50% so the middle quartile. Upper and middle quartile are 25%



