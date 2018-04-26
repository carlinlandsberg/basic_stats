# day_6.R
# Confidence Intervals
# Carlin Landsberg
# 26 April 2018

# Some notes --------------------------------------------------------------

# Confidence intervals similar to error bars

# Load libraries ----------------------------------------------------------

library(rcompanion)
library(mvtnorm)
library(DescTools)
library(lmtest)
library(tidyverse)
library(ggpubr)

 # update R and try to load rcompanion again...  

# load data ---------------------------------------------------------------

Input <- ("
Student  Sex     Teacher  Steps  Rating
          a        female  Jacob    8000   7
          b        female  Jacob    9000  10
          c        female  Jacob   10000   9
          d        female  Jacob    7000   5
          e        female  Jacob    6000   4
          f        female  Jacob    8000   8
          g        male    Jacob    7000   6
          h        male    Jacob    5000   5
          i        male    Jacob    9000  10
          j        male    Jacob    7000   8
          k        female  Sadam    8000   7
          l        female  Sadam    9000   8
          m        female  Sadam    9000   8
          n        female  Sadam    8000   9
          o        male    Sadam    6000   5
          p        male    Sadam    8000   9
          q        male    Sadam    7000   6
          r        female  Donald   10000  10
          s        female  Donald    9000  10
          t        female  Donald    8000   8
          u        female  Donald    8000   7
          v        female  Donald    6000   7
          w        male    Donald    6000   8
          x        male    Donald    8000  10
          y        male    Donald    7000   7
          z        male    Donald    7000   7
          ")
data <- read.table(textConnection(Input),header = TRUE)
summary(data)

# one-way data
groupwiseMean(Steps ~ Sex, data = data, conf = 0.95,digits = 3)

# two-way data
dat1 <- groupwiseMean(Steps ~ Teacher + Sex, data = data, conf = 0.95,digits = 3)
  # lives in the console, so how can we visualise what this means?

# Create the graph --------------------------------------------------------

library(ggplot2)

ggplot(data = dat1, aes(y = Mean, x = Sex)) +
  geom_point(aes(colour = Teacher)) +
  geom_errorbar(aes(ymin = Mean - Trad.lower,
                    ymax = Mean + Trad.upper,
                    colour = Teacher)) +
  facet_wrap(~Teacher) 
# calculate confidence and plot them arund the mean in this graph 

# Testing assumptions -----------------------------------------------------

# normal
# homoscedastic
# continuous (dependent variable)
# independent 

# log transform, natural logs, square, cubic, log10

dat2 <- data %>% 
  mutate(logdata = log(Steps),
         log10 = log10(Steps),
         sqrt = sqrt(Steps),
         cubert = (Steps)^(1/3)) %>% 
  select(-Student, -Rating) %>%             # This section added later
  gather(key = "data.type", value = "trans.data",
         -Sex, -Teacher)

ggplot(data = dat2, aes(x = trans.data)) +
  geom_histogram(binwidth = 1000, aes(fill = Sex), position = "dodge") +
# this histogram does not tell us much... 
  facet_grid(data.type ~ Teacher, scales = "free_x")

ggplot(data = dat2, aes(x = Steps)) +
  geom_histogram(aes(fill = Sex),
                 position = "dodge")

logplot <- ggplot(data = dat2, aes(x = logdata)) +
  geom_histogram(aes(fill = Sex),
                 position = "dodge")

log10plot <- ggplot(data = dat2, aes(x = log10)) +
  geom_histogram(aes(fill = Sex),
                 position = "dodge")

sqrtplot <- ggplot(data = dat2, aes(x = sqrt)) +
  geom_histogram(aes(fill = Sex),
                 position = "dodge")

cubertplot <- ggplot(dat = dat2, aes(x = cubert)) +
  geom_histogram(aes(fill = Sex),
                 position = "dodge")

ggarrange(logplot, log10plot, sqrtplot, cubertplot,
          ncol = 2, nrow = 2, common.legend = TRUE)

# Iris data ANOVA ---------------------------------------------------------

iris.dat <- as.tibble(iris)  

# HO: There is NO significant difference in petal.width between the iris species 

shapiro.test(iris$Petal.Length)
# p less than 0.05 so data is not normal
  # but this is pretty useless... 

iris %>% 
  group_by(Species) %>% 
  summarise(norm_dat = as.numeric(shapiro.test(Petal.Width)[2])) 
  # group by species, to separately determine if they are normal or not
# for setosa and versicolor, data are not normal
# for virginica, data normal
  # thus, treat all the data as not normal

# Do a Kruskal-Wallis instead of an ANOVA ---------------------------------

kruskal.test(Petal.Width ~ Species, data = iris)
  # we reject the null hypothesis
  # there IS a difference in petal.width between the iris species