# Day_2.R
# Day 2 of biostats basic stats course
# purpose: discussing data visualisations and distributions 
# Carlin Landsberg
# 13 April 2018 

# Load libraries ----------------------------------------------------------

library(tidyverse)

# Manual calculations -----------------------------------------------------

# Generate random data
r_dat <- data.frame(dat = rnorm(n = 600, mean = 372, sd = 50), # to get random normal data. looking for 3 arguments: n, mean, sd
                    sample = "A") # create your dataframe WITHIN here instead of creating more lines

# Quick visualisation (good to do before analysing your data)
ggplot(data = r_dat, aes(x = dat)) +
  geom_density()

# The mean
# calculated by: the sum of all the points
#                divided by
#                the number of all the points
r_dat %>% 
  summarise(r_sum = sum(dat), # use summarise if you have a big dataframe and you want to "condenses" it
            r_n = n(), # to see how many numbers or "points" there are, instead of adding static numbers, use numbers that are present in the dataset!
            r_mean = r_sum/r_n,
            r_mean_func = mean(dat)) 

# The median
# subset the data at n+1 divided by 2
# For median, first order the data

#The brute force with base R
r_dat$dat[(length(r_dat$dat)+1)/2]

# Or use tidy
r_dat %>% 
  arrange(dat) %>% # arrange orders the data from lowest to highest
  slice(n()/2) 

# Or the tidy automagic way
r_dat %>%
  #slice(dat, (nrow(dat)))
  summarise(r_median = median(dat))  

# Variance ----------------------------------------------------------------

# The sum of
  # Each value 
    #minus 
      #the mean
        # Squared
# Divided by 
  # The count of samples minus one
r_dat %>% 
  mutate(r_error = dat - mean(dat),
         r_error_square = r_error * r_error) %>% # add column onto existing dataframe (the anomaly column)
  summarise(r_squared_sum = sum(r_error_square),
            r_var = r_squared_sum/(n()-1),
            # or use the built in function
            r_var_funct = var(dat))

# The standard deviation
r_dat %>% 
  summarise(r_var = var(dat),
            r_sd = sqrt(r_var),
            r_sd_func = sd(dat))

# Exercise 1 ----------------------------------------------------------------

# first, what does summary return when we use ChickWeights?
summary(ChickWeight$weight)

# how would we reproduce this in tidy data?

ChickWeight %>% 
  summarise(min_weight = min(weight),
            quart_1 = quantile(weight, 0.25),
            med_weight = median(weight),
            mean_weight = mean(weight),
            quart_3 = quantile(weight, 0.75),
            max_weight = max(weight))


# Visualisations ----------------------------------------------------------

# First load libraries
# There few packages contain most functions neccesary 
# to make publictation ready figures
library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(viridis)

# load our SA time data
sa_time <- read_csv("sa_time")

# Edit our data 
  # always try to add changes in the code rather than changing the raw data
sa_time <- sa_time %>% 
  mutate(human = seq(1, n(), 1),
         geo = c(rep(c("Cape Town", "George", "PE"), times = 6), 
                     rep("Joburg", 2))) # adds human row from 1 to the number of rows (20) by increments of 1 (ie 1,2,3,4...)

# Create long data
sa_long <- sa_time %>% 
  gather(key = "time_type", value = "minutes", -human)
  
  
# Qualitative -------------------------------------------------------------

# generally use proportion (relative to something)
# count usually for something like a histogram
# see whatever is appropriate for the subject and the reader


# when making ggplots we want all the data (big dataframe) unlike in summaries where we condense the data to only whta we need

# Create a count of qualitative values
sa_count <- sa_long %>% 
  count(time_type) %>% # number of count for each type (20)
  mutate(prop = n/sum(n)) # should be three rows long and three columns wide

# Stacked bar graphs
ggplot(data = sa_count, aes(x = "", y = n, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
  labs(title = "Stacked bar graph", subtitle = "cumulative sum",
       x = NULL, y = "Count") +
  theme_minimal() 

# Stacked proportion bar graph
ggplot(data = sa_count, aes(x = "", y = prop, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
  scale_y_continuous(breaks = c(0.00, 0.33, 0.66, 1.00)) +
  labs(title = "Stacked bar graph", subtitle = "relative proportions",
       x = NULL, y = "Proportion") +
  theme_minimal()

# A pie chart... not rally to be used professionally though
ggplot(data = sa_count, aes(x = "", y = prop, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
  labs(title = "Pie Chart", subtitle = "but why though?",
       x = NULL, y = NULL) +
  coord_polar("y", start = 0) +
  theme_minimal()
  # fine for small sets of data... but still. just dont. just leave it. leave the whole thing


# Quantitative ------------------------------------------------------------

# Histograms
  # most straightforward way to visualise data, with only an x-axis
ggplot(data = sa_long, aes(x = minutes)) +
  geom_histogram()

# Oh, no!
# lets get ride of that one value (generally not done, so give a reason for this)
sa_clean <- sa_long %>% 
  filter(minutes < 300)

# A faceted histogram 1 (placed side by side)
ggplot(data = sa_clean, aes(x = minutes)) +
  geom_histogram(aes(fill = time_type), position = "dodge") +
  facet_wrap(~time_type, scales = "free_x")

# A faceted histogram 2 (on top of each other)
# add ncol = 1 to stack them on top of each other
ggplot(data = sa_clean, aes(x = minutes)) +
  geom_histogram(aes(fill = time_type), position = "dodge") +
  facet_wrap(~time_type, ncol = 1, scales = "free_x")

# This one shows the relative proportions
ggplot(data = sa_clean, aes(x = minutes)) +
  geom_histogram(aes(fill = time_type),
                 position = "dodge", binwidth = 1) +
  facet_wrap(~time_type, ncol = 1, scales = "free_x")

# so analysed, overall population has their own way of distinguishing between now, now now and just now
# distributions are the same 
# HOMEWORK: ask family same thing 

# Boxplots
  # x is qualitative, y is quantitative
ggplot(data = sa_clean, aes(x = time_type, y = minutes)) +
  geom_boxplot(aes(fill = time_type))
# Boxplot is  visualisation of summary table
  # q1 and q3 are the ends of the blocks
  # dot is the MAX value
  # between q1 and q3 is interquartile range
  # tail = q1 or q3 x 1.5 BUT why are tails not same length on either side then?
    # tail shows all datapoints within q1 or q3 x 1.5
  # Blue is more variable compared to red, thus red has a point as an outlier 
  # green is very small, so two points are outliers

# Notched boxplots
ggplot(data = sa_clean, aes(x = time_type, y = minutes)) +
  geom_boxplot(aes(fill = time_type), notch = TRUE)
# middle line is median
# areas in boxes that are overlapping within the notch are not statistically different from each other
  # ie "indented" parts in blue and red overlap, in green it does not so green is statistically different to blue and red

# Calculate summary stats for plotting over the boxplots
sa_summary_stats <- sa_clean %>% 
  group_by(time_type) %>% 
  summarise(time_type_mean = mean(minutes))

ggplot(data = sa_clean, aes(x = time_type, y = minutes)) +
  geom_boxplot(aes(fill = time_type), notch = TRUE) +
  geom_point(data = sa_summary_stats, size = 6, shape = 18,
             aes(y = time_type_mean), colour = "goldenrod")

# Relationships -----------------------------------------------------------

# A basic scatterplot 
  # Has quantitative data on both x and y axes
ggplot(data = sa_time, aes(y = now_now, x = just_now)) +
  geom_point() +
  coord_equal(xlim = c(0,300), ylim = c(0,300))

# Make the scale smaller at xlim and ylim
ggplot(data = sa_time, aes(y = now_now, x = just_now)) +
  geom_point() +
  coord_equal(xlim = c(0,60), ylim = c(0,60))

# Adding trend lines
ggplot(data = sa_time, aes(y = now_now, x = just_now)) +
  geom_point(aes(colour = geo)) +
  geom_smooth(aes(colour = geo), method = "lm") +
  coord_equal(xlim = c(0,60), ylim = c(0,60))
# if grey overlaps with other grey, points are similar
# angle of line shows relationship between x and y variables






