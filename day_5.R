# day_5.R
# Some more ANOVA
# Regressions
# Correlations
# Carlin Landsberg
# 20 April 2018

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(Rmisc)
library(ggpubr)
library(corrplot)

# Load the data -----------------------------------------------------------

snakes <- read_csv("snakes.csv") %>% 
  mutate(day = as.factor(day))

# OR
  # snakes$day = as.factor(snakes$day) 

# Manipulate the data -----------------------------------------------------

snakes_summary <- snakes %>% 
  group_by(day) %>% 
  summarise(snakes_mean = mean(openings),
            snakes_sd = sd(openings))

# Formulate a hypothesis --------------------------------------------------

# H0: there is NO difference in the number of openings from day to day
# H1: there IS a difference in the number of openings from day to day
  # Experiment designed to have only one of two outcomes (above) either H0 or H1
  
# Test a hypothesis -------------------------------------------------------

# First calculate SE and CI
snakes.summary2 <- summarySE(data = snakes, 
                             measurevar = "openings", # variance of openings
                             groupvars = c("day")) # group by day

# Visualise the data
ggplot(data = snakes, aes(x = day, y = openings)) +
  geom_segment(data = snakes.summary2, 
               aes(x = day, xend = day, y = openings - ci, 
                   yend = openings + ci, colour = day),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = day), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)
  # Segment is thick vertical colour line (xend, yend)
  # x and xend are the same 
  # y is mean minus ci, yend is mean plus ci
  # ci is confidence interval
# Similar to notch, it isnt reliable, just gives a suggestion of where differences lie

# across all days, there is no difference in the response to opening and closing of boxes

# BUT we have two factors, so we need another null hypothesis

# 1: H0: There is no difference between snakes with respect
        # to the number of openings at which they habituate.
# 2: H0: There is no difference between days in terms of the
        # number of openings at which the snakes habituate.


snakes.day.aov <- aov(openings ~ day, data = snakes)
summary(snakes.day.aov)
  # pr < 0.05, looking at H0 2: There IS a difference (H0 rejected)
  # F-value, Pr, df, Sum sq: must be reported
  # Sum sq: shows a lot of variance if this figure is high (unexplained stuff, should add more (eg add snakes))

# Test BOTH hypotheses
snakes.all.aov <- aov(openings ~ day + snake, data = snakes)
summary(snakes.all.aov)
  # snakes do not have significant effect.
  # pr > 0.05, looking at H0 1: there is no difference / effect (H0 fail to reject)

# Testing assumptions afterwards ------------------------------------------

# First visualise  normality of the data
snakes.residuals <- residuals(snakes.all.aov)
hist(snakes.residuals)

# Then visialise homoscedasicity of data
plot(fitted(snakes.all.aov), residuals(snakes.all.aov))
  # see that plots do not clump on just one side 

# Check Tukey results
snakes.tukey <- TukeyHSD(snakes.all.aov, which = "day")
plot(snakes.tukey)
  # significant difference at day 1 and 4 (p < 0.05)
  # also look at plot, 4-1 does not go over zero

# Visualise the factor interaction
ggplot(data = snakes, aes(x = as.numeric(day),
                          y = openings,
                          colour = snake)) +
  geom_line(size = 3) +
  geom_point(size = 4)

# Exercise ----------------------------------------------------------------

# Get the moth data from GitHub
# Run a two-way anova on them

moths <- read_csv("moth_traps.csv") %>% 
  gather(key = "trap", value = "count", -Location) # condense the data

# H0: there is NO difference in the count of moths at different locations
# H1: here IS adifference in the count of moths at different locations

moths.summary2 <- summarySE(data = moths, 
                             measurevar = "count", 
                             groupvars = c("Location"))

ggplot(data = moths, aes(x = Location, y = count)) +
  geom_segment(data = moths.summary2, 
               aes(x = Location, xend = Location, y = count - ci, 
                   yend = count + ci, colour = Location),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = Location), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)

# we have two factors, so we need two hypotheses
  # 1: H0: There is no difference of count of moths between various locations
  # 2: H0: There is no difference of count of moths between various traps

moth.loc.aov <- aov(count ~ Location, data = moths)
summary(moth.loc.aov)

# testing BOTH hypotheses
moth.all.aov <- aov(count ~ Location + trap, data = moths)
summary(moth.all.aov)
  # 1: H0: reject pr > 0.05 so, there IS  a difference at locations
  # 2: H0: fail to reject pr < 0.05, there is no differnce at different traps

# Testing assumptions -----------------------------------------------------

# First visualise  normality of the data
moths.residuals <- residuals(moth.all.aov)
hist(moths.residuals)

# Then visialise homoscedasicity of data
plot(fitted(moth.all.aov), residuals(moth.all.aov))

# Check Tukey results
moth.tukey <- TukeyHSD(moth.all.aov, which = "Location")
plot(moth.tukey)
# significant difference at top-middle and top-lower

# Visualise the factor interaction
ggplot(data = moths, aes(x = as.numeric(Location),
                          y = count,
                          colour = trap)) +
  geom_line(size = 3) +
  geom_point(size = 4)

plot1 <- ggplot(data = moths, aes(x = Location, y = count)) +
  geom_boxplot() +
  geom_jitter(width = 0.05, shape = 21)

plot2 <- ggplot(data = moths, aes(x = trap, y = count)) +
  geom_boxplot() +
  geom_jitter(width = 0.05, shape = 21)

plot3 <- ggplot(data = moths, aes(x = Location, y = count)) +
  geom_boxplot(aes(fill = trap)) 
  # geom_jitter(width = 0.05, shape = 21, aes(fill = trap))

finalplot <- ggarrange(plot1, plot2, plot3, nrow = 2, ncol = 2, labels = "AUTO")

# Regressions -------------------------------------------------------------

   # as one thing changes does something else change as well?... how much of x determine or affect y?
# for example, as ambient temperature increases, amount of activity increases in reptiles
  # the STRENGTH of this relationship tested using a linear regression

# ANIMATION: r^2: if 1, then perfect relationship (100%)
#                 if 0, then there is less of a relationship (eg. if 0.1 then only 10% of variance can be explained)
#            residSS: difference from observed (dots) and predicted (line) value
#                     this number is smallest at the ideal fit line (close to 0)
  # so ideal and "perfect" result is: p < 0.05, r^2 = 1, residSS = 0


# For the explanation of this statistical analysis
# We are going to use eruption data from Ol' Faithful

# Look at the top of the data
head(faithful)

# plot a quick scatterplot
ggplot(data = faithful, aes(x = waiting, y = eruptions)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, colour = "hotpink")

# Form a hypothesis -------------------------------------------------------

# H0: Waiting time does NOT influence the duration of the eruption
# H1: # H0: Waiting time DOES influence the duration of the eruption

# Test hypothesis ---------------------------------------------------------

eruption.lm <- lm(eruptions ~ waiting, data = faithful)
summary(eruption.lm)
  # p-value < 0.05, so we accept H1
  # Adjusted R-squared shows that 81% of variance explained

slope <- round(eruption.lm$coef[2], 3)
# p.val <- round(coefficients(summary(eruption.lm))[2, 4], 3) # it approx. 0, so...
p.val = 0.001
r2 <- round(summary(eruption.lm)$r.squared, 3)

ggplot(data = faithful, aes(x = waiting, y = eruptions)) +
  geom_point() +
  annotate("text", x = 45, y = 5, label = paste0("slope == ", slope, "~(min/min)"), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.75, label = paste0("italic(p) < ", p.val), parse = TRUE, hjust = 0) +
  annotate("text", x = 45, y = 4.5, label = paste0("italic(r)^2 == ", r2), parse = TRUE, hjust = 0) +
  stat_smooth(method = "lm", colour = "salmon") +
  labs(title = "Old Faithful eruption data",
       subtitle = "Linear regression",
       x = "Waiting time (minutes)",
       y = "Eruption duration (minutes)")
# grey line shows standard deviation

# Correlations ------------------------------------------------------------

#Load data
ecklonia <- read_csv("ecklonia.csv")

# Formulate a hypothesis --------------------------------------------------

#H0: There is no relationship between frond length and frond mass
#for the kelp Ecklonia maxima
#H1: There is relationship between frond length and frond mass
#for the kelp Ecklonia maxima
  # TAKE NOTE: we are not using the word "difference" but we are using "relationship"

# Test a hypothesis -------------------------------------------------------

cor.test(ecklonia$frond_length, ecklonia$frond_mass)
  # p < 0.05

# Visualise the data (scatterplot) ------------------------------------------------------

ggplot(data = ecklonia, aes(x = frond_length, y = frond_mass)) +
  geom_point()

# Run multiple tests at once ----------------------------------------------

ecklonia_sub <- ecklonia %>% 
  select(frond_length:epiphyte_length) # remove categorical variables
# semicolon: includes ALL columns from a up to z (A:Z)

ecklonia_cor <- cor(ecklonia_sub)
# this is basically a "table" 

# Spearman rank test ------------------------------------------------------

#First create an ordnial column
ecklonia$length <- as.numeric(cut((ecklonia$stipe_length + ecklonia$frond_length), 3))
  # cut creates "bins" like in a histogram

#Then run a Spearman test: for ORDINAL data (eg small, medium, large)
cor.test(ecklonia$length, ecklonia$stipe_diameter, method = "spearman")
  # rho = 0.60

# Kendall rank test -------------------------------------------------------

# used for data that is NOT NORMAL
cor.test(ecklonia$primary_blade_length, ecklonia$primary_blade_width, method = "kendall") 
  # tau = 0.34

# Visualize all the things ------------------------------------------------

ecklonia_pearson <- cor(ecklonia_sub)

corrplot(ecklonia_pearson, method = "circle")


# heat map ----------------------------------------------------------------

library(reshape2)
melted_eck <- melt(ecklonia_pearson)

ggplot(melted_eck, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue")

