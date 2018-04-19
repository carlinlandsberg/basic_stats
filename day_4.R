# day_4.R
# ANOVAS
# Carlin Landsberg
# 19 April 2018

# Some notes --------------------------------------------------------------

# if we just do a lot of t-tests, the error increases
# so we simultaneously look at differences using an analysis of varaiance (ANOVA)

# Load libraries ----------------------------------------------------------

library(tidyverse)
library(ggplot2)

# Revisiting t-tests  -----------------------------------------------------

chicks <- as_tibble(ChickWeight)

chicks_sub <- chicks %>% 
  filter(Diet %in% c(1, 2), Time == 21) # filtering out diets 1 and 2 (excluding diets 3 and 4) at time 21 (the last day only)
# are these two diets resulting in different sizes after day 21?

# t-test
t.test(weight ~ Diet, data = chicks_sub)
# we do not reject the null hyp 

# is there, between 4 diets, a difference in the means
# one factor (diet) with 4 levels

# ANOVA - one-way ---------------------------------------------------------

# RESEARCH QUESTION: at day 21 is there a difference in mass as a result of being fed four different diets

# NULL HYPOTHESIS: There is no difference in chciken mass at 21 days after being fed one of four diets

chicks_21 <- chicks %>% 
  filter(Time == 21)

chicks.aov1 <- aov(weight ~ Diet, data = chicks_21)
summary(chicks.aov1)

# chicks.aov1 <- aov(weight ~ Diet, data = filter(chicks, Time == 21))
  # can also filter this in one step

# Pr is probability : we do not accept H0, we accept H1
# after 21 days, the four different diets have a significant effect on the mass of chickens
# anova tell us there is a difference, but we do not know which diets have the effect and which not

ggplot(data = chicks_21, aes(x = Time, y = weight, fill = Diet)) +
  geom_boxplot(notch = TRUE)
# diet 1 does not overlap with diet 3 and diet 4 
# diet 3 overlaps with diet 2 and diet 4
  # but this grophs gives just a suggestion... so do another test

TukeyHSD(chicks.aov1)
# compares diets to each other (ie diet 1 vs diet 2)
# p-adj : 2-1 no significance; does not result in different size chicks
# 3-1 there is a difference
# all others are not significant
# if lwr is +ve we know there is a significant different (upr and lwr is above zero)

# ANOVA
summary(aov(weight ~ Diet, data = chicks_21))

# Tukey
TukeyHSD(aov(weight ~ Diet, data = chicks_21))

# Boxplot
ggplot(data = chicks_21, aes(x = Diet, y = weight, fill = Diet)) +
  geom_boxplot(notch = TRUE, colour = "grey50") +
#  coord_flip() +
  geom_segment(aes(x = Diet, xend = Diet, y = weight, yend = weight+2), colour = "red", size = 0.8)
# if lwr and upr go across zero, then not significant

# Segments showing confidence intervals
# dataframe of segments
chicks_Tukey <- as.data.frame(TukeyHSD(aov(weight ~ Diet, data = chicks_21))$Diet)
chicks_Tukey$pairs <- as.factor(row.names(chicks_Tukey))

ggplot(data = chicks_Tukey) +
  geom_segment(aes(x = pairs, xend = pairs, y = lwr, yend = upr)) +
  # geom_errorbar(aes(x = pairs, ymin = lwr, ymax = upr))
  coord_flip() +
  geom_hline(aes(yintercept = 0), size = 0.5, colour = "red", linetype = "dashed")

plot(TukeyHSD(chicks.aov1, "Diet"))
# plot(TukeyHSD(aov(weight ~ Diet, data = chicks_21)))

# Multiple factor ANOVA ---------------------------------------------------

# t-test compares 2 things (eg two boxplots)
# ANOVA compares many things (more than two boxplots - still one factor)

# so what is the relationship of diet AND time on chicken mass?

# H0: There is no change in chicken mass from day 0 to day 21

# Create a dataframe with just those days
chicks_0_21 <- ChickWeight %>% 
  filter(Time %in% c(0,2,21))

# Visualise the data
ggplot(data = chicks_0_21, aes(x = Time, y = weight)) +
  geom_boxplot(notch = T, aes(fill = as.factor(Time)))

# run an ANOVA
summary(aov(weight ~ as.factor(Time), data = chicks_0_21))

# Tukey post-hoc
TukeyHSD((aov(weight ~ as.factor(Time), data = chicks_0_21)))

# Look at confidence intervals
plot(TukeyHSD((aov(weight ~ as.factor(Time), data = chicks_0_21))))


# summary(aov(weight ~ Diet, data = filter(chicks, Time %in% c(2))))
# summary(aov(weight ~ Diet, data = filter(chicks, Time %in% c(10))))
  # can look at difference at specific days only by doing this
  # but doing many anovas also increases error

# formula: what is effect of diet on weight (weight ~ Diet) and effect of time on weight (+ as.factor(Time))
# as.factor (Time): df = 1 because c(0,21) so two values 
# pr values for Diet and Time are not talking to each other
# c(0,21) looks at only dat 0 and day 21, not all days from day 0 to day 21

# Look only at day 0 and 21 for both Time and Diet
summary(aov(weight ~ Diet + as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21))))

# Or simply look at ALL of the time
# ... which is NOT the hypothesis
summary(aov(weight ~ Diet + as.factor(Time), data = ChickWeight))
# note the increase in the df for the time factor
# but no increase in df for diet

# now to look at interactions BETWEEN factors
summary(aov(weight ~ Diet * as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21)))) 

# lets look at the Tukey results
TukeyHSD(aov(weight ~ Diet * as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21)))) 
# because it is *, it looks at ALL combinations
plot(TukeyHSD(aov(weight ~ Diet * as.factor(Time), data = filter(ChickWeight, Time %in% c(0, 21)))))

# create a line graph to help explain this concept
# First create mean values by Time and Diet
chicks_mean <- ChickWeight %>% 
  group_by(Diet, Time) %>% 
  summarise(weight_mean = mean(weight, na.rm = TRUE))

# Then visualise it
ggplot(data = chicks_mean, aes(x = Time, y = weight_mean, colour = Diet)) +
  geom_line(size = 2) +
  geom_point(shape = 15, size = 5) 

# Non-parametric tests ----------------------------------------------------

# but what if... we dont have normal data

# For a t-test we rather use Wilcox rank sum test
wilcox.test() # and then one fills this in the same as for t.test()

# usually avoid these because it is not robust

# And now for Kruskal-Wallis
  # more than 2 levels (so same as ANOVA)
kruskal.test(weight ~ Diet, data = chicks_0_21) 
# output is less robust (not as much info)

# load this for non-parametric post-hoc test
library(pgirmess)
kruskalmc(weight ~ Diet, data = chicks_0_21)















  

