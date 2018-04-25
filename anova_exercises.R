# anova_exercises.R
# Three ANOVA exercises from basic stats books
# Carlin Landsberg
# 24 April 2018

# Load libraries ----------------------------------------------------------

library(tidyverse)

# 7.4.1 Exercise 1 --------------------------------------------------------

# enter the mass at the end of the experiment
feed_1 <- c(60.8, 57.0, 65.0, 58.6, 61.7)
feed_2 <- c(68.7, 67.7, 74.0, 66.3, 69.8)
feed_3 <- c(102.6, 102.1, 100.2, 96.5)
feed_4 <- c(87.9, 84.2, 83.1, 85.7, 90.3)

# make a dataframe
bacon <- as.tibble(data.frame(
  feed = c(
    rep("Feed 1", length(feed_1)),
    rep("Feed 2", length(feed_2)),
    rep("Feed 3", length(feed_3)),
    rep("Feed 4", length(feed_4))
  ),
  mass = c(feed_1, feed_2, feed_3, feed_4)
))

# ANOVA -------------------------------------------------------------------

# QUESTION: Does feed type have an effect on the mass of pigs at the end of the experiment?
# H0: There is NO difference in pig mass at the end of the experiments after being fed one of four diets
# H1: There IS a difference in pig mass at the end of the experiments after being fed one of four diets

pigs.aov <- aov(mass ~ feed, data = bacon)
summary(pigs.aov)
  # Pr < 0.05, so we reject the null hypothesis
  # so, there is a difference in pig mass at the end of the experiment after being fed one of four diets

# There is a difference, but which feeds have the effect?

# Visualise with boxplots -------------------------------------------------

ggplot(data = bacon, aes(x = feed, y = mass, fill = feed )) +
  geom_boxplot(notch = TRUE)
  # none of the feeds notched overlap... but this graph is just a suggestion

# Tukey -------------------------------------------------------------------

TukeyHSD(pigs.aov)
  # p-adj < 0.05 for all feeds: show all diets are different and significant
  # lwr and upr does not go across zero in all cases

# Visualise this ---------------------------------------------------------

plot(TukeyHSD(pigs.aov))

# 7.4.2 Exercise 2 --------------------------------------------------------

teeth <- datasets::ToothGrowth

# QUESTION: Does difference in doses of Vitamin C have an effect on the length of tooth growth in guinea pigs
# H0: There is NO difference in tooth lengths of guinea pigs receiving one of three doses of vitamin C
# H1: There IS a difference in tooth lengths of guinea pigs receiving one of three doses of vitamin C

# Filter out only Vitamin C doses -----------------------------------------

teeth_vc <- ToothGrowth %>% 
  filter(supp == "VC")

# ANOVA -------------------------------------------------------------------

teeth.aov <- aov(len ~ as.factor(dose), data = teeth_vc)
summary(teeth.aov)
  # pr < 0.05, so we reject the null hypothesis
  # so, there is a difference in tooth lengths of guinea pigs receiving one of three doses of vitamin C

# So which dosages have the effect?

# Visualise this using a boxplot ------------------------------------------

ggplot(data = teeth_vc, aes(x = as.factor(dose), y = len, fill = as.factor(dose))) +
  geom_boxplot(notch = TRUE)
  # none of the dose notches overlap... suggesting they are different

# Tukey -------------------------------------------------------------------

TukeyHSD(teeth.aov)
  # p-adj < 0.05, very low for all. so all doses are significant and different 
  # lwr and upr does not cross zero

# Visualise this ----------------------------------------------------------

plot(TukeyHSD(teeth.aov))

# 7.4.3 Exercise 3 --------------------------------------------------------

teeth <- datasets::ToothGrowth

# H0: interactions between supplement and dose have NO effect on length of teeth
# H1: interactions between supplement and dose DO have an effect on length of teeth

# looking at only length by supplement
summary(aov(len ~ supp, data = teeth))

TukeyHSD((aov(len ~ supp, data = teeth)))

plot(TukeyHSD((aov(len ~ supp, data = teeth))))

# dose was done in previous example, but running too many ANOVAS increases error
# so... 

# now to look at interactions BETWEEN factors
summary(aov(len ~ supp * as.factor(dose), data = teeth)) 
  # pr < 0.05, so we reject the null hypothesis
  # so, interactions between supplement and dose DOES havae an effcet on length of teeth

# So which have the effect?
TukeyHSD((aov(len ~ supp * as.factor(dose), data = teeth)))

plot(TukeyHSD((aov(len ~ supp * as.factor(dose), data = teeth))))
# On plot, all combinations not crossing zero show which combinations of supplement and diet have the most effect on length of teeth 
