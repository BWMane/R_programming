#####################
##MØA112
##logit Model
#####################

##set the working directory
setwd("C:/Users/...")

##read-in data
Default <- read.csv("C:/Users/.../Default.csv")
Default <- Default[-1]
str(Default)

##change to factor
Default$default <- as.factor(Default$default)
Default$student <- as.factor(Default$student)

glm1 <- glm(default~balance, family=binomial, data=Default)
summary(glm1)

install.packages("pscl")
library(pscl)
pR2(glm1)

##student
glm2 <- glm(default~student, family=binomial, data=Default)
summary(glm2)

##both
glm3 <- glm(default~balance + student, family=binomial, data=Default)
summary(glm3)

install.packages("margins")
library(ggplot2)
library(tibble)
library(broom)
library(margins)

meffects = margins(glm1) 
print(meffects)
summary(meffects)

plot(meffects)

meffects = summary(meffects)
ggplot(data = meffects) +
  geom_point(aes(factor, AME)) +
  geom_errorbar(aes(x = factor, ymin = lower, ymax = upper)) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))

############################################################################
##labor participation
library(dplyr)
library(tidyverse)

##Importing stata dataset
library(haven)
MROZ <- read_dta("MROZ.DTA")

logit_participation = glm(inlf ~ nwifeinc+educ+exper+expersq+age+kidslt6+kidsge6, data = MROZ, family = "binomial")
summary(logit_participation)
tidy(logit_participation)

effects_logit_participation = margins(logit_participation) 
print(effects_logit_participation)
summary(effects_logit_participation)

effects_logit_participation = summary(effects_logit_participation)
ggplot(data = effects_logit_participation) +
  geom_point(aes(factor, AME)) +
  geom_errorbar(aes(x = factor, ymin = lower, ymax = upper)) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45))










