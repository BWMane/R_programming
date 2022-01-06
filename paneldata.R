################################
##Pooled cross section data
##R code for the secture slides
################################

##set the working directory
setwd("C:/Users/.../Courses/MA112/R")

################################################################################
##If you are using the full data

library(tidyverse)

##read-in data
gendat <- read.csv("C:/Users/2906235/OneDrive - Universitetet i Stavanger/Courses/MA112/R/gendat.csv")

colnames(gendat)[1] <- "Birth.year"
colnames(gendat)[2] <- "gender"

gendat <- gendat %>% filter(Birth.year<70)
table(gendat$Birth.year)
gendat$ID <- seq_along(gendat[,1])
gendat$NYFYLKE <- as.factor(gendat$NYFYLKE)

##reduced gendat
gendat_light <- gendat[,c(1:15,24:29,38:73,195,254:282,341:369,680:708)]
write.csv(gendat_light, "gendat_light.csv")

################################################################################
##If you are using the gendat_light

##read-in data
gendat <- read.csv("C:/Users//gendat_light.csv")


##
cohort50 <- gendat %>% filter(Birth.year==50)
cohort50 <- cohort50 %>% mutate(inc30=b79inn80, finc17=f79inn67, minc17=m79inn67, lninc30=log(inc30+1), lfinc17=log(finc17+1), lminc17=log(minc17+1)) %>% filter(between(inc30, quantile(inc30, .01), quantile(inc30, .99)))

cohort55 <- gendat %>% filter(Birth.year==55)
cohort55 <- cohort55 %>% mutate(inc30=b79inn85, finc17=f79inn72, minc17=m79inn72, lninc30=log(inc30+1), lfinc17=log(finc17+1), lminc17=log(minc17+1)) %>% filter(between(inc30, quantile(inc30, .01), quantile(inc30, .99)))

cohort60 <- gendat %>% filter(Birth.year==60)
cohort60 <- cohort60 %>% mutate(inc30=b79inn90, finc17=f79inn77, minc17=m79inn77, lninc30=log(inc30+1), lfinc17=log(finc17+1), lminc17=log(minc17+1)) 
cohort60 <- na.omit(cohort60)
cohort60 <- cohort60 %>% filter(between(inc30, quantile(inc30, .01), quantile(inc30, .99)))

cohort65 <- gendat %>% filter(Birth.year==65)
cohort65 <- cohort65 %>% mutate(inc30=b79inn95, finc17=f79inn82, minc17=m79inn82, lninc30=log(inc30+1), lfinc17=log(finc17+1), lminc17=log(minc17+1)) %>% filter(between(inc30, quantile(inc30, .01), quantile(inc30, .99)))


lm50 <- lm(inc30 ~finc17 + minc17, data=cohort50)
summary(lm50)

lm55 <- lm(inc30 ~finc17 + minc17, data=cohort55)
summary(lm55)

lm60 <- lm(inc30 ~finc17 + minc17, data=cohort60)
summary(lm60)

lm65 <- lm(inc30 ~finc17 + minc17, data=cohort65)
summary(lm65)

library(stargazer)
stargazer(lm50,lm55,lm60,lm65, title="Regression by Cohort",
          align=TRUE,
          no.space=TRUE,type="text")


lm50 <- lm(lninc30 ~lfinc17 + lminc17 + gender + NYFYLKE, data=cohort50)
summary(lm50)

lm55 <- lm(lninc30 ~lfinc17 + lminc17 + gender + NYFYLKE, data=cohort55)
summary(lm55)

lm60 <- lm(lninc30 ~lfinc17 + lminc17 + gender + NYFYLKE, data=cohort60)
summary(lm60)

lm65 <- lm(lninc30 ~ lfinc17 + lminc17 + gender + NYFYLKE, data=cohort65)
summary(lm65)

library(stargazer)
stargazer(lm50,lm55,lm60,lm65, title="Regression by Cohort",
          align=TRUE, omit = "NYFYLKE",
          no.space=TRUE,type="text")

##Pooled
library(dplyr)
cohort.pooled <- bind_rows(cohort50, cohort55, cohort60, cohort65)
cohort.pooled$cohort <- as.factor(cohort.pooled$Birth.year)

lmpooled <- lm(lninc30 ~ lfinc17 + lminc17 + gender + NYFYLKE + cohort, data=cohort.pooled)
summary(lmpooled)

lmpooled2 <- lm(lninc30 ~ lfinc17*cohort + lminc17*cohort + gender*cohort + NYFYLKE*cohort, data=cohort.pooled)
summary(lmpooled2)


library(stargazer)
stargazer(lm50,lm55,lm60,lm65,lmpooled2, title="Regression by Cohort",
          align=TRUE, omit = "NYFYLKE",
          no.space=TRUE,type="text")

