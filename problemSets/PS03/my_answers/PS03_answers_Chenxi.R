#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("nnet", "MASS", "ggplot2", "stargazer"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################

# load data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/gdpChange.csv", stringsAsFactors = F)

##### Problem 1a. the un-ordered regression model

# first step, we need to recode the GDPWdiff variable with ifelse command:
# if < 0 - "negative"
# if = 0 - "no change"
# if > 0 - "positive"
gdp_data$GDPWdiff <- ifelse(gdp_data$GDPWdiff > 0, "positive",
                            ifelse(gdp_data$GDPWdiff == 0, "no change", 
                                   "negative"))
# transfer GDPWdiff into factor variable,
# this step is to make sure we can put GDPWdiff into response variable
gdp_data$GDPWdiff <- as.factor(gdp_data$GDPWdiff)
gdp_data$GDPWdiff # check if we successful
# the results show: 
# Levels: negative no change positive. so we succeed.

# second step, let's fit the model, hu-rrray!
# we have these 2 predictors:
# - REG: 1 for Democracy, 0 for Non-Democracy
# - OIL: 1 for the average ratio of fuel exports in 1984-86 exceeded 50%; 
#        0 for otherwise
# and before we fit the model, we should remember to regard "no change"
# as our reference level
gdp_data$GDPWdiff <- relevel(gdp_data$GDPWdiff, ref = "no change")
# fit the model
p1a_model <- multinom(GDPWdiff ~ REG + OIL,
                      data = gdp_data)
summary(p1a_model)
stargazer(p1a_model)
# third step, exponentiation the coefficient to interpret
exp(coef(p1a_model)[,c(1:3)])

##### Problem 1b. the ordered regression model

# because in Problem 1a., we have set "no change" as our reference level.
# but in Problem 1b., we don't need this anymore.
# and if we keep the reference level, 
# we will make a wrong order (no change - negative - positive)
# so we need to re-factor variable GDPWdiff, to assign the right order, which is
# (negative - no change - positive)
gdp_data$GDPWdiff <- factor(gdp_data$GDPWdiff, 
                            ordered = TRUE, 
                            levels = c("negative", "no change", "positive"))
# then fit the model
p1b_model <- polr(GDPWdiff ~ REG + OIL,
                  data = gdp_data)
summary(p1b_model)

# next step, we wish to calculate p-value and combine p-value into current table
ctable <- coef(summary(p1b_model))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
stargazer(ctable)

#####################
# Problem 2
#####################

# load data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/MexicoMuniData.csv")

# fit a poisson distribution model
p2a_model <- glm(PAN.visits.06 ~ 
                 competitive.district + marginality.06 + PAN.governor.06, 
                 data = mexico_elections, family = poisson)
summary(p2a_model)
