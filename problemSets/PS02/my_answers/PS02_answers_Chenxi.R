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

lapply(c("stargazer", "tidyverse"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2024/blob/main/datasets/climateSupport.RData?raw=true"))

# check the details of the dataframe
summary(climateSupport)

# check if the type of variable is factor
var_types_0 <- sapply(climateSupport, str)
# countries and sanctions are both Ord.factor
# prepare the to be converted variables
convert <- c("countries", "sanctions")

# use for loop to convert variables
climateSupport$countries <- factor(climateSupport$countries, 
                            levels = c("20 of 192", "80 of 192", "160 of 192"),
                            ordered = FALSE)
climateSupport$sanctions <- factor(climateSupport$sanctions, 
                            levels = c("None", "5%", "15%", "20%"),
                            ordered = FALSE)

# check the type of variables
var_types_1 <- sapply(climateSupport, str)

# because the response variable is binary, so choose logistic regression here
q1mod <- glm(choice ~ ., # Y and Xs
             data = climateSupport, # select dataset
             family = "binomial") # select method as binomial
# summary the model
summary(q1mod)
# export to latex
stargazer(q1mod)

# the global hypothesis test
# create the null mod for hypothesis test
nullMod <- glm(choice ~ 1, # 1 = fit an intercept only
               data = climateSupport, # select dataset
               family = "binomial") # select method as binomial

# use chi square method to make global test
anova(nullMod, q1mod, test = "Chisq")
# and we can also try this way, they are equal!
anova(nullMod, q1mod, test = "LRT")

# check the predict value
q2c_predict <- predict(q1mod, # set model
                       newdata = data.frame(countries = "80 of 192", 
                                            sanctions = "None"), # set dataframe
                       type = "response") # get dependent variable value
print(q2c_predict) # print the results

# add interaction into the regression model
q2mod <- glm(choice ~ countries * sanctions, # Y and Xs
             data = climateSupport, # select dataset
             family = "binomial") # select method as binomial
summary(q2mod)

# use chi square method to make global test
anova(nullMod, q2mod, test = "Chisq")
# and we can also try this way, they are equal!
anova(nullMod, q2mod, test = "LRT")

# export to latex
stargazer(q2mod)

# make significant test for different slopes
anova(q1mod, q2mod, test = "Chisq")

# Make a data frame
predicted_data <- data.frame(
  choice = climateSupport$choice,
  q1mod_hat = q1mod$fitted.values,
  q2mod_hat = q2mod$fitted.values
)

# Reorder and plot for q1mod_hat
ordered_data <- arrange(predicted_data, q1mod_hat)
ordered_data <- mutate(ordered_data, rank = row_number())

q1_plot <- ggplot(ordered_data, aes(rank, q1mod_hat)) +
  geom_point(aes(colour = choice), alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = "Q1 Model - without interaction")

# Reorder and plot for q2mod_hat
ordered_data <- arrange(predicted_data, q2mod_hat)
ordered_data <- mutate(ordered_data, rank = row_number())

q2_plot <- ggplot(ordered_data, aes(rank, q2mod_hat)) +
  geom_point(aes(colour = choice), alpha = 0.5) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = "Q2 Model - with interaction")

# Save plots to a PDF
pdf("q2_plot1.pdf")
# Plot in two columns
gridExtra::grid.arrange(q1_plot, q2_plot, nrow = 2)
dev.off()

