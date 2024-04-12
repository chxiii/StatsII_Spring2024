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

lapply(c("eha", "ggplot2", "stargazer", "survival"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################

# load data
dat <- child # use the child data set in eha

# from the eha network codebook, 
# https://cran.r-project.org/web/packages/eha/eha.pdf - page 13
# we can see the instruction
# - sex: Sex
# - m.age: mother age

child_surv <- with(child, Surv(enter, exit, event))
km <- survfit(child_surv ~ 1, data = child)
summary(km, times = seq(0, 15, 1))

km_data <- data.frame(time = km$time, surv = km$surv)

# get CI
lower_ci <- km$lower
upper_ci <- km$upper

# visualisation the curve
pdf("Kaplan-Meier.pdf")
ggplot(km_data, aes(x = time, y = surv)) +
  geom_step() +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), 
              fill="#3F72AF", color="black", linetype = "dashed", alpha = 0.5) +  
  labs(title = "Kaplan-Meier Plot with Confidence Interval", x = "Years", y = "Survival Probability") +
  ylim(0.7, 1) +
  theme_bw()
dev.off()

COX <- coxph(child_surv ~ sex + m.age, data = dat)
summary(COX)
