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

lapply(c("ggplot2"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################

# The function of Kolmogorov-Smirnov test with normal distribution
ks_normal <- function(data) {
  # This function is used to test if a distribution follows normal distribution.
  
  # Parameters: 
  # - data: The empirical distribution
  
  # Returns:
  # - D: largest absolute difference between the two distribution
  # - p_value: Probability that the null hypothesis is correct

  # create empirical distribution of observed data
  ECDF <- ecdf(data)
  empiricalCDF <- ECDF(data)
  # generate test statistic
  D <- max(abs(empiricalCDF - pnorm(data)))
  # R can not deal with infinite, so I use 1000 instead
  upper_limit <- 1000
  # calculate p-value
  p_value <- sqrt(2 * pi) / D * sum(exp(-((2 * (1:upper_limit) - 1)^2 * pi^2) / (8 * D^2)))
  # return results
  return(list(D = D, p_value = p_value))
}
# set seed as 123
set.seed(123)
# generate 1,000 Cauchy random variables
data <- rcauchy(1000, location = 0, scale = 1)
# use function to drive Kolmogorov-Smirnov test
ks_results <- ks_normal(data)
# print the test results
paste("D:", ks_results$D)
paste("p_value:", ks_results$p_value)
# check by ks.test function in R
ks_check <- ks.test(data, "pnorm")
print(ks_check)

#####################
# Problem 2
#####################

set.seed (123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)

# Draw a scatter to see the distribution
pdf("q2_plot1.pdf")
q2_scatter <- ggplot(data, aes(x = x, y = y)) + 
  geom_point(shape=1, size=2.8, color="#0F4C75") + # set point shape and color
  theme_bw() + # set the coooooolest style
  theme(panel.grid = element_blank())  # no gird
print(q2_scatter)
dev.off()

# 


# Finally, let's compare with lm methods.
lm_method <- lm(y ~ x, data)
summary(lm_method)

