                #####################################
                #   Author: Thi Hoa (Hannah) Nguyen #
                #   Topic 3 Validation tests        #
                #####################################

rm(list=ls())          
set.seed(123) 

# Download dataset "bweight.csv" from https://github.com/hannahhoa/hannahhoa

###### Set working direction. Remember to change "\" into "/") #####
setwd("//uft.homes.deakin.edu.au/my-home/My Documents/Github")


###### Import data on birth weight and smoking #####
  bw <- read.table("bweight.csv", sep =";", header = TRUE)
  head(bw)

## Variable explanation
# bweight: weight of the child when he/she was born
# inc: income
# cigs: number of packages of cigarettes smoked by mother during pregnancy
# smoker: =1 if a smoker, =0 if not a smoker
# For the effect of birth weight on health and income see:
# http://ns.umich.edu/new/releases/5882
# other things that influence bweight: mom/dad education, mom/dad birth weight,
# mom age, baby gender, race 

#0.028 is a convergence factor from ounce to kg
  bw$weight <- bw$bwght*0.028 
  head(bw)

# From summary stats, we can see that there is outlier
  hist(bw$weight) 
  summary(bw$weight)

  plot(bw$cigs, bw$weight)



###### Models #####
  # A simple model that has omitted variable problem
  mod1 <- lm(weight~cigs, data = bw)
  summary(mod1)
  abline(coef(mod1), col="blue")

  # Model 2 includes income as a control covariate
  mod2 <- lm(weight ~ cigs + inc, data = bw)
  summary(mod2)


###### Validation tests for model "mod2" #####
## Install package "moments"
  install.packages("moments")
  library(moments)   
  

## Normality (JB) test for model 2
  s <- skewness(resid(mod2)) 
  k <- kurtosis(resid(mod2))
  JB <- (nrow(bw)/6)*(s^2 + (k-3)^2 /4)
  JB   
  1  - pchisq(JB, df = 2, ncp = 0) # ncp: position of the chisq on the axis
  # Reject the hypothesis that the error is normally distributed.
  

## QQ test
  stand <- resid(mod2)/sqrt(var(resid(mod2)))
  qqnorm(stand)
  abline(0,1)  
  # It is not along the line, there are outliers
  

## Heteroskedasticity test (white)
  WT <-lm(resid(mod2)^2 ~ inc + cigs + inc*cigs + I(inc^2) + I(cigs^2), data = bw) 
  # "I" stands for isolated, telling R that it is a new variable and request calculating it
  summary (WT)
  # pvalue = 0.98. Thus we do not reject the hypothesis that the error is heterogeneous
 
  
## Caculate R-squared of model WT
  WT.test <- nrow(bw)*summary(WT)$r.squared
  1 - pchisq(WT.test, df=5, ncp = 0)


## Ramsey reset test
  RR <- lm(weight ~ inc + cigs + I(fitted(mod2)^2) + I(fitted(mod2)^3) + I(fitted(mod2)^4), data = bw)
  anova(RR, mod2)



## Dealing with outliers
  hist(bw$weight)

# Create a new data set not containing outliers
  newdata <- bw[bw$weight < 7,]
  hist(newdata$weight)

# Model 1
  mod1 <- lm(weight ~ inc + cigs, data = newdata)
  summary(mod1)

# Re-run the RESET test and comment
  RR <- lm(weight ~ inc + cigs + I(fitted(mod2)^2) + I(fitted(mod2)^3) + I(fitted(mod2)^4), data = newdata)
  anova(RR, mod1)
  # Since the p-value = 0.06, we reject the null hypothesis that no non-linear correlation exists
  # at the significance level of alpha = 10%. If the significance level is 5%, then we do not reject that
  # null hypothesis. In other words, the model is mis-specified if the significance level is 5% or less.

####---- END ----####