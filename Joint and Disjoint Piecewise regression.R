rm(list=ls())

# Download file "ceosal2.dta" to your working directory
# Set working directory
setwd("fill in your working directory in here. Remeber to change \ into /")

##### 1 Import data CEO salaries formatted as a .dta file #####

		library(foreign)

		data <- read.dta("ceosal2.dta")
		head(data)
		summary(data)

  # Variable explanation:
  # salary: 1990 compensation, $1000s
  # age: in years
  # college =1 if attended college
  # grad =1 if attended graduate school
  # comten: years with company
  # ceoten: years as ceo with company
  # sales: 1990 firm sales, millions
  # profits: 1990 profits, millions
  # mktval: market value, end 1990, mills.

		
##### 2 A simple linear model #####		

		m01 <- lm(salary ~ mktval + sales, data = data)
		summary(m01)

# Why sales do not affect salary? Possibly because of multi-collinearity. Run the correlation to see that. And yes, they are highly correlated
	cor(data$mktval, data$sales)	

# Ramsery reset test

		RR <- lm(salary ~ mktval + sales + I(fitted(m01)^2) + I(fitted(m01)^3) + I(fitted(m01)^4) , data=data)
		anova(RR, m01)
  # pvalue = 0.07: borderline significance
  # so let's try a log model. log model usually has better fitness because it is more likely to result in normal distribution. However, do not compare R2 of log models with that of linear model.
		
##### 3 Log-log regressions ##### 

		hist(data$salary)

		hist(log(data$salary))		## much nicer distribution
    
		install.packages("moments")
		library(moments)

		kurtosis(log(data$salary))	## Kutosis = 3 => normal distribution, if >3 => steeper hon normal, if <3 => flatter than normal
		skewness(log(data$salary))	## skewnes = 0 => normal distribution, if >0 => positive skewness, if <0 => negative skewness
		
		m1 <- lm(log(salary) ~ log(mktval) + log(sales) + age + college + grad, data = data)
		summary(m1)

##### 4 What can be done with 0 when a variable is log-transformed? ####
		
	# Let's include CEO's experience
		m2 <- lm(log(salary) ~ log(mktval) + log(sales) + log(ceoten), data = data)
  # Error because "ceoten" has 0. There are 2 ways to solve this issue:
		  
  # Solution 1: Eliminate 0 years as CEO 
		summary(data$ceoten)
		data1 <- data[data$ceoten !=0,]

		m3 <- lm(log(salary) ~ log(mktval) + log(sales) + log(ceoten), data = data1)
		summary(m3)

		m4 <- lm(log(salary) ~ log(mktval) + log(sales) + log(ceoten) + grad, data = data1)
		summary(m4)
  
		# "college" is not significant because the number of CEO who did not attend college is too low in the sample
		data$college
  
    # check normality of residual
		hist(resid(m3))
		qqnorm(resid(m3))
		abline(0,1)
		
  # Solution 2: replace ceoten = 1 if ceoten =0. Results are very similar to m04
		data2 <- data
		data2$ceoten [data2$ceoten == 0] <- 1
		data2$ceoten 
		mod5 <- lm(log(salary) ~ log(mktval) + log(sales) + log(ceoten) + grad, data = data2)
		summary(mod5)
		hist(resid(mod5))

  # Ramsey RESET test
		RR <- lm(log(salary) ~ log(mktval) + log(sales) + log(ceoten) + I(fitted(m4)^2) + I(fitted(m4)^3) + I(fitted(m4)^4) , data=data1)
		anova(RR, m4)

  # Heteroskedasticity-consistent standard errors
		library(lmtest)
		library(sandwich)

		coeftest(m4, vcov = vcovHC(m4, "HC0"))

##### 5 Joint and Disjoint Piecewise Models #####
		plot(data$sales, data$salary)
		abline(m01, col = "blue")
		
		## Threshold: sales=3000
		data$thsales <- 0 
		data$thsales [data$sales>3000] <- 1
	
		## Model 1: Disjoint piecewise model
		m1 <- lm(salary ~ mktval + sales +  thsales + I(thsales*sales), data = data)
		summary(m1)
		# Model 1: salary = 584.5 + 0.022*mktval + 0.1006*sales + 430.5*thsales - 0.09853*thsales*sales + u
		# If thsales = 0, then: salary = 584.5 + 0.022*mktval + 0.1006*sales + e
		# If thsales = 1, then: salary = 1015 + 0.022*mktval + 0.002044*sales + i (where u,e, and i are error terms)
		
		# Plot (drop outlier that sales>10000)
		x <- data$sales
		y <- data$salary	
		plot (x, y, xlim = c(0,10000), ylim = c(0, 3500), xlab = "sales", ylab = "salary")	
		curve(584 + 0.1006*x, add=T, from=0, to=3000, col = "blue")
		curve(1015 + 0.002044*x, add=T, from=3000, to=10000, col = "blue")
		abline(v=3000, lty=3, col="yellow")
		
		## Model 2: Joint piecewise
		m2 <- lm(salary ~ mktval + sales + I(thsales*(sales-3000)), data = data)
		summary(m2)
		#Model 2: salary = 560 + 0.0227*mktval + 0.1338*sales - 0.129*thsales*(sales-3000) + u
		# If thsales = 0, then Model 2: salary = 560 + 0.0227*mktval + 0.1338*sales + e
		# If thsales = 1, then Model 2: salary = 947.15192 + 0.0227*mktval + 0.00481*sales + i
		
		# Plot
		curve(560 + 0.1338*x, add=T, from=0, to=3000, col="red")
		curve(947.15192 + 0.00481*x, add=T, from=3000, to=10000, col="red")
		
rm(list=ls())		
############### END ###############