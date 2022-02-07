            #####################################
            #   Author: Thi Hoa (Hannah) Nguyen #
            #   Topic 2 Non-linear Regression   #
            #####################################
  
  rm(list=ls())          
  set.seed(123) 

##### 1 A simple non-linear model #####
##### y <- 5 * x^0.5 * u

# x is uniformed distributed, N=1000, min=1, max=50                        
  x <- runif(1000,1,50)
  
# u is uniformed distributed, N=1000, min=0.5, max=2   
  u <- runif(1000,0.5,2)

# y: logarithm transformation of x and y  
  y <- 5 * x^0.5 * u  
  plot(x,y)
 
# By using logs, we get linear form: logy = log5 + 0.5logx + logu  
  plot(log(x),log(y))	
  
# Now estimate the model  
  m1 <- lm(log(y) ~ log(x))
  summary(m1)
  exp(coef(m1)[1])
  
  
 

##### 2 Another non-linear model #####
##### y <- 2 + 3 * log(x) + u
  rm(list=ls())
  set.seed(123) 

# x is uniformed distributed, N=500, min=1, max=50    
  x <- runif(500,1,50)
  
# u is normally distributed  
  u <- rnorm(500)
  plot(density(u))
  
# y is not normally distributed but that's alright  
  y <- 2 + 3 * log(x) + u
  plot(density(y))	
  
# Some plots  
  plot(x,y)
  plot(log(x),log(y))
  plot(x,y, col="blue", pch = 3, cex =0.5) 	
  # plot options: col = color, pch=plotting characters, cex = character expansion, lwd=line width  

# Data frame
  mydat <- data.frame(y = y, x = x)
  

## Model 1: This model is incorrect model as it's misspecified
  mod1 <- lm(y ~ x, data=mydat)
  summary(mod1)
  abline(coef(mod1), lwd = 2)

  # Residual distribution does not look too bad though
  hist(resid(mod1))					
  
  # Next plot shows that misspecification is evident. It shows that there is a 
  # relationship between x and residual that mod1 has not captured, indicating that
  # our model is not correct
  plot(x, resid(mod1))				
  

## Model 2: Correct model
  mod2 <- lm(y ~ log(x), data=mydat)
  summary(mod2)

  # Look at the model fit :) how nice it is  
  plot(mydat$x,mydat$y)					
  fit <- fitted(mod2)
  lines(mydat$x[order(mydat$x)], fit[order(mydat$x)], col="blue", lwd=2 )
 
  # Plotting x and residual results in a spherical shape, implying random residual
  plot(x, resid(mod2))


## Model 3: Lowess model (non parametric regression)
  mod3 <- lowess(x, y)
  plot(mydat$x,mydat$y)	

  # Compare fitness of 3 models
  abline(coef(mod1), col="red", lwd=2)
  lines(mydat$x[order(mydat$x)], fit[order(mydat$x)], col="blue", lwd=2 )
  lines(mod3, lwd= 2, col="green")	
  
  
## Model 4: Lowess model but less smooth local regression (f=0.1) 
  mod4 <- lowess(x, y, f=0.1)		
 
  
  
##### 3 Model fit #####
# To find model that fit the best, we compare RSS. 
# But first, let's have a quick look on fitted curves
  plot(mydat$x,mydat$y)					
  
  abline(coef(mod1), lwd = 2)
  fit <- fitted(mod2)
  lines(mydat$x[order(mydat$x)], fit[order(mydat$x)], col="blue", lwd=2 )
  lines(mod3, lwd= 2, col="green")	
  lines(mod4, lwd= 2, col="red")	
  
# Compute RSS
  sum(residuals(mod1)^2)		## RSS linear model
  sum(residuals(mod2)^2) 		## RSS log model
  y.o <- y[order(x)]			
  sum((y.o - mod3$y)^2) 		## RSS local reg
  sum((y.o - mod4$y)^2) 		## RSS 2nd local reg	

# Since model 4 has lowest RSS, it's the best fitting model
  
  rm(list=ls()) 
##### END #####
