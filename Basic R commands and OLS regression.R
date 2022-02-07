                #####################################
                #       Author: Thi Hoa Nguyen      #
                #   Topic 1 Basic R and OLS         #
                #####################################

                
################ 1 Basic #####
                
# Set seed for allowing your results to be replicated
  set.seed(123) 

# Generate random variable x with normal distribution, N=500
  x <- rnorm(500)

# See whole data
  x

# Histogram
  hist(x)	

# Plot
  plot(x)	

# Density plot
  plot(density(x))

# Help regardin ga function
  ?rnorm

# Similarly, generate y and plots
  y <- rnorm(500, mean = 2)	
  plot(density(x), xlim=c(-6,6), main="Density plot")
  lines(density(y), col="blue")	

################ 2 Data structure #####

# A vector
  a <- c(1,2,2,4,5,7)
  a

# A 2x2 matrix
  M <- matrix(c(1,2,3,4), nrow=2, ncol=2)  
  M

# A 4x1 matrix
  M <- matrix(c(1,2,3,4), nrow=4, ncol=1)
  M

# List all objectS in memory
  ls()

# Subset of a vector. First 10 elements of x
  x[1:10]

# subset of a matrix. First 4 elements of row 1
  M[1:4,1]


  E <- matrix(c(1,2,3,4,5,6,7,8,9,10,11,12), nrow = 3, ncol = 4)
  E
  E[1,2]						#E[row num, col num]. Show element of row 1, col 2
  E[1:3,1]          #First 3 elements of col 1
  E[1:3,2]          #First 3 elements of col 2
  E[2:3,2]          #Elements 2nd and 3rd of col 2
  E[2:3,1:2]				#Elements 2nd and 3rd of 1st and 2nd columns

# Remove objects no longer needed; remove all: rm(list=ls())  
  rm(x, y) 						

# Uniform distribution: N=100, min=1, max=5
  x <- runif(100,1,5)
  hist(x)		
  y <- 2 + 3 * x + rnorm(100)

# Scatterplot
  plot(x,y)


################ 3 Data frame #####
# To assign the name for the data
  mydat <- data.frame(y = y, x = x)	
  
# To see the data  
  mydat			
  
# With the name on the axes
  mydat <- data.frame(namey = y, namex = x)

# To see the first observations    
  head(mydat)
  mydat[1:10,]
  

  
################ 4 Linear regression #####

# Linear model    
  mod1 <- lm(y ~ x, data=mydat)
  
# Show result of model "mod1"  
  summary(mod1)				
  
# 2D Plot  
  plot(x,y)
  
# Add straight line to a plot  
  abline(coef(mod1), col="blue")		

# A larger random component (larger error)
  y <- 2 + 3 * x + 5*rnorm(100)
  mod1 <- lm(y ~ x)
  summary(mod1)
  plot(x,y)
  abline(coef(mod1), col="blue")
  
################ 5 Linear model with non-Gaussian errors #####
  
  # when the error has uniform distribution (not normally distributed). i.e non-Gaussian errors.
  # The model isn't biased but inefficient.
  y <- 2 + 3 * x + runif(100, -2, 2)		
  plot(x,y)  			
  mod2 <- lm(y ~ x)
  summary(mod2)
  abline(coef(mod2), col="blue")
  
################ END #####