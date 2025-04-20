##### VECM analysis of carbon, gas and electricity prices ####

### 1. Load data & packages
{
  rm(list=ls())
  
  setwd("insert directory")
  
  data <- read.table("ETSprices.csv", sep = ";", head = TRUE )

### data on carbon price, gas price, electricity price in UK (euro)
### df = dummy for the holiday days
### dc = dummy for big spike in the carbon price

### from 1st april 2005 to 30st june 2006

# install.packages("urca","vars", "tseries")

library(urca)
library(vars)
library(tseries)

### Create a simple function which lags variables 

lag <- function(X)
{
  lag <- c(NA,X[-length(X)])
  return(lag)
}
}
### 2. Plot series ####
{
  plot(data$Pcarb, type="l")
  
  plot(data$Pukel, type="l", ylim=c(0,280))
  lines(data$Pukgas, type="l", col="blue")  #price of electricity and price of gas are highly correlated, even by eyes ^^
  lines(data$Pcarb, type="l", col="red")
}

### 3. Stationarity and unit root tests ####
{
  acf(data$Pukel)
  adf.test(data$Pukel)   #Reject the null hypo of non-stationarity => price of electricity is stationary
  kpss.test(data$Pukel)  #Reject the null hy po of stationarity => non stationary series => two tests conflict
  
  acf(data$Pukgas)
  adf.test(data$Pukgas)  #both tests agree that price of gas is non-stationary
  kpss.test(data$Pukgas)
  
  adf.test(data$Pcarb)  #both tests agree that price of carbon is non-stationary
  kpss.test(data$Pcarb)
}

### 4. ESTIMATE VAR to understand the number of lags in the cointegration analysis ####
{
  y <- cbind(log(data$Pukel), log(data$Pukgas), log(data$Pcarb)) #Nr of endogenous variable: k=3
  colnames(y) <- c("elect", "gas", "carbon")
  head(y)
  
  x <- cbind(data$Df, data$Dc)
  colnames(x) <- c("Df", "Dc")
  head(x)
  
  m1 <- VAR(y, type = c("both"), lag.max = 10, ic = c("AIC")) 
  # y: khai bao rang bien endogenous, 
  #ic = c("AIC"): select the number of lags using AIC criteria
  #lag.max = 10: maximum number of lags is 10
  #type = c("both")?
  summary(m1)
  #R2 is very high but we can't trust it because of non-stationary series.
  #See the correlation marix of residuals: corr(elect, gas)=0.73 => very high.
  
  serial.test(m1, type="BG", lags.bg = 5)  
  # test the correlation between the errors in the model to be sure
  serial.test(m1, type="BG", lags.bg = 10) 
  # p-val=0.04=> reject the null hypo of non-autocorrelation => there is autocorrelation between the errors
  
}

### Add exogenous variables to the VAR (dummies to capture the change in carbon price)
{
  data$Dc1 <- lag(data$Dc)
  data$Dc2 <- lag(data$Dc1)
  data$Dc3 <- lag(data$Dc2)
  
  x <- cbind(data$Df, data$Dc, data$Dc1, data$Dc2)
  colnames(x) <- c("Df", "Dc", "Dc1", "Dc2")
  
  m1 <- VAR(y, type = c("both"), exogen = x, season = 5, lag.max = 10, ic = c("SC") )
  summary(m1)		#season =5: dummy for every 5 days
  # type = c("both"): to include both the constant (intercept) and the trend
  
  plot(m1)      #?? what does it graph tell us?
  
  serial.test(m1, type="BG", lags.bg = 5)		## passed
  #?? Error in cbind(ylagged, resids.l) : number of rows of matrices must match (see arg 2)
}

### 5. Check for cointegration ####
{
  ## TRACE TEST
  
  m2 <- ca.jo(y, type=c("trace"), season = 5, dumvar = x, ecdet = c("const"), K = 2,
              spec = "transitory")
  
  ## MAX EIGENVALUE TEST
  
  m2 <- ca.jo(y, type=c("eigen"), season = 5, dumvar = x, ecdet = c("const"), K = 2,
              spec = "transitory")
  
  summary(m2)
  #both tests give the same result: when r=0 => reject. when r=1 => accept => r=1 => 1 cointegrating combination
}

### 6. ESTIMATE THE VECM (input is the previous model) ####
{
  vecm <- cajorls(m2, r = 1)  #r = 1: what we found from step 4
  # electricity price is normalized to 1.
  # no exclusion restriction is needed cuz r=1
  # What happen: In the short run
  summary(vecm$rlm)
  #We get 3 models inside. We can trust R-squared in those models cuz the variables are stationary.
  #Model 1: elect.d ~ ect1 + Df + Dc + Dc1 + Dc2 + sd1 + sd2 + sd3 + sd4 + elect.dl1 + gas.dl1 + carbon.dl1 - 1
  #         ect1 = -0.26*** => electricity price is adjusted towards equilibrium by 26% in the first period
  #Model 2: gas.d ~ ect1 + Df + Dc + Dc1 + Dc2 + sd1 + sd2 + sd3 + sd4 + elect.dl1 + gas.dl1 + carbon.dl1 - 1         
  #         ect1 = -0.07 => insignificant=> gas price is NOT adjusted towards long-run equilibrium
  #Model 3: carbon.d ~ ect1 + Df + Dc + Dc1 + Dc2 + sd1 + sd2 + sd3 + sd4 + elect.dl1 + gas.dl1 + carbon.dl1 - 1         
  #         ect1 = -0.007 => insignificant=> carbon price is NOT adjusted towards long-run equilibrium
  #   => Gas price and carbon price are weakly exogenous in the long run
  
  # What happen: In the long run
  vecm$beta
  #result: BE CAREFUL WITH THE SIGNS OF THE COEFFICIENT!
  #ect1
  #elect.l1   1.0000000
  #gas.l1    -0.6564023 =>In the long run, an increase by 1$ in gas price leads to 0.65$ INCREASE in electricity price
  #carbon.l1 -0.3370133 =>In the long run, an increase by 1$ in carbon price leads to 0.34$ INCREASE in electricity price
  #constant  -0.6727322
  # WHICH SHOWS: alfa[P_elect(t) - 0.67 - 0.65P_gas(t-1) - 0.34P_car(t-1)]
  # The relationship to be intepreted in the long run is inside []: P_elect(t) = 0.67 + 0.65P_gas(t-1) + 0.34P_car(t-1)
  # This does not give the significance level for betas. The procedure of finding the significance level for betas is complicated!
  # Prof said to skip this, he ran and found that they are significant.
  
  resids <- resid(vecm$rlm)
  cor(resids)
  #Correlation between electricity price and gas price is still high
}

### 7. Impulse responce functions (impact of a change in gas price) ####
{
  mv <- vec2var(m2, r = 1)
  
  plot(irf(mv, impulse="gas", n.ahead = 20))
  #which shows the cumulative impact of the gas price which is proved to be 
  # exogeous in the long run not really understand this part
}