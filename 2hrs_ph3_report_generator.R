source('funcs.R')

# benchmark models  ----

report.full(model = 'snaive()',
            series = '2hrs ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

report.full(model = 'meanf()',
            series = '2hrs ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

report.full(model = 'naive()',
            series = '2hrs ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

# 2hrs ph3 Simple Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0)) ----
# arima(1,0,0)(1,0,0)

# One positive lag in ACF & PACF - AR signature
report(model = 'Arima(order=c(1, 0, 0))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3)

# Previous model had residual significant at lag = 12 (24hrs seasonality)
report(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3)

# Try with a transformation, avoid -Inf values
report(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0))',
       series = '2hrs ph3',
       transformation = 'log() %>% (function(d) {d[d==-Inf]<-0; d})',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3)

# Try also with a MA term since neither the ACF nor the PACF
# decays slowly or sinusoidally in order to be sure AR or MA is definitely usable
report(model = 'Arima(order=c(0, 0, 1))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3)

# Same as above, but with seasonal terms, again there were significant residuals 
# at lag 12
report(model = 'Arima(order=c(0, 0, 1), seasonal=c(0, 0, 1))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3)

#Fourier terms for the model AR(1) and SAR(1)
#K was randomly chosen by me
report(model = 'Arima(order=c(0, 0, 1), seasonal=c(0, 0, 1), xreg=fourier(., K=5))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3,
       xreg='fourier(., K=5, h=h)')

#Fourier terms for the model MA(1) and SMA(1)
#K was randomly chosen by me
report(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), xreg=fourier(., K=5))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3,
       xreg='fourier(., K=5, h=h)')

#Try with differences, although no indication of trend or seasonal-trend
report(model = 'Arima(order=c(0, 1, 1), seasonal=c(0, 1, 1))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3)

#Variation on the above
report(model = 'Arima(order=c(1, 1, 0), seasonal=c(1, 1, 0))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3)

report(model = 'auto.arima()',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3)

# 2hrs ph3 -1 day forecast - Simple Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0)) ----
# fourier arima(1,0,0)(1,0,0)
report(model = 'Arima(order=c(1, 0, 0))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 1)

report(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 1)

report(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0))',
       series = '2hrs ph3',
       transformation = 'log() %>% (function(d) {d[d==-Inf]<-0; d})',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 1)

report(model = 'Arima(order=c(0, 0, 1))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 1)

report(model = 'Arima(order=c(0, 0, 1), seasonal=c(0, 0, 1))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 1)

report(model = 'Arima(order=c(0, 0, 1), seasonal=c(0, 0, 1), xreg=fourier(., K=5))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 1,
       xreg='fourier(., K=5, h=h)')

report(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), xreg=fourier(., K=5))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 1,
       xreg='fourier(., K=5, h=h)')

report(model = 'Arima(order=c(0, 1, 1), seasonal=c(0, 1, 1))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 1)

report(model = 'Arima(order=c(1, 1, 0), seasonal=c(1, 1, 0))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 1)

report(model = 'auto.arima()',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 1)

# 2hrs ph3 from the front ----

report(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = 0,
       traindays = 7,
       testdays = 1)

report(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), xreg=fourier(., K=5))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = 0,
       traindays = 7,
       testdays = 1,
       xreg='fourier(., K=5, h=h)')

report(model = 'auto.arima()',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = 0,
       traindays = 7,
       testdays = 1)

# 2hrs ph3 full - apply the best models so far on the full data----
report.full(model = 'auto.arima()',
       series = '2hrs ph3',
       transformation = 'identity()',
       traindays = 7,
       testdays = 1)

#https://stackoverflow.com/questions/7233288/non-stationary-seasonal-ar-part-from-css-error-in-r

# This model predicts the new value as a multiple of the previous (seasonal) value plus a constant
# This model assumes mean reverting behaviour (no trend - no diffs) which for house-hold is true (energy consumption is the same, on average)
report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="ML")',
            series = '2hrs ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

# Fit the seasonality using fourier terms nad the errors using the logic above
report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="ML", xreg=fourier(., K=5))',
            series = '2hrs ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1,
            xreg='fourier(., K=5, h=h)')

report.full(model = 'auto.arima(xreg=fourier(., K=5))',
            series = '2hrs ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1,
            xreg='fourier(., K=5, h=h)')


# ARIMA(1,0,0)(1,0,0) on 4:1 days and ARIMA(1, 0, 0)(1, 0, 0) with Fourier(k=5) on 7:1 ----

# TODO:Fit the seasonality using fourier terms and the errors using the logic above

best.fcast.2hrsPh3 <- NULL
best.traindays <- 0
best.testdays <- 0

for(traindays in 3:7)
{
  for(testdays in 1:3)
  {
    print(paste("Trying", traindays, "train days and", testdays, "test days"))
    current <- fullforecast(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="ML")',
                dataset = datasets[['2hrs ph3']]$series,
                transformation = 'identity()',
                traindays = traindays,
                testdays = testdays,
                xreg=NULL)
    
    if(is.null(best.fcast.2hrsPh3) || current$accuracy[[2]] < best.fcast.2hrsPh3$accuracy[[2]])
    {
      best.fcast.2hrsPh3 <- current
      best.traindays <- traindays
      best.testdays <- testdays
    }
  }
}

report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="ML")',
            series = '2hrs ph3',
            transformation = 'identity()',
            traindays = best.traindays, # 4
            testdays = best.testdays) # 1

best.fcast.fourier.2hrsPh3 <- NULL
best.fourier.traindays <- 0
best.fourier.testdays <- 0

for(traindays in 3:7)
{
  for(testdays in 1:3)
  {
    print(paste("Trying", traindays, "train days and", testdays, "test days"))
    current <- fullforecast(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="ML", xreg=fourier(., K=5))',
                            dataset = datasets[['2hrs ph3']]$series,
                            transformation = 'identity()',
                            traindays = traindays,
                            testdays = testdays,
                            xreg='fourier(., K=5, h=h)')
    
    if(is.null(best.fcast.fourier.2hrsPh3) || current$accuracy[[2]] < best.fcast.fourier.2hrsPh3$accuracy[[2]])
    {
      best.fcast.fourier.2hrsPh3 <- current
      best.fourier.traindays <- traindays
      best.fourier.testdays <- testdays
    }
    
  }
}

report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="ML", xreg=fourier(., K=5))',
            series = '2hrs ph3',
            transformation = 'identity()',
            traindays = best.fourier.traindays, # 7
            testdays = best.fourier.testdays, # 1
            xreg='fourier(., K=5, h=h)')

# Find best K for the above model ARIMA(1,0,0)(1,0,0) ----

best.fcast.k.2hrsPh3 <- NULL
best.k <- 0
#K must be not be greater than period/2
for(k in 1:(frequency(datasets[['2hrs ph3']]$series)/2))
{
  print(paste("Trying k =", k))
  m <- paste('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="ML", xreg=fourier(., K=', k, '))', sep='')
  xreg <- paste('fourier(., h=h, K=', k, ')')
  current <- fullforecast(model = m,
                          dataset = datasets[['2hrs ph3']]$series,
                          transformation = 'identity()',
                          traindays = best.fourier.traindays, # 7
                          testdays = best.fourier.testdays, # 1
                          xreg=xreg)
  
  if(is.null(best.fcast.k.2hrsPh3) || current$accuracy[[2]] < best.fcast.k.2hrsPh3$accuracy[[2]])
  {
    best.fcast.k.2hrsPh3 <- current
    best.k <- k
  }
}

report.full(model = paste('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="ML", xreg=fourier(., K=', best.k, '))', sep=''),
            series = '2hrs ph3',
            transformation = 'identity()',
            traindays = best.fourier.traindays, # 7
            testdays = best.fourier.testdays, # 1
            xreg = paste('fourier(., h=h, K=', best.k, ')'))


# Best model: ARIMA(1, 0, 0)(1, 0, 0) (K=1) RMSE 320----





# observation based modelling ----
report(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="ML")',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = 0,
       traindays = 24,
       testdays = 12,
       obs = TRUE)

report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS")',
            series = '2hrs ph3',
            transformation = 'identity()',
            traindays = 48,
            testdays = 1,
            obs=TRUE)

report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="ML", xreg=fourier(., K=1))',
            series = '2hrs ph3',
            transformation = 'identity()',
            traindays = 48,
            testdays = 1,
            xreg = 'fourier(., h=h, K=1)',
            obs = TRUE)
