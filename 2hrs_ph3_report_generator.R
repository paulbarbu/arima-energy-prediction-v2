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

# Some outlier present, try to ignore it by tsclean
report(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0))',
       series = '2hrs ph3',
       transformation = 'tsclean()',
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

# 2hrs ph3 full - apply the best models so far on the full data ----
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


#  Find the best train:test days ratio for ARIMA(1,0,0)(1,0,0) ----

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

#  Find the best train:test days ratio for ARIMA(1,0,0)(1,0,0) K=5----
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
            testdays = best.fourier.testdays, # 3
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
                          testdays = best.fourier.testdays, # 3
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
            testdays = best.fourier.testdays, # 3
            xreg = paste('fourier(., h=h, K=', best.k, ')')) #2


# Best model: ARIMA(1, 0, 0)(1, 0, 0) (K=2), 7:3, RMSE 318, MAE 182 || with tsclean RMSE 330, MAE 169 ----
# For tsclean() RMSE has gone up since it applies bigger penalty to the bigger errors (there are bigger errors since tsclean "smoothes" the data hence the outliers will give even bigger errors)
# For 5th obs: RMSE=318 ; MAE=179
report.full(output_format = 'pdf_document',
            model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="ML", xreg=fourier(., K=2))',
            series = '2hrs ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 3,
            xreg = 'fourier(., h=h, K=2)')

report.full(output_format = 'pdf_document',
            model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="ML", xreg=fourier(., K=2))',
            series = '2hrs ph3',
            transformation = 'tsclean()',
            traindays = 7,
            testdays = 3,
            xreg = 'fourier(., h=h, K=2)')

fifthOD.fcast <- quote(
  {cbind(
    dummies=getNthObsDummies(5, 1, h, frequency(.)),
    fourier(., h=h, K=2)
  )}
)

fifthOD.fit <- quote(
  {cbind(
    dummies=getNthObsDummies(5, 1, length(.), frequency(.)),
    fourier(., K=2)
  )}
)

report.full(#output_format = 'pdf_document',
            model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="ML", xreg=', paste0(deparse(fifthOD.fit), collapse='') ,')'),
            series = '2hrs ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 3,
            xreg = paste0(deparse(fifthOD.fcast), collapse=''))

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

report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=fourier(., K=2))',
            series = '2hrs ph3',
            transformation = 'identity()',
            traindays = 48,
            testdays = 2,
            xreg = 'fourier(., h=h, K=2)',
            obs = TRUE)

best.fcast.obs.2hrsPh3 <- NULL
best.testobs <- 0
trainobs <- 4*frequency(datasets[['2hrs ph3']]$series) #48

for(testobs in 1:(frequency(datasets[['2hrs ph3']]$series) - 1)) # 1:11
{
  print(paste("Trying", trainobs, "train observations and", testobs, "test observations"))
  current <- fullforecast.obs(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="ML")',
                          dataset = datasets[['2hrs ph3']]$series,
                          transformation = 'identity()',
                          trainobs = trainobs,
                          testobs = testobs,
                          xreg=NULL)
  
  if(is.null(best.fcast.obs.2hrsPh3) || current$accuracy[[2]] < best.fcast.obs.2hrsPh3$accuracy[[2]])
  {
    best.fcast.obs.2hrsPh3 <- current
    best.testobs <- testobs
  }
}

report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="ML")',
            series = '2hrs ph3',
            transformation = 'identity()',
            traindays = trainobs,
            testdays = best.testobs, # 9
            obs = TRUE)


best.fcast.obs.fourier.2hrsPh3 <- NULL
best.fourier.testobs <- 0

for(testobs in 2:(frequency(datasets[['2hrs ph3']]$series) - 1)) # 2:11
{
  print(paste("Trying", trainobs, "train observations and", testobs, "test observations"))
  current <- fullforecast.obs(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), xreg=fourier(., K=2), method="CSS")',
                              dataset = datasets[['2hrs ph3']]$series,
                              transformation = 'identity()',
                              trainobs = trainobs,
                              testobs = testobs,
                              xreg = 'fourier(., h=h, K=2)')
  
  if(is.null(best.fcast.obs.fourier.2hrsPh3) || current$accuracy[[2]] < best.fcast.obs.fourier.2hrsPh3$accuracy[[2]])
  {
    best.fcast.obs.fourier.2hrsPh3 <- current
    best.fourier.testobs <- testobs
  }
}

report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), xreg=fourier(., K=2), method="ML")',
            series = '2hrs ph3',
            transformation = 'identity()',
            traindays = trainobs,
            testdays = best.fourier.testobs, # 11
            xreg = 'fourier(., h=h, K=2)',
            obs = TRUE)

# no better model was found this way (by using only several observations ---- 

# tsclean on the best model ----
report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="ML", xreg=fourier(., K=2))',
            series = '2hrs ph3',
            transformation = 'tsclean()',
            traindays = 7,
            testdays = 3,
            xreg = 'fourier(., h=h, K=2)')

# dummies on 6th day - 6th day has an "outlier" ----

sixthDD.fcast <- quote(
  {cbind(
    dummies=get6thDayDummies(h, frequency(.), start(.)[[1]]),
    fourier(., h=h, K=2)
  )}
)

sixthDD.fit <- quote(
  {cbind(
    dummies=get6thDayDummies(length(.), frequency(.), start(.)[[1]]),
    fourier(., K=2)
  )}
)

report(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="ML", xreg=', paste0(deparse(sixthDD.fit), collapse='') ,')'),
            series = '2hrs ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 3,
            xreg = paste0(deparse(sixthDD.fcast), collapse=''))

#7:3 rmse=330 mae=193
report.full(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(sixthDD.fit), collapse='') ,')'),
            series = '2hrs ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 3,
            xreg = paste0(deparse(sixthDD.fcast), collapse=''))


(datasets[['2hrs ph3']]$series %>% 
{eval(x)} ->
    external.regressors)
plot(external.regressors)

# dummies on every weekday ----

dailyD.fcast <- quote(
  {cbind(
    dummies=getDailyDummies(h, frequency(.), start(.)[[1]]),
    fourier(., h=h, K=2)
  )}
)

dailyD.fit <- quote(
  {cbind(
    dummies=getDailyDummies(length(.), frequency(.), start(.)[[1]]),
    fourier(., K=2)
  )}
)

report(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(dailyD.fit), collapse='') ,')'),
       series = '2hrs ph3',
       transformation = 'identity()',
       traindays = 7,
       testdays = 3,
       xreg = paste0(deparse(dailyD.fcast), collapse=''))

# 7:3 rmse=333, mae=202
report.full(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="ML", xreg=', paste0(deparse(dailyD.fit), collapse='') ,')'),
       series = '2hrs ph3',
       transformation = 'identity()',
       traindays = 7,
       testdays = 3,
       xreg = paste0(deparse(dailyD.fcast), collapse=''))
# dummies on the 5th obs (the "outlier") ----

fifthOD.fcast <- quote(
  {cbind(
    dummies=getNthObsDummies(5, 1, h, frequency(.)),
    fourier(., h=h, K=2)
  )}
)

fifthOD.fit <- quote(
  {cbind(
    dummies=getNthObsDummies(5, 1, length(.), frequency(.)),
    fourier(., K=2)
  )}
)

report(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(fifthOD.fit), collapse='') ,')'),
       series = '2hrs ph3',
       transformation = 'identity()',
       traindays = 7,
       testdays = 3,
       xreg = paste0(deparse(fifthOD.fcast), collapse=''))

# 7:3 rmse=318, mae=179
report.full(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="ML", xreg=', paste0(deparse(fifthOD.fit), collapse='') ,')'),
            series = '2hrs ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 3,
            xreg = paste0(deparse(fifthOD.fcast), collapse=''))

# dummies on the 5th to the 7th obs (the "outliers") ----
best.fcast.dummy.2hrsPh3 <- NULL
best.startDummy <- 0
best.lenDummy <- 0

for(startDummy in 4:8)
{
  for(lenDummy in 1:5)
  {
    print(paste("Trying startDummy =", startDummy, ", length =", lenDummy))
    
    obsDummies.fcast <- substitute(
      {cbind(
        dummies=getNthObsDummies(startDummy, lenDummy, h, frequency(.)),
        fourier(., h=h, K=2)
      )},
      list(startDummy=startDummy, lenDummy=lenDummy)
    )
    
    obsDummies.fit <- substitute(
      {cbind(
        dummies=getNthObsDummies(startDummy, lenDummy, length(.), frequency(.)),
        fourier(., K=2)
      )},
      list(startDummy=startDummy, lenDummy=lenDummy)
    )
    
    current <- fullforecast(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(obsDummies.fit), collapse='') ,')'),
                            dataset = datasets[['2hrs ph3']]$series,
                            transformation = 'identity()',
                            traindays = 7,
                            testdays = 3,
                            xreg = paste0(deparse(obsDummies.fcast), collapse=''))
    
    if(is.null(best.fcast.dummy.2hrsPh3) || current$accuracy[[2]] < best.fcast.dummy.2hrsPh3$accuracy[[2]])
    {
      best.fcast.dummy.2hrsPh3 <- current
      best.startDummy <- startDummy
      best.lenDummy <- lenDummy
    }
    
  }
}

bestObsDummies.fcast <- substitute(
  {cbind(
    dummies=getNthObsDummies(best.startDummy, best.lenDummy, h, frequency(.)),
    fourier(., h=h, K=2)
  )},
  list(best.startDummy = best.startDummy, best.lenDummy = best.lenDummy)
)

bestObsDummies.fit <- substitute(
  {cbind(
    dummies=getNthObsDummies(best.startDummy, best.lenDummy, length(.), frequency(.)),
    fourier(., K=2)
  )},
  list(best.startDummy = best.startDummy, best.lenDummy = best.lenDummy)
)

# 7:3, dummies 4:4, rmse=318, mae=184
report.full(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="ML", xreg=', paste0(deparse(bestObsDummies.fit), collapse='') ,')'),
            series = '2hrs ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 3,
            xreg = paste0(deparse(bestObsDummies.fcast), collapse=''))




# other tries ----
report.full(model = 'Arima(order=c(2, 1, 1), seasonal=c(2, 0, 0))',
            series = '2hrs ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="ML", xreg=fourier(., K=1), include.mean=FALSE)',
            series = '2hrs ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1,
            xreg = 'fourier(., h=h, K=1)')




#series = msts(read.csv(file = '../energyarima/benchmarks/2hrs-ph3.txt'), seasonal.periods=c((24*60)/120, (24*60*7)/120), start=0),
# report.full(
#             model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=fourier(., K=c(1, 1)))',
#             series = '2hrs ph3',
#             transformation = 'identity()',
#             traindays = 7,
#             testdays = 1,
#             xreg = 'fourier(., h=h, K=c(1, 1))')

# if predicting one single point ahead it works, otherwise I produce too many fcast points
fullforecast.obs(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=fourier(., K=1))',
            dataset = datasets[['2hrs ph3']]$series,
            transformation = 'identity()',
            trainobs = 48,
            testobs = 12,
            xreg = 'fourier(., h=h, K=1)')

fullforecast.serial.obs(model = 'Arima(order=c(1, 0, 0), method="CSS")',
                 dataset = head(datasets[['2hrs ph3']]$series, 100),
                 transformation = 'identity()',
                 trainobs = 48,
                 testobs = 12,
                 xreg = NULL)

fullforecast(model = 'Arima(order=c(1, 0, 0), method="CSS")',
                        dataset = head(datasets[['2hrs ph3']]$series, 1000),
                        transformation = 'identity()',
                        traindays = 7,
                        testdays = 2,
                        xreg = NULL)

report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=fourier(., K=1))',
                         series = '2hrs ph3',
                         transformation = 'identity()',
                         traindays = 48,
                         testdays = 12,
                         xreg = 'fourier(., h=h, K=1)',
                         obs=TRUE)
