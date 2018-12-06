source('funcs.R')

# benchmark models  ----

report.full(model = 'snaive()',
            series = '2hrs ph1',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

report.full(model = 'meanf()',
            series = '2hrs ph1',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

report.full(model = 'naive()',
            series = '2hrs ph1',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

# try to find the best ARIMA model  ----

# sinusoidal ACF and 1 lag in PACF from the beginning of the dataset
report(model = 'Arima(order=c(1, 0, 0))',
       series = '2hrs ph1',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = 0,
       traindays = 7,
       testdays = 3)

# sinusoidal ACF and 1 lag in PACF
report(model = 'Arima(order=c(1, 0, 0))',
       series = '2hrs ph1',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3)

# significant values in lags of order 12
report(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0))',
       series = '2hrs ph1',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3)

# two significant lags around the seasonal period
report(model = 'Arima(order=c(1, 0, 0), seasonal=c(2, 0, 0), method="CSS")',
       series = '2hrs ph1',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3)

# seems fine, but the forecast is overall a bit too high
report(model = 'Arima(order=c(1, 0, 0), seasonal=c(2, 0, 0), include.mean=FALSE, method="CSS")',
       series = '2hrs ph1',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3)


# regression with ARMA errors on fourier terms, (tries with 2 seasonal AR terms, but results were worse)
report(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), xreg=fourier(., K=2), method="CSS")',
       series = '2hrs ph1',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3,
       xreg='fourier(., K=2, h=h)')


#  Find the best train:test days ratio for ARIMA(1,0,0)(1,0,0) ----

best.fcast.2hrsPh1 <- NULL
best.traindays <- 0
best.testdays <- 0

for(traindays in 3:7)
{
  for(testdays in 1:3)
  {
    print(paste("Trying", traindays, "train days and", testdays, "test days"))
    current <- fullforecast(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS")',
                            dataset = datasets[['2hrs ph1']]$series,
                            transformation = 'identity()',
                            traindays = traindays,
                            testdays = testdays,
                            xreg=NULL)
    
    if(is.null(best.fcast.2hrsPh1) || current$accuracy[[2]] < best.fcast.2hrsPh1$accuracy[[2]])
    {
      best.fcast.2hrsPh1 <- current
      best.traindays <- traindays
      best.testdays <- testdays
    }
  }
}

report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="ML")',
            series = '2hrs ph1',
            transformation = 'identity()',
            traindays = best.traindays, # 7
            testdays = best.testdays) # 2

# Skip over the step where I hardcode a fourier value ----
# Find best K for the above model ARIMA(1,0,0)(1,0,0) ----

best.fcast.k.2hrsPh1 <- NULL
best.k <- 0
#K must be not be greater than period/2
for(k in 1:(frequency(datasets[['2hrs ph1']]$series)/2))
{
  print(paste("Trying k =", k))
  m <- paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=fourier(., K=', k, '))')
  xreg <- paste0('fourier(., h=h, K=', k, ')')
  current <- fullforecast(model = m,
                          dataset = datasets[['2hrs ph1']]$series,
                          transformation = 'identity()',
                          traindays = best.traindays, # 7
                          testdays = best.testdays, # 2
                          xreg=xreg)
  
  if(is.null(best.fcast.k.2hrsPh1) || current$accuracy[[2]] < best.fcast.k.2hrsPh1$accuracy[[2]])
  {
    best.fcast.k.2hrsPh1 <- current
    best.k <- k
  }
}

report.full(model = paste('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=fourier(., K=', best.k, '))', sep=''),
            series = '2hrs ph1',
            transformation = 'identity()',
            traindays = best.traindays, # 7
            testdays = best.testdays, # 2
            xreg = paste('fourier(., h=h, K=', best.k, ')')) #2

# Best model: 7:2, ARIMA(1, 0, 0)(1, 0, 0), K=2, RMSE=469.4395 	MAE=223.396  ----
# 6th-9th obs dummies rmse=464, mae=222
# without seasonal part, only fourier: 467, mae=220
# dummies without seasonal part, only fourier: rmse=462, mae=218
report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=fourier(., K=2))',
            series = '2hrs ph1',
            transformation = 'identity()',
            traindays = 7,
            testdays = 2,
            xreg = 'fourier(., h=h, K=2)')

dummies.fcast <- quote(
  {cbind(
    dummies=getNthObsDummies(6, 3, h, frequency(.)),
    fourier(., h=h, K=2)
  )}
)

dummies.fit <- quote(
  {cbind(
    dummies=getNthObsDummies(6, 3, length(.), frequency(.)),
    fourier(., K=2)
  )}
)

report.full(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(dummies.fit), collapse='') ,')'),
  series = '2hrs ph1',
  transformation = 'identity()',
  traindays = 7,
  testdays = 2,
  xreg = paste0(deparse(dummies.fcast), collapse=''))

#rmse=462, mae=218
report.full(model = paste0('Arima(order=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(dummies.fit), collapse='') ,')'),
  series = '2hrs ph1',
  transformation = 'identity()',
  traindays = 7,
  testdays = 2,
  xreg = paste0(deparse(dummies.fcast), collapse=''))

# rmse=467, mae=220
report.full(model = 'Arima(order=c(1, 0, 0), method="CSS", xreg=fourier(., K=2))',
            series = '2hrs ph1',
            transformation = 'identity()',
            traindays = 7,
            testdays = 2,
            xreg = 'fourier(., h=h, K=2)')
# dummies on 6th day - not applicable, patterns are too crazy ----

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

# 7:2 rmse=486, mae=242
report.full(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(dailyD.fit), collapse='') ,')'),
            series = '2hrs ph1',
            transformation = 'identity()',
            traindays = 7,
            testdays = 2,
            xreg = paste0(deparse(dailyD.fcast), collapse=''))

# dummies on the 4-85th+1-5th obs (the "outliers") ----

best.fcast.dummy.2hrsPh1 <- NULL
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
                            dataset = datasets[['2hrs ph1']]$series,
                            transformation = 'identity()',
                            traindays = 7,
                            testdays = 2,
                            xreg = paste0(deparse(obsDummies.fcast), collapse=''))
    
    if(is.null(best.fcast.dummy.2hrsPh1) || current$accuracy[[2]] < best.fcast.dummy.2hrsPh1$accuracy[[2]])
    {
      best.fcast.dummy.2hrsPh1 <- current
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

# 7:2, dummies: 6:3, rmse=464, mae=222
report.full(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(bestObsDummies.fit), collapse='') ,')'),
            series = '2hrs ph1',
            transformation = 'identity()',
            traindays = 7,
            testdays = 2,
            xreg = paste0(deparse(bestObsDummies.fcast), collapse=''))

