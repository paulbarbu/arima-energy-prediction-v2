source('funcs.R')

# benchmark models  ----

report.full(model = 'snaive()',
            series = '2hrs ph2',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

report.full(model = 'meanf()',
            series = '2hrs ph2',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

report.full(model = 'naive()',
            series = '2hrs ph2',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

# try to find the best ARIMA model  ----

# sinusoidal ACF and 1 lag in PACF
report(model = 'Arima(order=c(1, 0, 0))',
       series = '2hrs ph2',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3)

# significant values in lags of order 12
report(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0))',
       series = '2hrs ph2',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3)

# seems fine, but the forecast is overall a bit too high
report(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), include.mean=FALSE)',
       series = '2hrs ph2',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3)

# try with MA & seasonal MA terms since the data could fit the "pattern" - worse than AR
report(model = 'Arima(order=c(0, 0, 1), seasonal=c(0, 0, 1))',
       series = '2hrs ph2',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3)

# regression with ARMA errors on fourier terms
report(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), xreg=fourier(., K=2))',
       series = '2hrs ph2',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -8,
       traindays = 7,
       testdays = 1,
       xreg='fourier(., K=2, h=h)')


#  Find the best train:test days ratio for ARIMA(1,0,0)(1,0,0) ----

best.fcast.2hrsPh2 <- NULL
best.traindays <- 0
best.testdays <- 0

for(traindays in 3:7)
{
  for(testdays in 1:3)
  {
    print(paste("Trying", traindays, "train days and", testdays, "test days"))
    current <- fullforecast(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS")',
                            dataset = datasets[['2hrs ph2']]$series,
                            transformation = 'identity()',
                            traindays = traindays,
                            testdays = testdays,
                            xreg=NULL)
    
    if(is.null(best.fcast.2hrsPh2) || current$accuracy[[2]] < best.fcast.2hrsPh2$accuracy[[2]])
    {
      best.fcast.2hrsPh2 <- current
      best.traindays <- traindays
      best.testdays <- testdays
    }
  }
}

report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="ML")',
            series = '2hrs ph2',
            transformation = 'identity()',
            traindays = best.traindays, # 7
            testdays = best.testdays) # 3

# Skip over the step where I hardcode a fourier value ----
# Find best K for the above model ARIMA(1,0,0)(1,0,0) ----

best.fcast.k.2hrsPh2 <- NULL
best.k <- 0
#K must be not be greater than period/2
for(k in 1:(frequency(datasets[['2hrs ph2']]$series)/2))
{
  print(paste("Trying k =", k))
  m <- paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=fourier(., K=', k, '))')
  xreg <- paste0('fourier(., h=h, K=', k, ')')
  current <- fullforecast(model = m,
                          dataset = datasets[['2hrs ph2']]$series,
                          transformation = 'identity()',
                          traindays = best.traindays, # 7
                          testdays = best.testdays, # 3
                          xreg=xreg)
  
  if(is.null(best.fcast.k.2hrsPh2) || current$accuracy[[2]] < best.fcast.k.2hrsPh2$accuracy[[2]])
  {
    best.fcast.k.2hrsPh2 <- current
    best.k <- k
  }
}

report.full(model = paste('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=fourier(., K=', best.k, '))', sep=''),
            series = '2hrs ph2',
            transformation = 'identity()',
            traindays = best.traindays, # 7
            testdays = best.testdays, # 3
            xreg = paste('fourier(., h=h, K=', best.k, ')')) #1

# Best model: 7:3, ARIMA(1, 0, 0)(1, 0, 0), K=1, RMSE=373.8851 MAE=184.5098 ----
# dummies 4:4 RMSE=370  MAE=178
# no SAR term, dummies 4:4 RMSE=366  MAE=178
report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=fourier(., K=1))',
            series = '2hrs ph2',
            transformation = 'identity()',
            traindays = 7,
            testdays = 3,
            xreg = 'fourier(., h=h, K=1)')

bestObsDummies.fcast <- substitute(
  {cbind(
    dummies=getNthObsDummies(4, 4, h, frequency(.)),
    fourier(., h=h, K=1)
  )}
)

bestObsDummies.fit <- substitute(
  {cbind(
    dummies=getNthObsDummies(4, 4, length(.), frequency(.)),
    fourier(., K=1)
  )}
)

# 7:3, dummies 4:4, rmse=370.118, mae=178.8
report.full(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="ML", xreg=', paste0(deparse(bestObsDummies.fit), collapse='') ,')'),
            series = '2hrs ph2',
            transformation = 'identity()',
            traindays = 7,
            testdays = 3,
            xreg = paste0(deparse(bestObsDummies.fcast), collapse=''))

report.full(model = paste0('Arima(order=c(1, 0, 0), method="ML", xreg=', paste0(deparse(bestObsDummies.fit), collapse='') ,')'),
            series = '2hrs ph2',
            transformation = 'identity()',
            traindays = 7,
            testdays = 3,
            xreg = paste0(deparse(bestObsDummies.fcast), collapse=''))

# rmse=374 mae=185 - worse
report.full(model = 'Arima(order=c(1, 0, 0), method="CSS", xreg=fourier(., K=1))',
            series = '2hrs ph2',
            transformation = 'identity()',
            traindays = 7,
            testdays = 3,
            xreg = 'fourier(., h=h, K=1)')

# dummies on 6th day - 6th day has an "outlier" ----

sixthDD.fcast <- quote(
  {cbind(
    dummies=get6thDayDummies(h, frequency(.), start(.)[[1]]),
    fourier(., h=h, K=1)
  )}
)

sixthDD.fit <- quote(
  {cbind(
    dummies=get6thDayDummies(length(.), frequency(.), start(.)[[1]]),
    fourier(., K=1)
  )}
)

#7:3 rmse=408 mae=198
report.full(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(sixthDD.fit), collapse='') ,')'),
            series = '2hrs ph2',
            transformation = 'identity()',
            traindays = 7,
            testdays = 3,
            xreg = paste0(deparse(sixthDD.fcast), collapse=''))

# dummies on every weekday ----

dailyD.fcast <- quote(
  {cbind(
    dummies=getDailyDummies(h, frequency(.), start(.)[[1]]),
    fourier(., h=h, K=1)
  )}
)

dailyD.fit <- quote(
  {cbind(
    dummies=getDailyDummies(length(.), frequency(.), start(.)[[1]]),
    fourier(., K=1)
  )}
)

# 7:3 rmse=382, mae=196
report.full(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(dailyD.fit), collapse='') ,')'),
            series = '2hrs ph2',
            transformation = 'identity()',
            traindays = 7,
            testdays = 3,
            xreg = paste0(deparse(dailyD.fcast), collapse=''))
# dummies on the 5th obs (the "outlier") ----

fifthOD.fcast <- quote(
  {cbind(
    dummies=getNthObsDummies(5, 1, h, frequency(.)),
    fourier(., h=h, K=1)
  )}
)

fifthOD.fit <- quote(
  {cbind(
    dummies=getNthObsDummies(5, 1, length(.), frequency(.)),
    fourier(., K=1)
  )}
)

# 7:3 rmse=376, mae=183
report.full(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(fifthOD.fit), collapse='') ,')'),
            series = '2hrs ph2',
            transformation = 'identity()',
            traindays = 7,
            testdays = 3,
            xreg = paste0(deparse(fifthOD.fcast), collapse=''))

# dummies on the 4-8th+1-5th obs (the "outliers") ----

best.fcast.dummy.2hrsPh2 <- NULL
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
        fourier(., h=h, K=1)
      )},
      list(startDummy=startDummy, lenDummy=lenDummy)
    )
    
    obsDummies.fit <- substitute(
      {cbind(
        dummies=getNthObsDummies(startDummy, lenDummy, length(.), frequency(.)),
        fourier(., K=1)
      )},
      list(startDummy=startDummy, lenDummy=lenDummy)
    )
    
    current <- fullforecast(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(obsDummies.fit), collapse='') ,')'),
                            dataset = datasets[['2hrs ph2']]$series,
                            transformation = 'identity()',
                            traindays = 7,
                            testdays = 3,
                            xreg = paste0(deparse(obsDummies.fcast), collapse=''))
    
    if(is.null(best.fcast.dummy.2hrsPh2) || current$accuracy[[2]] < best.fcast.dummy.2hrsPh2$accuracy[[2]])
    {
      best.fcast.dummy.2hrsPh2 <- current
      best.startDummy <- startDummy
      best.lenDummy <- lenDummy
    }
    
  }
}

bestObsDummies.fcast <- substitute(
  {cbind(
    dummies=getNthObsDummies(best.startDummy, best.lenDummy, h, frequency(.)),
    fourier(., h=h, K=1)
  )},
  list(best.startDummy = best.startDummy, best.lenDummy = best.lenDummy)
)

bestObsDummies.fit <- substitute(
  {cbind(
    dummies=getNthObsDummies(best.startDummy, best.lenDummy, length(.), frequency(.)),
    fourier(., K=1)
  )},
  list(best.startDummy = best.startDummy, best.lenDummy = best.lenDummy)
)

# 7:3, dummies 4:4, rmse=370.118, mae=178.8
report.full(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="ML", xreg=', paste0(deparse(bestObsDummies.fit), collapse='') ,')'),
            series = '2hrs ph2',
            transformation = 'identity()',
            traindays = 7,
            testdays = 3,
            xreg = paste0(deparse(bestObsDummies.fcast), collapse=''))

