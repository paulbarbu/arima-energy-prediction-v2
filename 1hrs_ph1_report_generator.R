source('funcs.R')

# benchmark models  ----

report.full(model = 'snaive()',
            series = '1hrs ph1',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

report.full(model = 'meanf()',
            series = '1hrs ph1',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

report.full(model = 'naive()',
            series = '1hrs ph1',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

# try to find the best ARIMA model  ----
report(model = 'Arima(order=c(1, 0, 0))',
       series = '1hrs ph1',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3)

report(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0))',
       series = '1hrs ph1',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3)

# there are several significant lags around the sesonal one after introducing it
report(model = 'Arima(order=c(1, 0, 0), seasonal=c(2, 0, 0), method="CSS", include.mean=FALSE)',
       series = '1hrs ph1',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3)

#introduce fourier terms, again the higher SAR term is not helpful
report(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), xreg=fourier(., K=2))',
       series = '1hrs ph1',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3,
       xreg='fourier(., K=2, h=h)')

#  Find the best train:test days ratio for ARIMA(1,0,0)(1,0,0) ----

best.fcast.1hrsPh1 <- NULL
best.traindays <- 0
best.testdays <- 0

for(traindays in 3:7)
{
  for(testdays in 2:3) # for "1" I got: non-finite finite-difference value in optim
  {
    print(paste("Trying", traindays, "train days and", testdays, "test days"))
    current <- fullforecast(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS")',
                            dataset = datasets[['1hrs ph1']]$series,
                            transformation = 'identity()',
                            traindays = traindays,
                            testdays = testdays,
                            xreg=NULL)
    
    if(is.null(best.fcast.1hrsPh1) || current$accuracy[[2]] < best.fcast.1hrsPh1$accuracy[[2]])
    {
      best.fcast.1hrsPh1 <- current
      best.traindays <- traindays
      best.testdays <- testdays
    }
  }
}

report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS")',
            series = '1hrs ph1',
            transformation = 'identity()',
            traindays = best.traindays, # 7
            testdays = best.testdays) # 2

# Skip over the step where I hardcode a fourier value ----
# Find best K for the above model ARIMA(1,0,0)(1,0,0) ----

best.fcast.k.1hrsPh1 <- NULL
best.k <- 0
#K must be not be greater than period/2
for(k in 1:(frequency(datasets[['1hrs ph1']]$series)/2))
{
  print(paste("Trying k =", k))
  m <- paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=fourier(., K=', k, '))')
  xreg <- paste0('fourier(., h=h, K=', k, ')')
  current <- fullforecast(model = m,
                          dataset = datasets[['1hrs ph1']]$series,
                          transformation = 'identity()',
                          traindays = best.traindays, # 7
                          testdays = best.testdays, # 2
                          xreg=xreg)
  
  if(is.null(best.fcast.k.1hrsPh1) || current$accuracy[[2]] < best.fcast.k.1hrsPh1$accuracy[[2]])
  {
    best.fcast.k.1hrsPh1 <- current
    best.k <- k
  }
}

report.full(model = paste('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=fourier(., K=', best.k, '))', sep=''),
            series = '1hrs ph1',
            transformation = 'identity()',
            traindays = best.traindays, # 7
            testdays = best.testdays, # 2
            xreg = paste('fourier(., h=h, K=', best.k, ')')) #2


# Best model 7:2, ARIMA(1, 0, 0)(1, 0, 0), K=2, RMSE=455, MAE=218 ----
# 7:2, dummies 11:1, ARIMA(1, 0, 0)(1, 0, 0), K=2, RMSE=452, MAE=217
# 7:2, dummies 11:1, ARIMA(1, 0, 0), K=2, RMSE=449, MAE=215
# 7:2, ARIMA(1, 0, 0), K=2, RMSE=454, MAE=217
report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=fourier(., K=2))',
            series = '1hrs ph1',
            transformation = 'identity()',
            traindays = 7,
            testdays = 2,
            xreg = 'fourier(., h=h, K=2)')

bestObsDummies.fcast <- substitute(
  {cbind(
    dummies=getNthObsDummies(11, 1, h, frequency(.)),
    fourier(., h=h, K=2)
  )}
)

bestObsDummies.fit <- substitute(
  {cbind(
    dummies=getNthObsDummies(11, 1, length(.), frequency(.)),
    fourier(., K=2)
  )}
)

# 7:2, dummies: 11:1, rmse=452, mae=217
report.full(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(bestObsDummies.fit), collapse='') ,')'),
            series = '1hrs ph1',
            transformation = 'identity()',
            traindays = 7,
            testdays = 2,
            xreg = paste0(deparse(bestObsDummies.fcast), collapse=''))

report.full(model = paste0('Arima(order=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(bestObsDummies.fit), collapse='') ,')'),
            series = '1hrs ph1',
            transformation = 'identity()',
            traindays = 7,
            testdays = 2,
            xreg = paste0(deparse(bestObsDummies.fcast), collapse=''))

report.full(model = 'Arima(order=c(1, 0, 0), method="CSS", xreg=fourier(., K=2))',
            series = '1hrs ph1',
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

# 7:2 rmse=464, mae=231
report.full(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(dailyD.fit), collapse='') ,')'),
            series = '1hrs ph1',
            transformation = 'identity()',
            traindays = 7,
            testdays = 2,
            xreg = paste0(deparse(dailyD.fcast), collapse=''))

# dummies on the 5-12th+1-5th obs (the "outliers") ----

best.fcast.dummy.1hrsPh1 <- NULL
best.startDummy <- 0
best.lenDummy <- 0

for(startDummy in 5:12)
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
                            dataset = datasets[['1hrs ph1']]$series,
                            transformation = 'identity()',
                            traindays = 7,
                            testdays = 2,
                            xreg = paste0(deparse(obsDummies.fcast), collapse=''))
    
    if(is.null(best.fcast.dummy.1hrsPh1) || current$accuracy[[2]] < best.fcast.dummy.1hrsPh1$accuracy[[2]])
    {
      best.fcast.dummy.1hrsPh1 <- current
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

# 7:2, dummies: 11:1, rmse=452, mae=217
report.full(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(bestObsDummies.fit), collapse='') ,')'),
            series = '1hrs ph1',
            transformation = 'identity()',
            traindays = 7,
            testdays = 2,
            xreg = paste0(deparse(bestObsDummies.fcast), collapse=''))

