source('funcs.R')

# benchmark models  ----

report.full(model = 'snaive()',
            series = '1hrs ph2',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

report.full(model = 'meanf()',
            series = '1hrs ph2',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

report.full(model = 'naive()',
            series = '1hrs ph2',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

# try to find the best ARIMA model  ----

# sinusoidal ACF and 1 lag in PACF
report(model = 'Arima(order=c(1, 0, 0))',
       series = '1hrs ph2',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3)

# significant values in lags of order 12
report(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0))',
       series = '1hrs ph2',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3)

# significant values around lags 1 and seasonal lags so we try to incorporate them, the distribution of residuals seems slightly better (more normal)
report(model = 'Arima(order=c(2, 0, 0), seasonal=c(2, 0, 0))',
       series = '1hrs ph2',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3)

# try with MA & seasonal MA terms since the data could fit the "pattern" - worse than AR
report(model = 'Arima(order=c(0, 0, 1), seasonal=c(0, 0, 1))',
       series = '1hrs ph2',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3)

#  Find the best train:test days ratio for ARIMA(1,0,0)(1,0,0) ----

best.fcast.1hrsPh2 <- NULL
best.traindays <- 0
best.testdays <- 0

for(traindays in 3:7)
{
  for(testdays in 2:3) # for "1" I got: non-finite finite-difference value in optim
  {
    print(paste("Trying", traindays, "train days and", testdays, "test days"))
    current <- fullforecast(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS")',
                            dataset = datasets[['1hrs ph2']]$series,
                            transformation = 'identity()',
                            traindays = traindays,
                            testdays = testdays,
                            xreg=NULL)
    
    if(is.null(best.fcast.1hrsPh2) || current$accuracy[[2]] < best.fcast.1hrsPh2$accuracy[[2]])
    {
      best.fcast.1hrsPh2 <- current
      best.traindays <- traindays
      best.testdays <- testdays
    }
  }
}

report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS")',
            series = '1hrs ph2',
            transformation = 'identity()',
            traindays = best.traindays, # 4
            testdays = best.testdays) # 3

# Skip over the step where I hardcode a fourier value ----
# Find best K for the above model ARIMA(1,0,0)(1,0,0) ----

best.fcast.k.1hrsPh2 <- NULL
best.k <- 0
#K must be not be greater than period/2
for(k in 3:(frequency(datasets[['1hrs ph2']]$series)/2)) # for lower k, errors: 
{
  print(paste("Trying k =", k))
  m <- paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=fourier(., K=', k, '))')
  xreg <- paste0('fourier(., h=h, K=', k, ')')
  current <- fullforecast(model = m,
                          dataset = datasets[['1hrs ph2']]$series,
                          transformation = 'identity()',
                          traindays = best.traindays, # 4
                          testdays = best.testdays, # 3
                          xreg=xreg)
  
  if(is.null(best.fcast.k.1hrsPh2) || current$accuracy[[2]] < best.fcast.k.1hrsPh2$accuracy[[2]])
  {
    best.fcast.k.1hrsPh2 <- current
    best.k <- k
  }
}

report.full(model = paste('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=fourier(., K=', best.k, '))', sep=''),
            series = '1hrs ph2',
            transformation = 'identity()',
            traindays = best.traindays, # 4
            testdays = best.testdays, # 3
            xreg = paste('fourier(., h=h, K=', best.k, ')')) #3

# Best model: 4:3, ARIMA(1, 0, 0)(1, 0, 0), K=3, RMSE=350 MAE=165 ----
# 4:3, dummies 8:7, rmse=346, mae=164
# for dummies: an improvement from 2hrs, since I got it down with 1 for length of the dummies
# 4:3, ARIMA(1, 0, 0), K=3, RMSE=335 MAE=159
# 4:3, dummies 8:7, rmse=331, mae=157
report.full(output_format = 'pdf_document',
            model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=fourier(., K=3))',
            series = '1hrs ph2',
            transformation = 'identity()',
            traindays = 4,
            testdays = 3,
            xreg = 'fourier(., h=h, K=3)')

bestObsDummies.fcast <- substitute(
  {cbind(
    dummies=getNthObsDummies(8, 7, h, frequency(.)),
    fourier(., h=h, K=3)
  )}
)

bestObsDummies.fit <- substitute(
  {cbind(
    dummies=getNthObsDummies(8, 7, length(.), frequency(.)),
    fourier(., K=3)
  )}
)

# 4:3, dummies: 8:7, rmse=346, mae=164
report.full(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(bestObsDummies.fit), collapse='') ,')'),
            series = '1hrs ph2',
            transformation = 'identity()',
            traindays = 4,
            testdays = 3,
            xreg = paste0(deparse(bestObsDummies.fcast), collapse=''))

report.full(model = paste0('Arima(order=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(bestObsDummies.fit), collapse='') ,')'),
            series = '1hrs ph2',
            transformation = 'identity()',
            traindays = 4,
            testdays = 3,
            xreg = paste0(deparse(bestObsDummies.fcast), collapse=''))

report.full(model = 'Arima(order=c(1, 0, 0),method="CSS", xreg=fourier(., K=3))',
            series = '1hrs ph2',
            transformation = 'identity()',
            traindays = 4,
            testdays = 3,
            xreg = 'fourier(., h=h, K=3)')

# dummies on 6th day - 6th day has an "outlier" ----

sixthDD.fcast <- quote(
  {cbind(
    dummies=get6thDayDummies(h, frequency(.), start(.)[[1]]),
    fourier(., h=h, K=3)
  )}
)

sixthDD.fit <- quote(
  {cbind(
    dummies=get6thDayDummies(length(.), frequency(.), start(.)[[1]]),
    fourier(., K=3)
  )}
)

#7:3 rmse=575 mae=214
#non-finite value supplied by optim for 4 days of training  - because we talk about weeks but do not train on one
report.full(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(sixthDD.fit), collapse='') ,')'),
            series = '1hrs ph2',
            transformation = 'identity()',
            traindays = 7,
            testdays = 3,
            xreg = paste0(deparse(sixthDD.fcast), collapse=''))

# dummies on every weekday ----

dailyD.fcast <- quote(
  {cbind(
    dummies=getDailyDummies(h, frequency(.), start(.)[[1]]),
    fourier(., h=h, K=3)
  )}
)

dailyD.fit <- quote(
  {cbind(
    dummies=getDailyDummies(length(.), frequency(.), start(.)[[1]]),
    fourier(., K=3)
  )}
)

# 7:3 rmse=346, mae=169
#non-finite value supplied by optim"
report.full(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(dailyD.fit), collapse='') ,')'),
            series = '1hrs ph2',
            transformation = 'identity()',
            traindays = 7,
            testdays = 3,
            xreg = paste0(deparse(dailyD.fcast), collapse=''))

# dummies on 7-15th+1-7 obs (the "outlier") ----
best.fcast.dummy.1hrsPh2 <- NULL
best.startDummy <- 0
best.lenDummy <- 0

for(startDummy in 7:15)
{
  for(lenDummy in 1:7)
  {
    print(paste("Trying startDummy =", startDummy, ", length =", lenDummy))
    
    obsDummies.fcast <- substitute(
      {cbind(
        dummies=getNthObsDummies(startDummy, lenDummy, h, frequency(.)),
        fourier(., h=h, K=3)
      )},
      list(startDummy=startDummy, lenDummy=lenDummy)
    )
    
    obsDummies.fit <- substitute(
      {cbind(
        dummies=getNthObsDummies(startDummy, lenDummy, length(.), frequency(.)),
        fourier(., K=3)
      )},
      list(startDummy=startDummy, lenDummy=lenDummy)
    )
    
    current <- fullforecast(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(obsDummies.fit), collapse='') ,')'),
                            dataset = datasets[['1hrs ph2']]$series,
                            transformation = 'identity()',
                            traindays = 4,
                            testdays = 3,
                            xreg = paste0(deparse(obsDummies.fcast), collapse=''))
    
    if(is.null(best.fcast.dummy.1hrsPh2) || current$accuracy[[2]] < best.fcast.dummy.1hrsPh2$accuracy[[2]])
    {
      best.fcast.dummy.1hrsPh2 <- current
      best.startDummy <- startDummy
      best.lenDummy <- lenDummy
    }
    
  }
}

bestObsDummies.fcast <- substitute(
  {cbind(
    dummies=getNthObsDummies(best.startDummy, best.lenDummy, h, frequency(.)),
    fourier(., h=h, K=3)
  )},
  list(best.startDummy = best.startDummy, best.lenDummy = best.lenDummy)
)

bestObsDummies.fit <- substitute(
  {cbind(
    dummies=getNthObsDummies(best.startDummy, best.lenDummy, length(.), frequency(.)),
    fourier(., K=3)
  )},
  list(best.startDummy = best.startDummy, best.lenDummy = best.lenDummy)
)

# 4:3, dummies: 8:7, rmse=346, mae=164
report.full(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(bestObsDummies.fit), collapse='') ,')'),
            series = '1hrs ph2',
            transformation = 'identity()',
            traindays = 4,
            testdays = 3,
            xreg = paste0(deparse(bestObsDummies.fcast), collapse=''))

# 4:3, dummies: 8:3, rmse=347, mae=162
report.full(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(bestObsDummies.fit), collapse='') ,')'),
            series = '1hrs ph2',
            transformation = 'identity()',
            traindays = 4,
            testdays = 3,
            xreg = paste0(deparse(bestObsDummies.fcast), collapse=''))
