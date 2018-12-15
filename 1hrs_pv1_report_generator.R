source('funcs.R')

# benchmark models  ----

report.full(model = 'snaive()',
            series = '1hrs pv1',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

report.full(model = 'meanf()',
            series = '1hrs pv1',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

report.full(model = 'naive()',
            series = '1hrs pv1',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

# try to find the best ARIMA model  ----
report(model = 'Arima(order=c(1, 0, 0))',
       series = '1hrs pv1',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = 0,
       traindays = 7,
       testdays = 3)

report(model = 'Arima(order=c(2, 0, 0))',
       series = '1hrs pv1',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = 0,
       traindays = 7,
       testdays = 3)

report(model = 'Arima(order=c(3, 0, 0))',
       series = '1hrs pv1',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = 0,
       traindays = 7,
       testdays = 3)

report(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0))',
       series = '1hrs pv1',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = 0,
       traindays = 7,
       testdays = 3)

# add fourier terms
report(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), xreg=fourier(., K=2))',
       series = '1hrs pv1',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = 0,
       traindays = 7,
       testdays = 3,
       xreg='fourier(., K=2, h=h)')

#  Find the best train:test days ratio for ARIMA(1,0,0)(1,0,0) ----
scaler <- 1/100000000000
best.fcast.1hrsPv1 <- NULL
best.traindays <- 0
best.testdays <- 0

for(traindays in 3:7)
{
  for(testdays in 2:3)
  {
    print(paste("Trying", traindays, "train days and", testdays, "test days"))
    current <- fullforecast(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS")',
                            dataset = datasets[['1hrs pv1']]$series * scaler,
                            transformation = 'identity()',
                            traindays = traindays,
                            testdays = testdays,
                            xreg=NULL)
    
    if(is.null(best.fcast.1hrsPv1) || current$accuracy[[2]] < best.fcast.1hrsPv1$accuracy[[2]])
    {
      best.fcast.1hrsPv1 <- current
      best.traindays <- traindays
      best.testdays <- testdays
    }
  }
}

report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="ML")',
            series = '1hrs pv1',
            transformation = 'identity()',
            traindays = best.traindays, # 5
            testdays = best.testdays) # 2

# Skip over the step where I hardcode a fourier value ----
# Find best K for the above model ARIMA(1,0,0)(1,0,0) ----

best.fcast.k.1hrsPv1 <- NULL
best.k <- 0
#K must be not be greater than period/2
for(k in 1:(frequency(datasets[['1hrs pv1']]$series)/2))
{
  print(paste("Trying k =", k))
  m <- paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=fourier(., K=', k, '))')
  xreg <- paste0('fourier(., h=h, K=', k, ')')
  current <- fullforecast(model = m,
                          dataset = datasets[['1hrs pv1']]$series * scaler,
                          transformation = 'identity()',
                          traindays = best.traindays, # ?
                          testdays = best.testdays, # ?
                          xreg=xreg)
  
  if(is.null(best.fcast.k.1hrsPv1) || current$accuracy[[2]] < best.fcast.k.1hrsPv1$accuracy[[2]])
  {
    best.fcast.k.1hrsPv1 <- current
    best.k <- k
  }
}

report.full(model = paste('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=fourier(., K=', best.k, '))', sep=''),
            series = '1hrs pv1',
            transformation = 'identity()',
            traindays = best.traindays, # 5
            testdays = best.testdays, # 2
            xreg = paste('fourier(., h=h, K=', best.k, ')')) # 2

# Best model: 5:2, ARIMA(1, 0, 0)(1, 0, 0), K=2, RMSE=460 MAE=232  ----
# 11th-17th obs dummies rmse=461, mae=230
# without seasonal part, only fourier: rmse=460, mae=234
# without seasonal part, 2AR, only fourier: rmse=460, mae=234
# without seasonal part, 3AR, only fourier: rmse=460, mae=234
# dummies without seasonal part, only fourier: rmse=460, mae=237
# dummies without seasonal part, 2AR, only fourier: rmse=460, mae=237
# dummies without seasonal part, 3AR, only fourier: rmse=460, mae=237
report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=fourier(., K=2))',
            series = '1hrs pv1',
            transformation = 'identity()',
            traindays = 5,
            testdays = 2,
            xreg = 'fourier(., h=h, K=2)')

dummies.fcast <- quote(
  {cbind(
    dummies=getNthObsDummies(11, 6, h, frequency(.)),
    fourier(., h=h, K=2)
  )}
)

dummies.fit <- quote(
  {cbind(
    dummies=getNthObsDummies(11, 6, length(.), frequency(.)),
    fourier(., K=2)
  )}
)

report.full(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(dummies.fit), collapse='') ,')'),
            series = '1hrs pv1',
            transformation = 'identity()',
            traindays = 5,
            testdays = 2,
            xreg = paste0(deparse(dummies.fcast), collapse=''))

# NO SAR, with dummies 
# rmse=460, mae=234
report.full(model = paste0('Arima(order=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(dummies.fit), collapse='') ,')'),
            series = '1hrs pv1',
            transformation = 'identity()',
            traindays = 5,
            testdays = 2,
            xreg = paste0(deparse(dummies.fcast), collapse=''))

# no SAR with dummies
# 2 AR terms
# rmse=460, mae=234
report.full(model = paste0('Arima(order=c(2, 0, 0), method="CSS", xreg=', paste0(deparse(dummies.fit), collapse='') ,')'),
            series = '1hrs pv1',
            transformation = 'identity()',
            traindays = 5,
            testdays = 2,
            xreg = paste0(deparse(dummies.fcast), collapse=''))

# no SAR with dummies
# 3 AR terms
# rmse=, mae=
report.full(model = paste0('Arima(order=c(3, 0, 0), method="CSS", xreg=', paste0(deparse(dummies.fit), collapse='') ,')'),
            series = '1hrs pv1',
            transformation = 'identity()',
            traindays = 5,
            testdays = 2,
            xreg = paste0(deparse(dummies.fcast), collapse=''))

# no SAR, no dummies
# rmse=460, mae=237
report.full(model = 'Arima(order=c(1, 0, 0), method="CSS", xreg=fourier(., K=2))',
            series = '1hrs pv1',
            transformation = 'identity()',
            traindays = 5,
            testdays = 2,
            xreg = 'fourier(., h=h, K=2)')

# no SAR, no dummies, 2AR
# rmse=460, mae=237
report.full(model = 'Arima(order=c(2, 0, 0), method="CSS", xreg=fourier(., K=2))',
            series = '1hrs pv1',
            transformation = 'identity()',
            traindays = 5,
            testdays = 2,
            xreg = 'fourier(., h=h, K=2)')

# no SAR, no dummies, 3 AR terms
# rmse=460, mae=237
report.full(model = 'Arima(order=c(3, 0, 0), method="CSS", xreg=fourier(., K=2))',
            series = '1hrs pv1',
            transformation = 'identity()',
            traindays = 5,
            testdays = 2,
            xreg = 'fourier(., h=h, K=2)')


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

# 5:2 rmse=?, mae=?
report.full(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(dailyD.fit), collapse='') ,')'),
            series = '1hrs pv1',
            transformation = 'identity()',
            traindays = 5,
            testdays = 2,
            xreg = paste0(deparse(dailyD.fcast), collapse=''))

# dummies on the ?-?th+?-?th obs (the "outliers") ----

best.fcast.dummy.1hrsPv1 <- NULL
best.startDummy <- 0
best.lenDummy <- 0

for(startDummy in 8:11)
{
  for(lenDummy in 5:9)
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
                            dataset = datasets[['1hrs pv1']]$series,
                            transformation = 'identity()',
                            traindays = 5,
                            testdays = 2,
                            xreg = paste0(deparse(obsDummies.fcast), collapse=''))
    
    if(is.null(best.fcast.dummy.1hrsPv1) || current$accuracy[[2]] < best.fcast.dummy.1hrsPv1$accuracy[[2]])
    {
      best.fcast.dummy.1hrsPv1 <- current
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

# 5:2, dummies: 11:6, rmse=461, mae=230
report.full(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(bestObsDummies.fit), collapse='') ,')'),
            series = '1hrs pv1',
            transformation = 'identity()',
            traindays = 5,
            testdays = 2,
            xreg = paste0(deparse(bestObsDummies.fcast), collapse=''))
