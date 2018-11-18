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
# 5th hour dummies RMSE=376  MAE=183
report.full(output_format = 'pdf_document',
            model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=fourier(., K=1))',
            series = '2hrs ph2',
            transformation = 'identity()',
            traindays = 7,
            testdays = 3,
            xreg = 'fourier(., h=h, K=1)')

fifthHD.fcast <- quote(
  {cbind(
    dummies=get5thHourDummies(h, frequency(.)),
    fourier(., h=h, K=1)
  )}
)

fifthHD.fit <- quote(
  {cbind(
    dummies=get5thHourDummies(length(.), frequency(.)),
    fourier(., K=1)
  )}
)

report.full(#output_format = 'pdf_document',
  model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(fifthHD.fit), collapse='') ,')'),
  series = '2hrs ph2',
  transformation = 'identity()',
  traindays = 7,
  testdays = 3,
  xreg = paste0(deparse(fifthHD.fcast), collapse=''))

# dummies on 6th day - 6th day has an "outlier" ----

sixthDD.fcast <- quote(
  {cbind(
    dummies=get6thDayDummies(h, frequency(.)),
    fourier(., h=h, K=1)
  )}
)

sixthDD.fit <- quote(
  {cbind(
    dummies=get6thDayDummies(length(.), frequency(.)),
    fourier(., K=1)
  )}
)

#7:3 rmse=374 mae=184
report.full(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(sixthDD.fit), collapse='') ,')'),
            series = '2hrs ph2',
            transformation = 'identity()',
            traindays = 7,
            testdays = 3,
            xreg = paste0(deparse(sixthDD.fcast), collapse=''))

# dummies on every weekday ----

dailyD.fcast <- quote(
  {cbind(
    dummies=getDailyDummies(h, frequency(.)),
    fourier(., h=h, K=1)
  )}
)

dailyD.fit <- quote(
  {cbind(
    dummies=getDailyDummies(length(.), frequency(.)),
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
# dummies on 5th hour (the "outlier") ----

fifthHD.fcast <- quote(
  {cbind(
    dummies=get5thHourDummies(h, frequency(.)),
    fourier(., h=h, K=1)
  )}
)

fifthHD.fit <- quote(
  {cbind(
    dummies=get5thHourDummies(length(.), frequency(.)),
    fourier(., K=1)
  )}
)

# 7:3 rmse=376, mae=183
report.full(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(fifthHD.fit), collapse='') ,')'),
            series = '2hrs ph2',
            transformation = 'identity()',
            traindays = 7,
            testdays = 3,
            xreg = paste0(deparse(fifthHD.fcast), collapse=''))


