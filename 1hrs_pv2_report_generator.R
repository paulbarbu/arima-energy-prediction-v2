source('funcs.R')

# benchmark models  ----

report.full(model = 'snaive()',
            series = '1hrs pv2',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

report.full(model = 'meanf()',
            series = '1hrs pv2',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

report.full(model = 'naive()',
            series = '1hrs pv2',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

# try to find the best ARIMA model  ----
report(model = 'Arima(order=c(1, 0, 0))',
       series = '1hrs pv2',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = 0,
       traindays = 7,
       testdays = 3)

#try with one MA as well since I have one significant 1st positive lag and 2nd significant negative lag
report(model = 'Arima(order=c(1, 0, 1))',
       series = '1hrs pv2',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = 0,
       traindays = 7,
       testdays = 3)

# sinusoidal ACF and 2 lag in PACF from the beginning of the dataset, but also lambda= 0.2654112
report(model = 'Arima(order=c(2, 0, 0), lambda=0.2654112)',
       series = '1hrs pv2',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = 0,
       traindays = 7,
       testdays = 3)

# sinusoidal ACF and 2 lag in PACF from the beginning of the dataset
report(model = 'Arima(order=c(2, 0, 0))',
       series = '1hrs pv2',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = 0,
       traindays = 7,
       testdays = 3)

# significant term in PACF, so add 1 SAR term
report(model = 'Arima(order=c(2, 0, 0), seasonal=c(1, 0, 0))',
       series = '1hrs pv2',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = 0,
       traindays = 7,
       testdays = 3)

# try to reduce the AR term, indeed, better results
report(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0))',
       series = '1hrs pv2',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = 0,
       traindays = 7,
       testdays = 3)

# add fourier terms
report(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), xreg=fourier(., K=5))',
       series = '1hrs pv2',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = 0,
       traindays = 7,
       testdays = 3,
       xreg='fourier(., K=5, h=h)')

#  Find the best train:test days ratio for ARIMA(1,0,0)(1,0,0) ----

best.fcast.1hrsPv2 <- NULL
best.traindays <- 0
best.testdays <- 0

for(traindays in 3:7)
{
  for(testdays in 2:3)
  {
    print(paste("Trying", traindays, "train days and", testdays, "test days"))
    current <- fullforecast(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS")',
                            dataset = datasets[['1hrs pv2']]$series,
                            transformation = 'identity()',
                            traindays = traindays,
                            testdays = testdays,
                            xreg=NULL)
    
    if(is.null(best.fcast.1hrsPv2) || current$accuracy[[2]] < best.fcast.1hrsPv2$accuracy[[2]])
    {
      best.fcast.1hrsPv2 <- current
      best.traindays <- traindays
      best.testdays <- testdays
    }
  }
}

report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="ML")',
            series = '1hrs pv2',
            transformation = 'identity()',
            traindays = best.traindays, # ?
            testdays = best.testdays) # ?

# Skip over the step where I hardcode a fourier value ----
# Find best K for the above model ARIMA(1,0,0)(1,0,0) ----

best.fcast.k.1hrsPv2 <- NULL
best.k <- 0
#K must be not be greater than period/2
for(k in 1:(frequency(datasets[['1hrs pv2']]$series)/2))
{
  print(paste("Trying k =", k))
  m <- paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=fourier(., K=', k, '))')
  xreg <- paste0('fourier(., h=h, K=', k, ')')
  current <- fullforecast(model = m,
                          dataset = datasets[['1hrs pv2']]$series,
                          transformation = 'identity()',
                          traindays = best.traindays, # ?
                          testdays = best.testdays, # ?
                          xreg=xreg)
  
  if(is.null(best.fcast.k.1hrsPv2) || current$accuracy[[2]] < best.fcast.k.1hrsPv2$accuracy[[2]])
  {
    best.fcast.k.1hrsPv2 <- current
    best.k <- k
  }
}

report.full(model = paste('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=fourier(., K=', best.k, '))', sep=''),
            series = '1hrs pv2',
            transformation = 'identity()',
            traindays = best.traindays, # ?
            testdays = best.testdays, # ?
            xreg = paste('fourier(., h=h, K=', best.k, ')')) # ?

