library(rmarkdown)
library(fs)

source('funcs.R')

# benchmark models  ----

report.full(model = 'snaive()',
            series = '1hrs ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

report.full(model = 'meanf()',
            series = '1hrs ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

report.full(model = 'naive()',
            series = '1hrs ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

# 7:3 Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), xreg=fourier(., K=4)) best RMSE  ----

#sinusoidal lags in ACF & 1 positive in PACF => AR signature
report(model = 'Arima(order=c(1, 0, 0))',
       series = '1hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3)

# we had seasonal lags, so we add seasonal component
report(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0))',
       series = '1hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3)

# we still have some significant lags early and around the seasonal period
report(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), xreg=fourier(., K=4))',
       series = '1hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3,
       xreg = 'fourier(., K=4)')

# 7:1 ----

report(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), xreg=fourier(., K=4))',
       series = '1hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 1,
       xreg = 'fourier(., K=4)')


# full 7:1 ----

#RMSE 313
report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), xreg=fourier(., K=4), method="CSS")',
       series = '1hrs ph3',
       transformation = 'identity()',
       traindays = 7,
       testdays = 1,
       xreg = 'fourier(., K=4, h=h)')

report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS")',
            series = '1hrs ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

report.full(model = 'auto.arima()',
            series = '1hrs ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

# Error in optim(init[mask], armafn, method = optim.method, hessian = TRUE,  : 
# non-finite finite-difference value [1] 
fullforecast.serial(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="ML")',
            dataset = datasets[['1hrs ph3']]$series,
            transformation = 'identity()',
            traindays = 7,
            testdays = 1,
            xreg = NULL)

#Error in optim(init[mask], armafn, method = optim.method, hessian = TRUE,  : 
#non-finite finite-difference value [1] 
fullforecast.serial(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0))',
                    dataset = datasets[['1hrs ph3']]$series,
                    transformation = 'identity()',
                    traindays = 7,
                    testdays = 1,
                    xreg = NULL)

# Error in optim(init[mask], armafn, method = optim.method, hessian = TRUE,  : 
# function cannot be evaluated at initial parameters 
fullforecast.serial(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), optim.method="Nelder-Mead")',
                    dataset = datasets[['1hrs ph3']]$series,
                    transformation = 'identity()',
                    traindays = 7,
                    testdays = 1,
                    xreg = NULL)

# Error in optim(init[mask], armafn, method = optim.method, hessian = TRUE,  : 
# non-finite finite-difference value [1]
fullforecast.serial(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS-ML")',
                    dataset = datasets[['1hrs ph3']]$series,
                    transformation = 'identity()',
                    traindays = 7,
                    testdays = 1,
                    xreg = NULL)


#RMSE = 323
fullforecast.serial(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS")',
                    dataset = datasets[['1hrs ph3']]$series,
                    transformation = 'identity()',
                    traindays = 7,
                    testdays = 1,
                    xreg = NULL)

# Find the best train:test days ratio and K for the above ARIMA(1,0,0)(1,0,0) with fourier, K=4 ----

best.fcast.fourier.1hrsPh3 <- NULL
best.fourier.traindays <- 0
best.fourier.testdays <- 0

for(traindays in 3:7)
{
  for(testdays in 1:3)
  {
    print(paste("Trying", traindays, "train days and", testdays, "test days"))
    current <- fullforecast(
      model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), xreg=fourier(., K=4), method="CSS")',
      dataset = datasets[['1hrs ph3']]$series,
      transformation = 'identity()',
      traindays = traindays,
      testdays = testdays,
      xreg = 'fourier(., K=4, h=h)')
    
    if(is.null(best.fcast.fourier.1hrsPh3) || current$accuracy[[2]] < best.fcast.fourier.1hrsPh3$accuracy[[2]])
    {
      best.fcast.fourier.1hrsPh3 <- current
      best.fourier.traindays <- traindays
      best.fourier.testdays <- testdays
    }
    
  }
}

report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), xreg=fourier(., K=4), method="CSS")',
            series = '1hrs ph3',
            transformation = 'identity()',
            traindays = best.fourier.traindays, # 7
            testdays = best.fourier.testdays, # 1
            xreg='fourier(., K=4, h=h)')

# find the best k for 7:1, ARIMA(1,0,0)(1,0,0) with fourier ----

best.fcast.k.1hrsPh3 <- NULL
best.k <- 0
#K must be not be greater than period/2
for(k in 1:(frequency(datasets[['1hrs ph3']]$series)/2))
{
  print(paste("Trying k =", k))
  m <- paste('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=fourier(., K=', k, '))', sep='')
  xreg <- paste('fourier(., h=h, K=', k, ')')
  current <- fullforecast(model = m,
                          dataset = datasets[['1hrs ph3']]$series,
                          transformation = 'identity()',
                          traindays = best.fourier.traindays, # 7
                          testdays = best.fourier.testdays, # 1
                          xreg=xreg)
  
  if(is.null(best.fcast.k.1hrsPh3) || current$accuracy[[2]] < best.fcast.k.1hrsPh3$accuracy[[2]])
  {
    best.fcast.k.1hrsPh3 <- current
    best.k <- k
  }
}

report.full(model = paste('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=fourier(., K=', best.k, '))', sep=''),
            series = '1hrs ph3',
            transformation = 'identity()',
            traindays = best.fourier.traindays, # 7
            testdays = best.fourier.testdays, # 1
            xreg = paste('fourier(., h=h, K=', best.k, ')'))

#  best for 7:1, ARIMA(1,0,0)(1,0,0) with fourier k=2 ----