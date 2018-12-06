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

report(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), xreg=fourier(., K=2))',
       series = '1hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3,
       xreg = 'fourier(., K=2)')


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
            testdays = best.fourier.testdays, # 2
            xreg='fourier(., K=4, h=h)')

# find the best k for 7:2, ARIMA(1,0,0)(1,0,0) with fourier ----

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
                          testdays = best.fourier.testdays, # 2
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
            testdays = best.fourier.testdays, # 2
            xreg = paste('fourier(., h=h, K=', best.k, ')')) # 2

#  best for 7:2, ARIMA(1,0,0)(1,0,0) with fourier k=2, RMSE=311, MAE=179  ----
# with tsclean RMSE= 320, MAE=168 
# For observation based 168:2, same model, same k, RMSE=317, MAE =186
# dummies: 9:2, rmse=308, mae=174
# dummies: 9:2, no SAR term, rmse=308, mae=172
report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=fourier(., K=2))',
            series = '1hrs ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 2,
            xreg = 'fourier(., h=h, K=2)')

report.full(model = paste('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=fourier(., K=', 2, '))', sep=''),
            series = '1hrs ph3',
            transformation = 'tsclean()',
            traindays = 7,
            testdays = 2,
            xreg = paste('fourier(., h=h, K=', 2, ')'))

obsDummies.fcast <- quote(
  {cbind(
    dummies=getNthObsDummies(9, 2, h, frequency(.)),
    fourier(., h=h, K=2)
  )}
)

obsDummies.fit <- quote(
  {cbind(
    dummies=getNthObsDummies(9, 2, length(.), frequency(.)),
    fourier(., K=2)
  )}
)

report.full(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(obsDummies.fit), collapse='') ,')'),
            series = '1hrs ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 2,
            xreg = paste0(deparse(obsDummies.fcast), collapse=''))


report.full(model = paste0('Arima(order=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(obsDummies.fit), collapse='') ,')'),
            series = '1hrs ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 2,
            xreg = paste0(deparse(obsDummies.fcast), collapse=''))

#same
report.full(model = 'Arima(order=c(1, 0, 0), method="CSS", xreg=fourier(., K=2))',
            series = '1hrs ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 2,
            xreg = 'fourier(., h=h, K=2)')

# observation based modelling ----
best.fcast.obs.1hrsPh3 <- NULL
best.testobs <- 0
trainobs <- 7*frequency(datasets[['1hrs ph3']]$series) #168

for(testobs in 1:(frequency(datasets[['1hrs ph3']]$series) - 1)) # 1:23
{
  print(paste("Trying", trainobs, "train observations and", testobs, "test observations"))
  current <- fullforecast.obs(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS")',
                              dataset = datasets[['1hrs ph3']]$series,
                              transformation = 'identity()',
                              trainobs = trainobs,
                              testobs = testobs,
                              xreg=NULL)
  
  if(is.null(best.fcast.obs.1hrsPh3) || current$accuracy[[2]] < best.fcast.obs.1hrsPh3$accuracy[[2]])
  {
    best.fcast.obs.1hrsPh3 <- current
    best.testobs <- testobs
  }
}

report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS")',
            series = '1hrs ph3',
            transformation = 'identity()',
            traindays = trainobs,
            testdays = best.testobs, # 19
            obs = TRUE)


best.fcast.obs.fourier.1hrsPh3 <- NULL
best.fourier.testobs <- 0

for(testobs in 2:(frequency(datasets[['1hrs ph3']]$series) - 1)) # 2:23
{
  print(paste("Trying", trainobs, "train observations and", testobs, "test observations"))
  current <- fullforecast.obs(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), xreg=fourier(., K=2), method="CSS")',
                              dataset = datasets[['1hrs ph3']]$series,
                              transformation = 'identity()',
                              trainobs = trainobs,
                              testobs = testobs,
                              xreg = 'fourier(., h=h, K=2)')
  
  if(is.null(best.fcast.obs.fourier.1hrsPh3) || current$accuracy[[2]] < best.fcast.obs.fourier.1hrsPh3$accuracy[[2]])
  {
    best.fcast.obs.fourier.1hrsPh3 <- current
    best.fourier.testobs <- testobs
  }
}

report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), xreg=fourier(., K=2), method="CSS")',
            series = '1hrs ph3',
            transformation = 'identity()',
            traindays = trainobs,
            testdays = best.fourier.testobs, # 2
            xreg = 'fourier(., h=h, K=2)',
            obs = TRUE)


# tsclean on best ----
report.full(model = paste('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=fourier(., K=', 2, '))', sep=''),
            series = '1hrs ph3',
            transformation = 'tsclean()',
            traindays = 7,
            testdays = 2,
            xreg = paste('fourier(., h=h, K=', 2, ')'))


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

report(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(sixthDD.fit), collapse='') ,')'),
       series = '1hrs ph3',
       transformation = 'identity()',
       traindays = 7,
       testdays = 2,
       xreg = paste0(deparse(sixthDD.fcast), collapse=''))

#7:2 rmse=375 mae=214
report.full(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(sixthDD.fit), collapse='') ,')'),
            series = '1hrs ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 2,
            xreg = paste0(deparse(sixthDD.fcast), collapse=''))


(get_days(datasets[['1hrs ph3']]$series, 0, 2, NULL)$train %>% 
{eval(getNthObsDummies(8, 4, length(.), frequency(.)))} ->
    external.regressors)
#plot(external.regressors)
autoplot(get_days(datasets[['1hrs ph3']]$series, 0, 2, NULL)$train) +
  autolayer(ts(external.regressors*1000, frequency=24, start=141-141))
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
       series = '1hrs ph3',
       transformation = 'identity()',
       traindays = 7,
       testdays = 2,
       xreg = paste0(deparse(dailyD.fcast), collapse=''))

# 7:2 rmse=322, mae=195
report.full(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(dailyD.fit), collapse='') ,')'),
            series = '1hrs ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 2,
            xreg = paste0(deparse(dailyD.fcast), collapse=''))

# dummies on 6-13th+1-5 obs (the "outlier") ----
best.fcast.dummy.1hrsPh3 <- NULL
best.startDummy <- 0
best.lenDummy <- 0

for(startDummy in 6:13)
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
                            dataset = datasets[['1hrs ph3']]$series,
                            transformation = 'identity()',
                            traindays = 7,
                            testdays = 2,
                            xreg = paste0(deparse(obsDummies.fcast), collapse=''))
    
    if(is.null(best.fcast.dummy.1hrsPh3) || current$accuracy[[2]] < best.fcast.dummy.1hrsPh3$accuracy[[2]])
    {
      best.fcast.dummy.1hrsPh3 <- current
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

# 7:2, dummies: 9:2, rmse=308, mae=174
report.full(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(bestObsDummies.fit), collapse='') ,')'),
            series = '1hrs ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 2,
            xreg = paste0(deparse(bestObsDummies.fcast), collapse=''))

# other tries ----

report.full(model = 'Arima(order=c(4, 1, 1), seasonal=c(2, 0, 0), method="CSS")',
            series = '1hrs ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=fourier(., K=2), include.mean=FALSE)',
            series = '1hrs ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1,
            xreg = 'fourier(., h=h, K=2)')

