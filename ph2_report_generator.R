source('funcs.R')

# benchmark models  ----
report.full(model = 'snaive()',
            series = 'ph2',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

report.full(model = 'meanf()',
            series = 'ph2',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

report.full(model = 'naive()',
            series = 'ph2',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

# 7:3 full ----


#simple
report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS")',
            series = 'ph2',
            transformation = 'identity()',
            traindays = 7,
            testdays = 3,
            serial = TRUE)

# inspiration from 1hrs series
report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=fourier(., K=3))',
            series = 'ph2',
            transformation = 'identity()',
            traindays = 4,
            testdays = 3,
            xreg = 'fourier(., h=h, K=3)',
            serial = TRUE)

# inspiration from 2hrs series
report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=fourier(., K=1))',
            series = 'ph2',
            transformation = 'identity()',
            traindays = 7,
            testdays = 3,
            xreg = 'fourier(., h=h, K=1)',
            serial = TRUE)

# with 8th hour up to 11th dummies - inspiration from 2hrs series combined with the ratio from the 1hrs
# K=3 - more complex seasonality on denser data - from 1hrs
obsDummies.fcast <- quote(
  {cbind(
    dummies=getNthObsDummies(8*frequency(.)+1, 3*frequency(.), length(.), frequency(.)),
    fourier(., h=h, K=3)
  )}
)

obsDummies.fit <- quote(
  {cbind(
    dummies=getNthObsDummies(8*frequency(.)+1, 3*frequency(.), length(.), frequency(.)),
    fourier(., K=3)
  )}
)

report.full(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(obsDummies.fit), collapse='') ,')'),
            series = 'ph2',
            transformation = 'identity()',
            traindays = 4,
            testdays = 3,
            xreg = paste0(deparse(obsDummies.fcast), collapse=''),
            serial = TRUE)

#with 6th day dummies - inspiration from 1hrs series 
#with 7:3 since we're talking about 6th day dummies hence we need a training week

sixthDD.fcast <- quote(
  {cbind(
    dummies=get6thDayDummies(h, frequency(.)),
    fourier(., h=h, K=3)
  )}
)

sixthDD.fit <- quote(
  {cbind(
    dummies=get6thDayDummies(length(.), frequency(.)),
    fourier(., K=3)
  )}
)

report.full(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(sixthDD.fit), collapse='') ,')'),
            series = 'ph2',
            transformation = 'identity()',
            traindays = 7,
            testdays = 3,
            xreg = paste0(deparse(sixthDD.fcast), collapse=''),
            serial = TRUE)
