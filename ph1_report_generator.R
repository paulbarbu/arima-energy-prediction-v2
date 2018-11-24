source('funcs.R')

# benchmark models  ----
report.full(model = 'snaive()',
            series = 'ph1',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

report.full(model = 'meanf()',
            series = 'ph1',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

report.full(model = 'naive()',
            series = 'ph1',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

# 7:2 full ----

#simple
report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS")',
            series = 'ph1',
            transformation = 'identity()',
            traindays = 7,
            testdays = 2,
            serial = TRUE)

# fourier terms, inspiration from both of the series
report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=fourier(., K=2))',
            series = 'ph1',
            transformation = 'identity()',
            traindays = 7,
            testdays = 2,
            xreg = 'fourier(., h=h, K=2)',
            serial=TRUE)

#dummies, inspiration from 2hrs
# ATTENTION: I converted from "dummies" in the `2hrs ph1 series`` to "hours", this means, I multipled by 2 to get the hourly intervals right
# with 11th hour up to 18th hour dummies - inspiration from 2hrs series - not 12th since I want to "catch it in full"
obsDummies.fcast <- quote(
  {cbind(
    dummies=getNthObsDummies(11*(frequency(.)/24)+1, 7*(frequency(.)/24), length(.), frequency(.)),
    fourier(., h=h, K=2)
  )}
)

obsDummies.fit <- quote(
  {cbind(
    dummies=getNthObsDummies(11*(frequency(.)/24)+1, 7*(frequency(.)/24), length(.), frequency(.)),
    fourier(., K=2)
  )}
)

report.full(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(obsDummies.fit), collapse='') ,')'),
            series = 'ph1',
            transformation = 'identity()',
            traindays = 7,
            testdays = 2,
            xreg = paste0(deparse(obsDummies.fcast), collapse=''),
            serial = TRUE)

#dummies, inspiration from 1hrs
obsDummies.fcast <- quote(
  {cbind(
    dummies=getNthObsDummies(11*(frequency(.)/24)+1, 1*(frequency(.)/24), length(.), frequency(.)),
    fourier(., h=h, K=2)
  )}
)

obsDummies.fit <- quote(
  {cbind(
    dummies=getNthObsDummies(11*(frequency(.)/24)+1, 1*(frequency(.)/24), length(.), frequency(.)),
    fourier(., K=2)
  )}
)

report.full(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(obsDummies.fit), collapse='') ,')'),
            series = 'ph1',
            transformation = 'identity()',
            traindays = 7,
            testdays = 2,
            xreg = paste0(deparse(obsDummies.fcast), collapse=''),
            serial = TRUE)