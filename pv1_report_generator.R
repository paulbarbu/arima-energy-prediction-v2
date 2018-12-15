source('funcs.R')


# benchmark models  ----
report.full(model = 'snaive()',
            series = 'pv1',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

report.full(model = 'meanf()',
            series = 'pv1',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

report.full(model = 'naive()',
            series = 'pv1',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

# 5:2 full ----

#simple
report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS")',
            series = 'pv1',
            transformation = 'identity()',
            traindays = 5,
            testdays = 2,
            serial = TRUE)

# fourier terms, inspiration from both series, K=2
report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=fourier(., K=2))',
            series = 'pv1',
            transformation = 'identity()',
            traindays = 5,
            testdays = 2,
            xreg = 'fourier(., h=h, K=2)',
            serial=TRUE)

# fourier terms without SAR
report.full(model = 'Arima(order=c(1, 0, 0), method="CSS", xreg=fourier(., K=2))',
            series = 'pv1',
            transformation = 'identity()',
            traindays = 5,
            testdays = 2,
            xreg = 'fourier(., h=h, K=2)',
            serial=TRUE)

# dummies, inspiration from 2hrs, including K=2
# ATTENTION: I converted from "dummies" in the `2hrs pv1 series`` to "hours", this means, I multipled by 2 to get the hourly intervals right
# with 8th hour up to 16th hour dummies - inspiration from 2hrs series
obsDummies.fcast <- quote(
  {cbind(
    dummies=getNthObsDummies(16*(frequency(.)/24)+1, 4*(frequency(.)/24), length(.), frequency(.)),
    fourier(., h=h, K=2)
  )}
)

obsDummies.fit <- quote(
  {cbind(
    dummies=getNthObsDummies(16*(frequency(.)/24)+1, 4*(frequency(.)/24), length(.), frequency(.)),
    fourier(., K=2)
  )}
)

report.full(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(obsDummies.fit), collapse='') ,')'),
            series = 'pv1',
            transformation = 'identity()',
            traindays = 5,
            testdays = 2,
            xreg = paste0(deparse(obsDummies.fcast), collapse=''),
            serial = TRUE)

#dummies, inspiration from 1hrs
obsDummies.fcast <- quote(
  {cbind(
    dummies=getNthObsDummies(11*(frequency(.)/24)+1, 6*(frequency(.)/24), length(.), frequency(.)),
    fourier(., h=h, K=3)
  )}
)

obsDummies.fit <- quote(
  {cbind(
    dummies=getNthObsDummies(11*(frequency(.)/24)+1, 6*(frequency(.)/24), length(.), frequency(.)),
    fourier(., K=3)
  )}
)

report.full(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(obsDummies.fit), collapse='') ,')'),
            series = 'pv1',
            transformation = 'identity()',
            traindays = 5,
            testdays = 2,
            xreg = paste0(deparse(obsDummies.fcast), collapse=''),
            serial = TRUE)

#VERY FAST because I removed the SAR term for the two cases above

# 16th hour to 20th hour, K=2
obsDummies.fcast <- quote(
  {cbind(
    dummies=getNthObsDummies(16*(frequency(.)/24)+1, 4*(frequency(.)/24), length(.), frequency(.)),
    fourier(., h=h, K=2)
  )}
)

obsDummies.fit <- quote(
  {cbind(
    dummies=getNthObsDummies(16*(frequency(.)/24)+1, 4*(frequency(.)/24), length(.), frequency(.)),
    fourier(., K=2)
  )}
)

report.full(model = paste0('Arima(order=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(obsDummies.fit), collapse='') ,')'),
            series = 'pv1',
            transformation = 'identity()',
            traindays = 5,
            testdays = 2,
            xreg = paste0(deparse(obsDummies.fcast), collapse=''),
            serial = TRUE)

# 11th hour to 17th hour, K=2
obsDummies.fcast <- quote(
  {cbind(
    dummies=getNthObsDummies(11*(frequency(.)/24)+1, 6*(frequency(.)/24), length(.), frequency(.)),
    fourier(., h=h, K=3)
  )}
)

obsDummies.fit <- quote(
  {cbind(
    dummies=getNthObsDummies(11*(frequency(.)/24)+1, 6*(frequency(.)/24), length(.), frequency(.)),
    fourier(., K=3)
  )}
)

report.full(model = paste0('Arima(order=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(obsDummies.fit), collapse='') ,')'),
            series = 'pv1',
            transformation = 'identity()',
            traindays = 5,
            testdays = 2,
            xreg = paste0(deparse(obsDummies.fcast), collapse=''),
            serial = TRUE)