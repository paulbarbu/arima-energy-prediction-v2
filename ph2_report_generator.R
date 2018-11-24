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

#TODO: run this
# fourier with no SAR terms, derived from the hrs serioes, since that was better for the 5min ph2 series (see above)
report.full(model = 'Arima(order=c(1, 0, 0), method="CSS", xreg=fourier(., K=3))',
            series = 'ph2',
            transformation = 'identity()',
            traindays = 4,
            testdays = 3,
            xreg = 'fourier(., h=h, K=3)',
            serial = TRUE)

# ATTENTION: I converted from "dummies" in the `2hrs ph2 series`` to "hours", this means, I multipled by 2 to get the hourly intervals right
# with 8th hour up to 11th hour dummies - inspiration from 2hrs series combined with the ratio from the 1hrs
# K=3 - more complex seasonality on denser data - from 1hrs
obsDummies.fcast <- quote(
  {cbind(
    dummies=getNthObsDummies(8*(frequency(.)/24)+1, 3*(frequency(.)/24), length(.), frequency(.)),
    fourier(., h=h, K=3)
  )}
)

obsDummies.fit <- quote(
  {cbind(
    dummies=getNthObsDummies(8*(frequency(.)/24)+1, 3*(frequency(.)/24), length(.), frequency(.)),
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

# dummies 4:4 becomse 8:8 hours
# with 8th hour up to 16th hour dummies - (NEW!) inspiration from 2hrs series combined with the ratio from the 1hrs
# K=3 - more complex seasonality on denser data - from 1hrs
obsDummies.fcast <- quote(
  {cbind(
    dummies=getNthObsDummies(8*(frequency(.)/24)+1, 8*(frequency(.)/24), length(.), frequency(.)),
    fourier(., h=h, K=3)
  )}
)

obsDummies.fit <- quote(
  {cbind(
    dummies=getNthObsDummies(8*(frequency(.)/24)+1, 8*(frequency(.)/24), length(.), frequency(.)),
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

#TODO: run this
# ATTENTION: I converted from "dummies" in the `2hrs ph2 series`` to "hours", this means, I multipled by 2 to get the hourly intervals right
# with 8th hour up to 11th hour dummies - inspiration from 2hrs series combined with the ratio from the 1hrs
# K=3 - more complex seasonality on denser data - from 1hrs
# no SAR terms
obsDummies.fcast <- quote(
  {cbind(
    dummies=getNthObsDummies(8*(frequency(.)/24)+1, 3*(frequency(.)/24), length(.), frequency(.)),
    fourier(., h=h, K=3)
  )}
)

obsDummies.fit <- quote(
  {cbind(
    dummies=getNthObsDummies(8*(frequency(.)/24)+1, 3*(frequency(.)/24), length(.), frequency(.)),
    fourier(., K=3)
  )}
)

report.full(model = paste0('Arima(order=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(obsDummies.fit), collapse='') ,')'),
            series = 'ph2',
            transformation = 'identity()',
            traindays = 4,
            testdays = 3,
            xreg = paste0(deparse(obsDummies.fcast), collapse=''),
            serial = TRUE)