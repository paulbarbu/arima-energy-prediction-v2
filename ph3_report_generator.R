source('funcs.R')

# benchmark models  ----
report.full(model = 'snaive()',
            series = 'ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

report.full(model = 'meanf()',
            series = 'ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

report.full(model = 'naive()',
            series = 'ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

# 7:1 full ----

#Error in { : task 2 failed - "cannot allocate vector of size 6.5 Gb"
report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), xreg=fourier(., K=4), method="CSS")',
            series = 'ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1,
            xreg = 'fourier(., K=4, h=h)')


#$accuracy
#             ME     RMSE      MAE    MPE MAPE      ACF1 Theil's U
#Test set 5.270776 346.4092 210.4319 -Inf  Inf 0.7271158         0
# 1hr30mins of running - sequentially
fullforecast.serial(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS")',
                    dataset = datasets[['ph3']]$series,
                    transformation = 'identity()',
                    traindays = 7,
                    testdays = 1,
                    xreg = NULL)


# still very high memory usage
fullforecast.serial.obs(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="ML")',
                    dataset = datasets[['ph3']]$series,
                    transformation = 'identity()',
                    trainobs = 12, #frequency(datasets[['ph3']]$series),
                    testobs = 1,
                    xreg = NULL,
                    max.iterations = 1)

#simple
report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS")',
            series = 'ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1,
            serial = TRUE)

# inspiration from 1hrs series
report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=fourier(., K=2))',
            series = 'ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1,
            xreg = 'fourier(., h=h, K=2)',
            serial = TRUE)

# inspiration from 1hrs series without SAR term
report.full(model = 'Arima(order=c(1, 0, 0), method="CSS", xreg=fourier(., K=2))',
            series = 'ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1,
            xreg = 'fourier(., h=h, K=2)',
            serial = TRUE)

# inspiration from 2hrs series
report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=fourier(., K=1))',
            series = 'ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1,
            xreg = 'fourier(., h=h, K=1)',
            serial = TRUE)

# 7:2 full ----
#simple
report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS")',
            series = 'ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 2,
            serial = TRUE)

# inspiration from 1hrs & 2hrs series
report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=fourier(., K=2))',
            series = 'ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 2,
            xreg = 'fourier(., h=h, K=2)',
            serial = TRUE)

# with 8th hour up to 11th dummies (inspiration from 1hrs mainly, but also 2hrs sicne there we had 5 obs)
obsDummies.fcast <- quote(
  {cbind(
    dummies=getNthObsDummies(8*(frequency(.)/24)+1, 3*(frequency(.)/24), h, frequency(.)),
    fourier(., h=h, K=2)
  )}
)

obsDummies.fit <- quote(
  {cbind(
    dummies=getNthObsDummies(8*(frequency(.)/24)+1, 3*(frequency(.)/24), length(.), frequency(.)),
    fourier(., K=2)
  )}
)

report.full(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(obsDummies.fit), collapse='') ,')'),
            series = 'ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 2,
            xreg = paste0(deparse(obsDummies.fcast), collapse=''),
            serial = TRUE)

#direct inspiration from 1hrs ph3 - 9:2 dummies
obsDummies.fcast <- quote(
  {cbind(
    dummies=getNthObsDummies(9*(frequency(.)/24)+1, 2*(frequency(.)/24), h, frequency(.)),
    fourier(., h=h, K=2)
  )}
)

obsDummies.fit <- quote(
  {cbind(
    dummies=getNthObsDummies(9*(frequency(.)/24)+1, 2*(frequency(.)/24), length(.), frequency(.)),
    fourier(., K=2)
  )}
)

report.full(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(obsDummies.fit), collapse='') ,')'),
            series = 'ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 2,
            xreg = paste0(deparse(obsDummies.fcast), collapse=''),
            serial = TRUE)


#direct inspiration from 1hrs ph3 - 9:2 dummies without SAR term
obsDummies.fcast <- quote(
  {cbind(
    dummies=getNthObsDummies(9*(frequency(.)/24)+1, 2*(frequency(.)/24), h, frequency(.)),
    fourier(., h=h, K=2)
  )}
)

obsDummies.fit <- quote(
  {cbind(
    dummies=getNthObsDummies(9*(frequency(.)/24)+1, 2*(frequency(.)/24), length(.), frequency(.)),
    fourier(., K=2)
  )}
)

report.full(model = paste0('Arima(order=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(obsDummies.fit), collapse='') ,')'),
            series = 'ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 2,
            xreg = paste0(deparse(obsDummies.fcast), collapse=''),
            serial = TRUE)
