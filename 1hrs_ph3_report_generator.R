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

report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), xreg=fourier(., K=4), method="ML")',
       series = '1hrs ph3',
       transformation = 'identity()',
       traindays = 7,
       testdays = 1,
       xreg = 'fourier(., K=4, h=h)')
