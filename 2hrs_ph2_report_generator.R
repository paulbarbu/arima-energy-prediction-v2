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

# ???  ----

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

#
