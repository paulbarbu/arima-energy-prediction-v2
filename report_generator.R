library(rmarkdown)
library(fs)

source('funcs.R')

# 2hrs ph3 Simple Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0)) ----
# arima(1,0,0)(1,0,0)

report(model = 'Arima(order=c(1, 0, 0))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3)

report(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3)

report(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0))',
       series = '2hrs ph3',
       transformation = 'log() %>% (function(d) {d[d==-Inf]<-0; d})',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3)

report(model = 'Arima(order=c(0, 0, 1))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3)

report(model = 'Arima(order=c(0, 0, 1), seasonal=c(0, 0, 1))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3)

report(model = 'Arima(order=c(0, 0, 1), seasonal=c(0, 0, 1), xreg=fourier(., K=5))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3,
       xreg='fourier(., K=5, h=h)')

report(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), xreg=fourier(., K=5))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3,
       xreg='fourier(., K=5, h=h)')

report(model = 'Arima(order=c(0, 1, 1), seasonal=c(0, 1, 1))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3)

report(model = 'Arima(order=c(1, 1, 0), seasonal=c(1, 1, 0))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3)

report(model = 'auto.arima()',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3)

# 2hrs ph3 -1 day forecast - Simple Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0)) ----
# fourier arima(1,0,0)(1,0,0)
report(model = 'Arima(order=c(1, 0, 0))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 1)

report(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 1)

report(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0))',
       series = '2hrs ph3',
       transformation = 'log() %>% (function(d) {d[d==-Inf]<-0; d})',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 1)

report(model = 'Arima(order=c(0, 0, 1))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 1)

report(model = 'Arima(order=c(0, 0, 1), seasonal=c(0, 0, 1))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 1)

report(model = 'Arima(order=c(0, 0, 1), seasonal=c(0, 0, 1), xreg=fourier(., K=5))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 1,
       xreg='fourier(., K=5, h=h)')

report(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), xreg=fourier(., K=5))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 1,
       xreg='fourier(., K=5, h=h)')

report(model = 'Arima(order=c(0, 1, 1), seasonal=c(0, 1, 1))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 1)

report(model = 'Arima(order=c(1, 1, 0), seasonal=c(1, 1, 0))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 1)

report(model = 'auto.arima()',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 1)

# 2hrs ph3 from the front ----

report(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = 0,
       traindays = 7,
       testdays = 1)

report(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), xreg=fourier(., K=5))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = 0,
       traindays = 7,
       testdays = 1,
       xreg='fourier(., K=5, h=h)')

report(model = 'auto.arima()',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = 0,
       traindays = 7,
       testdays = 1)


#TODO: auto.arima for all data
#TODO: set K to an interval in order to find the best one, how to choose it?

# 2hrs ph3 full ----
report.full(model = 'auto.arima()',
       series = '2hrs ph3',
       transformation = 'identity()',
       traindays = 7,
       testdays = 1)

#https://stackoverflow.com/questions/7233288/non-stationary-seasonal-ar-part-from-css-error-in-r
report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="ML")',
            series = '2hrs ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="ML", xreg=fourier(., K=5))',
            series = '2hrs ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1,
            xreg='fourier(., K=5, h=h)')
