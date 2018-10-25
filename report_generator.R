library(rmarkdown)
library(fs)

source('funcs.R')

# 2hrs ph3 Simple Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0)) ----
# arima(1,0,0)(1,0,0)

# One positive lag in ACF & PACF - AR signature
report(model = 'Arima(order=c(1, 0, 0))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3)

# Previous model had residual significant at lag = 12 (24hrs seasonality)
report(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3)

# Try with a transformation, avoid -Inf values
report(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0))',
       series = '2hrs ph3',
       transformation = 'log() %>% (function(d) {d[d==-Inf]<-0; d})',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3)

# Try also with a MA term since neither the ACF nor the PACF
# decays slowly or sinusoidally in order to be sure AR or MA is definitely usable
report(model = 'Arima(order=c(0, 0, 1))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3)

# Same as above, but with seasonal terms, again there were significant residuals 
# at lag 12
report(model = 'Arima(order=c(0, 0, 1), seasonal=c(0, 0, 1))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3)

#Fourier terms for the model AR(1) and SAR(1)
#K was randomly chosen by me
report(model = 'Arima(order=c(0, 0, 1), seasonal=c(0, 0, 1), xreg=fourier(., K=5))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3,
       xreg='fourier(., K=5, h=h)')

#Fourier terms for the model MA(1) and SMA(1)
#K was randomly chosen by me
report(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), xreg=fourier(., K=5))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3,
       xreg='fourier(., K=5, h=h)')

#Try with differences, although no indication of trend or seasonal-trend
report(model = 'Arima(order=c(0, 1, 1), seasonal=c(0, 1, 1))',
       series = '2hrs ph3',
       transformation = 'identity()',
       diffs = 'identity()',
       sdiffs = 'identity()',
       startday = -10,
       traindays = 7,
       testdays = 3)

#Variation on the above
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

# 2hrs ph3 full - apply the best models so far on the full data----
report.full(model = 'auto.arima()',
       series = '2hrs ph3',
       transformation = 'identity()',
       traindays = 7,
       testdays = 1)

#https://stackoverflow.com/questions/7233288/non-stationary-seasonal-ar-part-from-css-error-in-r

# This model predicts the new value as a multiple of the previous (seasonal) value plus a constant
# This model assumes mean reverting behaviour (no trend - no diffs) which for house-hold is true (energy consumption is the same, on average)
report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="ML")',
            series = '2hrs ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

# Fit the seasonality using fourier terms nad the errors using the logic above
report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="ML", xreg=fourier(., K=5))',
            series = '2hrs ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1,
            xreg='fourier(., K=5, h=h)')

report.full(model = 'auto.arima(xreg=fourier(., K=5))',
            series = '2hrs ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1,
            xreg='fourier(., K=5, h=h)')
