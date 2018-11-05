library(rmarkdown)
library(fs)

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


report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0))',
            series = 'ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1)

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
