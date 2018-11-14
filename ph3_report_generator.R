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

# inspiration from 2hrs series
report.full(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=fourier(., K=1))',
            series = 'ph3',
            transformation = 'identity()',
            traindays = 7,
            testdays = 1,
            xreg = 'fourier(., h=h, K=1)',
            serial = TRUE)

