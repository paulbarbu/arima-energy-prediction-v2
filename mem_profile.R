library(profvis)

source("funcs.R")
prof.2hrsph3 <- lineprof(
  fullforecast.serial(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="ML")',
                      dataset = datasets[['2hrs ph3']]$series,
                      transformation = 'identity()',
                      traindays = 7,
                      testdays = 1,
                      xreg = NULL,
                      max.iterations = 2))
print(prof.2hrsph3)

prof.ph3 <- lineprof(
  fullforecast.serial(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="ML")',
                      dataset = datasets[['ph3']]$series,
                      transformation = 'identity()',
                      traindays = 1,
                      testdays = 1,
                      xreg = NULL,
                      max.iterations = 2))
print(prof.ph3)

prof.ph3.serial.obs <- profvis(
    fullforecast.serial.obs(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="ML")',
                        dataset = datasets[['ph3']]$series,
                        transformation = 'identity()',
                        trainobs = 12, #frequency(datasets[['ph3']]$series),
                        testobs = 1,
                        xreg = NULL,
                        max.iterations = 0)
)

print(prof.ph3.serial.obs)


prof.2hrsph3.serial.obs <- profvis(
  fullforecast.serial.obs(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="ML")',
                        dataset = datasets[['2hrs ph3']]$series,
                        transformation = 'identity()',
                        trainobs = 12, #frequency(datasets[['ph3']]$series),
                        testobs = 1,
                        xreg = NULL,
                        max.iterations = 0)
)
print(prof.2hrsph3.serial.obs)
