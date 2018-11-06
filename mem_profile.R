library(lineprof)

source("funcs.R")
prof.2hrsph3 <- lineprof(
  fullforecast.serial(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="ML")',
                      dataset = datasets[['2hrs ph3']]$series,
                      transformation = 'identity()',
                      traindays = 7,
                      testdays = 1,
                      xreg = NULL,
                      max.iterations = 2))
shine(prof.2hrsph3)

prof.ph3 <- lineprof(
  fullforecast.serial(model = 'Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="ML")',
                      dataset = datasets[['ph3']]$series,
                      transformation = 'identity()',
                      traindays = 1,
                      testdays = 1,
                      xreg = NULL,
                      max.iterations = 2))
shine(prof.ph3)
