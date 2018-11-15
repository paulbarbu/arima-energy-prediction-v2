library(forecast)
library(foreach)
library(doParallel)
library(fs)
library(rmarkdown)

source('datasets.R')

# series= datasets[['1hrs ph3']]$series; x = get_days(series, 0, 1, 1); View(series); View(x$train); View(x$test)
# series= datasets[['1hrs ph3']]$series; x = get_days(series, 1, 1, 1); View(series); View(x$train); View(x$test)
get_days <- function(dataset, startday, traindays = 7, testdays=3)
{
  if(startday < 0) 
  {
    startday <- end(dataset)[1] + 1 + startday 
    #print(paste("modified start day = ", startday))
  }
  days <- list()
  
  if(is.null(traindays) || traindays == 0)
  {
    days$train <- window(dataset, start=c(startday, 1))
    testdays <- NULL
  }
  else
  {
    days$train <- window(dataset, start=c(startday, 1), end=c(startday + traindays, 0))
  }
  
  if(is.null(testdays) || testdays == 0)
  {
    days$test <- NULL
  }
  else
  {
    days$test <- window(dataset, start=c(startday + traindays, 1), end=c(startday + traindays + testdays, 0))
  }
  
  return(days)
}

get_obs <- function(dataset, startobs, trainobs = 7, testobs=3)
{
  if(startobs < 0) 
  {
    startobs <- length(dataset) + 1 + startobs
    #print(paste("modified start day = ", startday))
  }
  days <- list()
  
  if(is.null(trainobs) || trainobs == 0)
  {
    days$train <- subset(dataset, start=startobs)
    testobs <- NULL
  }
  else
  {
    days$train <- subset(dataset, start=startobs, end=startobs + trainobs-1)
  }
  
  if(is.null(testobs) || testobs == 0)
  {
    days$test <- NULL
  }
  else
  {
    days$test <- subset(dataset, start=startobs + trainobs, end=startobs + trainobs + testobs-1)
  }
  
  return(days)
}

report <- function(...)
{
  report.params <- list(...)
  
  fmt <- paste(paste(rep('%s.', length(report.params)), collapse=''), 'html', sep='')
  filename <- path_sanitize(do.call(sprintf, c(fmt, report.params)))
  
  dirname <- gsub(' ', '_', report.params$series)
  
  print(sprintf('Generating report file: %s/%s', dirname, filename))
  
  render("arima_model.Rmd",
         params = report.params,
         output_file = filename,
         output_dir = dirname,
         quiet = TRUE)  
  
  print(sprintf('Done: %s/%s', dirname, filename))
}

report.full <- function(output_format='html_document', ...)
{
  report.params <- list(...)
  
  extension <- "html"
  if(output_format == "pdf_document")
  {
    extension <- "pdf"
  }
  
  fmt <- paste('full_', paste(rep('%s.', length(report.params)), collapse=''), extension, sep='')
  filename <- path_sanitize(do.call(sprintf, c(fmt, report.params)))
  
  dirname <- gsub(' ', '_', report.params$series)
  
  print(sprintf('Generating full data report file: %s/%s', dirname, filename))
  
  gc()
  render("full_forecast_model.Rmd",
         params = report.params,
         output_file = filename,
         output_dir = dirname,
         output_format = output_format,
         quiet = TRUE)  
  
  print(sprintf('Done: %s/%s', dirname, filename))
  gc()
}


#fullforecast(get_days(datasets$`2hrs ph3`$series, 0, 3, 1)$train, 'identity()', 'Arima(order=c(1,1,0))', 1, 1, NULL) -> a
fullforecast <- function(dataset, transformation, model, traindays, testdays, xreg)
{
  startday <- 0
  endday <- end(dataset)[1] + 1 - (traindays + testdays)
  
  fcasts <- list()
  fcasts$points <- c()
  
  numcores <- max(1, detectCores() - 1)
  
  cl <- makeCluster(numcores)
  registerDoParallel(cl)
  
  print(paste('Running on', numcores, 'cores')) 
  
  gc()
  
  fcasts$points <- foreach(currentday = seq(startday, endday, testdays),
                           .export = c("get_days"),
                           .packages = c("forecast"),
                           .combine = c, 
                           .verbose = TRUE) %dopar%
  {
    print(paste("Current day =", currentday))
    datachunk <- get_days(dataset, currentday, traindays, testdays)
    
    eval(parse(text=paste('datachunk$train %>%',
                          transformation))) -> datachunk$train.transformed
    
    h <- testdays * frequency(datachunk$test)
    
    # for naive, snaive and meanf models
    # the model already returns the forecasts, no separate forecast step
    # is needed
    if(any(startsWith(model, c("naive()", "snaive()", "meanf()"))))
    {
      model <- sub("()", paste("(h=", h, ")", sep=""), model, fixed=TRUE)
    } 
    
    eval(parse(text=paste('datachunk$train.transformed %>%', model))) -> fit
    
    fcast <- NULL
    
    # for naive, snaive and meanf models
    # the model already returns the forecasts, no separate forecast step
    # is needed
    if(any(startsWith(model, c("naive(", "snaive(", "meanf("))))
    {
      chunk.fcast <- fit
    } else
    {
      fourier.terms <- NULL
      
      if(!is.null(xreg))
      {
        eval(parse(text=paste('datachunk$test %>%', xreg))) -> fourier.terms
      }
      
      fit %>% forecast(h=h, xreg=fourier.terms) -> chunk.fcast
    }
    
    gc()
    
    return(chunk.fcast$mean)
  }
  
  gc()
  
  stopCluster(cl)
  
  fcasts$points <- ts(fcasts$points, start=traindays, frequency = frequency(dataset))
  testpoints <- get_days(dataset, traindays, NULL, NULL)$train
  
  fcasts$accuracy <- accuracy(fcasts$points, testpoints)
  
  fcasts$adjpoints <- fcasts$points
  fcasts$adjpoints[fcasts$adjpoints < 0] <- 0
  fcasts$adjaccuracy <- accuracy(fcasts$adjpoints, testpoints)
  
  return(fcasts)
}

fullforecast.serial <- function(dataset, transformation, model, traindays, testdays, xreg, max.iterations = NULL)
{
  startday <- 0
  if(is.null(max.iterations))
  {
    endday <- end(dataset)[1] + 1 - (traindays + testdays)
  }else{
    endday <- max.iterations
  }
  
  fcasts <- list()
  fcasts$points <- c()
  
  for(currentday in seq(startday, endday, testdays))
  {
    print(paste("Current day =", currentday))
    datachunk <- get_days(dataset, currentday, traindays, testdays)
    
    eval(parse(text=paste('datachunk$train %>%',
                         transformation))) -> datachunk$train.transformed
    
    h <- testdays * frequency(datachunk$test)
    
    # for naive, snaive and meanf models
    # the model already returns the forecasts, no separate forecast step
    # is needed
    if(any(startsWith(model, c("naive()", "snaive()", "meanf()"))))
    {
     model <- sub("()", paste("(h=", h, ")", sep=""), model, fixed=TRUE)
    } 
    
    eval(parse(text=paste('datachunk$train.transformed %>%', model))) -> fit
    
    fcast <- NULL
    
    # for naive, snaive and meanf models
    # the model already returns the forecasts, no separate forecast step
    # is needed
    if(any(startsWith(model, c("naive(", "snaive(", "meanf("))))
    {
     chunk.fcast <- fit
    } else
    {
     fourier.terms <- NULL
     
     if(!is.null(xreg))
     {
       eval(parse(text=paste('datachunk$test %>%', xreg))) -> fourier.terms
     }
     
     fit %>% forecast(h=h, xreg=fourier.terms) -> chunk.fcast
    }
    
    fcasts$points <- c(fcasts$points, chunk.fcast$mean)
    
    gc()
  }
  
  gc()
  
  fcasts$points <- ts(fcasts$points, start=traindays, frequency = frequency(dataset))
  testpoints <- get_days(dataset, traindays, NULL, NULL)$train
  
  fcasts$accuracy <- accuracy(fcasts$points, testpoints)
  
  fcasts$adjpoints <- fcasts$points
  fcasts$adjpoints[fcasts$adjpoints < 0] <- 0
  fcasts$adjaccuracy <- accuracy(fcasts$adjpoints, testpoints)
  
  return(fcasts)
}

fullforecast.obs <- function(dataset, transformation, model, trainobs, testobs, xreg)
{
  startobs <- 0
  endobs <- length(dataset) - (trainobs + testobs)
  
  fcasts <- list()
  fcasts$points <- c()
  
  numcores <- max(1, detectCores() - 1)
  
  cl <- makeCluster(numcores)
  registerDoParallel(cl)
  
  print(paste('Running on', numcores, 'cores')) 
  
  gc()
  
  fcasts$points <- foreach(currentobs = seq(startobs, endobs, testobs),
                           .export = c("get_obs"),
                           .packages = c("forecast"),
                           .combine = c, 
                           .verbose = TRUE) %dopar%
   {
     print(paste("Current obs =", currentobs))
     datachunk <- get_obs(dataset, currentobs, trainobs, testobs)
     
     eval(parse(text=paste('datachunk$train %>%',
                           transformation))) -> datachunk$train.transformed
     
     h <- testobs
     
     # for naive, snaive and meanf models
     # the model already returns the forecasts, no separate forecast step
     # is needed
     if(any(startsWith(model, c("naive()", "snaive()", "meanf()"))))
     {
       model <- sub("()", paste("(h=", h, ")", sep=""), model, fixed=TRUE)
     } 
     
     eval(parse(text=paste('datachunk$train.transformed %>%', model))) -> fit
     
     fcast <- NULL
     
     # for naive, snaive and meanf models
     # the model already returns the forecasts, no separate forecast step
     # is needed
     if(any(startsWith(model, c("naive(", "snaive(", "meanf("))))
     {
       chunk.fcast <- fit
     } else
     {
       fourier.terms <- NULL
       
       if(!is.null(xreg))
       {
         eval(parse(text=paste('datachunk$test %>%', xreg))) -> fourier.terms
       }
       
       fit %>% forecast(h=h, xreg=fourier.terms) -> chunk.fcast
     }
     
     gc()
     
     return(chunk.fcast$mean)
   }
  
  gc()
  
  stopCluster(cl)
  
  fcasts$points <- ts(fcasts$points, start=trainobs/frequency(dataset), frequency = frequency(dataset))
  testpoints <- get_obs(dataset, trainobs, NULL, NULL)$train
  
  fcasts$accuracy <- accuracy(fcasts$points, testpoints)
  
  fcasts$adjpoints <- fcasts$points
  fcasts$adjpoints[fcasts$adjpoints < 0] <- 0
  fcasts$adjaccuracy <- accuracy(fcasts$adjpoints, testpoints)
  
  return(fcasts)
}


fullforecast.serial.obs <- function(dataset, transformation, model, trainobs, testobs, xreg, max.iterations = NULL)
{
  startobs <- 0
  
  if(is.null(max.iterations))
  {
    endobs <- length(dataset) - (trainobs + testobs)
  }else{
    endobs <- max.iterations
  }
  
  fcasts <- list()
  fcasts$points <- c()
  
  for(currentobs in seq(startobs, endobs, testobs))
  {
    print(paste("Current obs =", currentobs))
    datachunk <- get_obs(dataset, currentobs, trainobs, testobs)
    
    eval(parse(text=paste('datachunk$train %>%',
                          transformation))) -> datachunk$train.transformed
    
    h <- testobs
    
    # for naive, snaive and meanf models
    # the model already returns the forecasts, no separate forecast step
    # is needed
    if(any(startsWith(model, c("naive()", "snaive()", "meanf()"))))
    {
      model <- sub("()", paste("(h=", h, ")", sep=""), model, fixed=TRUE)
    } 
    
    eval(parse(text=paste('datachunk$train.transformed %>%', model))) -> fit
    
    fcast <- NULL
    
    # for naive, snaive and meanf models
    # the model already returns the forecasts, no separate forecast step
    # is needed
    if(any(startsWith(model, c("naive(", "snaive(", "meanf("))))
    {
      chunk.fcast <- fit
    } else
    {
      fourier.terms <- NULL
      
      if(!is.null(xreg))
      {
        eval(parse(text=paste('datachunk$test %>%', xreg))) -> fourier.terms
      }
      
      fit %>% forecast(h=h, xreg=fourier.terms) -> chunk.fcast
    }
    
    fcasts$points <- c(fcasts$points, chunk.fcast$mean)
    
    gc()
  }
  
  gc()
  
  fcasts$points <- ts(fcasts$points, start=trainobs/frequency(dataset), frequency = frequency(dataset))
  testpoints <- get_obs(dataset, trainobs, NULL, NULL)$train
  
  fcasts$accuracy <- accuracy(fcasts$points, testpoints)
  
  fcasts$adjpoints <- fcasts$points
  fcasts$adjpoints[fcasts$adjpoints < 0] <- 0
  fcasts$adjaccuracy <- accuracy(fcasts$adjpoints, testpoints)
  
  return(fcasts)
}
