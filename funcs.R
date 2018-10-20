
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
  days$train <- window(dataset, start=c(startday, 1), end=c(startday + traindays, 0))
  days$test <- window(dataset, start=c(startday + traindays, 1), end=c(startday + traindays + testdays, 0))
  
  return(days)
}
