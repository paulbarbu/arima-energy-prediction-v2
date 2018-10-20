library(rmarkdown)

report <- function(...)
{
  report.params <- list(...)
  
  fmt <- paste(paste(rep('%s.', length(report.params)), collapse=''),'html', sep='')
  filename <- do.call(sprintf, c(fmt, report.params))
  
  dirname <- gsub(' ', '_', report.params$series)
  
  print(sprintf('Generating report file: %s/%s', dirname, filename))
  
  render("arima_model.Rmd",
         params = report.params,
         output_file = filename,
         output_dir = dirname,
         quiet = TRUE)  
  
  print(sprintf('Done: %s/%s', dirname, filename))
}

report(model = 'Arima(order=c(1, 0, 0))',
       series = '2hrs ph3',
       startday = -10,
       traindays = 7,
       testdays = 3)

