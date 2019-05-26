Predicting energy usage with the ARIMA algorithm (V2)
=====================================================
This is the second iteration of https://github.com/paulbarbu/arima-energy-prediction

Implemented in R 3.5.0.

Dependencies
============
R version 3.5.0 (2018-04-23) -- "Joy in Playing"

This project depends on the following R packages:

* forecast
* foreach
* doParallel
* openssl
* ggplot2
* gridExtra
* knitr
* rmarkdown

Installation & usage
====================
On Windows I recommand using RStudio, while on Linux the command line is sufficiently intuitive to work directly there.
In RStudio simply open one of the files ending in "report_generator.R" and set the source directory to be the one of the currently opened file.

Now you can run either a snippet of code in RStudio by selecting it or you can just run the whole file.
Depending on the file you've selected this may take up to several hours.

The `report` function is used to create a report file (using `knitr`) on a small part of the data, this is a detailed
report containing several plots and statistical indicators as to what ARIMA parameters are best suited to forecast the given data.

Once the user is fully aware of the data and its features, the `report.full` function may be used to generate the forecasts on the full data set.
This is usually what we're interested in, the forecast on a set of data and the accuracy.

One of the most complex calls to `report.full` is as follows:
```
dailyD.fcast <- quote(
  {cbind(
    dummies=getDailyDummies(h, frequency(.), start(.)[[1]]),
    fourier(., h=h, K=2)
  )}
)

dailyD.fit <- quote(
  {cbind(
    dummies=getDailyDummies(length(.), frequency(.), start(.)[[1]]),
    fourier(., K=2)
  )}
)

report.full(model = paste0('Arima(order=c(1, 0, 0), seasonal=c(1, 0, 0), method="CSS", xreg=', paste0(deparse(dailyD.fit), collapse='') ,')'),
            series = '1hrs pv1',
            transformation = 'identity()',
            traindays = 5,
            testdays = 2,
            xreg = paste0(deparse(dailyD.fcast), collapse=''))
```

This call shows that an ARIMA(1, 0, 0)(1, 0, 0) with external regressors (daily dummy variables) is used on the _pv1_ series sub-sampled at one hour intervals.
No transformation is applied on the data since the argument is `identity()`.
The model is consecutively fit on five training days and is evaluated on two test days until the whole data set is forecast.

The `quote` and `deparse` functions are used to turn pieces of code into strings and back.
This is needed in order to be able to dinamically change the external regressors used by the ARIMA model.

Both `render` and `render.full` will generate a report file (either in HTML or PDF format), the report's source code can be inspected in `arima_model.Rmd` and in `full_forecast_model.Rmd` respectively.
At the core of creating a report for the full data is the `fullforecast` function, which accepts the same parameters as `report.full`,
the main difference being that the data is not passed in by its name, instead the values in the set are used now (as a `ts` variable).
Note that the `arima_model.Rmd` file and `full_forecast_model.Rmd` files can be run individually from RStudio by setting the correct parameters at the top of the files.

The results of the `report.full` call are stored in a separate directory, each time series will have its forecasted results in its own directory.
The results are composed of five file types:
 * the main .HTML (or .PDF) one, where the accuracy of the model and the forecasted data is plotted.
	Usually this file is used to inspect the usefulness of the algorithm.
 * the .POINTS.CSV one, where the forecasted points in the series are stored.
 * the .ADJPOINTS.CSV one, where the forecasted points in the series (adjusted to be only positive) are stored.
	This file can be used to re-draw the plot or to further investigate details about the forecasts.
 * the .RDATA one, where the forecasted points, the adjusted forecasted points together with their respective accuracy measures are held.
	The only difference between the CSV files is that the accuracies are stored here as well.
 * the .TXT one, where information with regards to the full ARIMA model are held.
	Due to technical reasons the previously mentioned files have their names trimmed to 100 characters and sanitized.
	This leads to incomplete names or to possibl ename duplication when several models are run.
	Hence each of the previous files will get a hash at the end of the name (eg. 1f8ab92b4c2080cf824405f705f955e9).
	Then this TXT file will have exactly that hash name and will contain the unabriged version of the ARIMA parameters used to forecast the data.
	
As one can see the above files only provide the results of the forecast (either in graphic form, the HTML/PDF file) or in numerical form in the CSV/RDATA files.
The original data is not provided as part of those files since they are stored separately.
For details on the original data, one should look into the datasets.R file.
This file can be directly loaded in R and then access to the time series can be made via the `datasets` variable.
What this file accomplishes is nothing more than loading the data stored in the *data/* directory.
Each TXT file in there holds the correctly sampled (according to the name) version of time series points from the fems20.csv data, for example:
 * *2hrs-pv2.txt* holds the PV2 time series sampled every two hours 
 * *hourly-pv2.txt* holds the PV2 time series sampled at one hour intervals
 * *pv2.txt* holds the original PV2 time series (5 minute sampling rate) from which the other two series were derived
 
License (MIT)
=============
Please note that this is my dissertation project for which I have an exam in July 2019.
Please note that I do not claim any rights on the data used in this project.
The data is the same one used by Stefan Feilmeier in "Stefan Feilmeier. Loads management based on Photovoltaic and Energy Storage System.
MSc Thesis, "Lucian Blaga" University of Sibiu, Supervisor: Árpád Gellért, 2015."

Copyright 2018-2019 (c) Barbu Paul - Gheorghe

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
