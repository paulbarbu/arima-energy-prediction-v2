datasets <- list(
  '2hrs ph3' = list(
    series = ts(read.csv(file = '../energyarima/benchmarks/2hrs-ph3.txt'), frequency=(24*60)/120, start=0),
    #series = msts(read.csv(file = '../energyarima/benchmarks/2hrs-ph3.txt'), seasonal.periods=c((24*60)/120, (24*60*7)/120), start=0),
    time_span = 120 #minutes
  ),
  '2hrs ph2' = list(
    series = ts(read.csv(file = '../energyarima/benchmarks/2hrs-ph2.txt'), frequency=(24*60)/120, start=0),
    time_span = 120 #minutes
  ),
  '2hrs ph1' = list(
    series = ts(read.csv(file = '../energyarima/benchmarks/2hrs-ph1.txt'), frequency=(24*60)/120, start=0),
    time_span = 120 #minutes
  ),
  '2hrs pv2' = list(
    series = ts(read.csv(file = '../energyarima/benchmarks/2hrs-pv2.txt'), frequency=(24*60)/120, start=0),
    time_span = 120 #minutes
  ),
  '2hrs pv1' = list(
    series = ts(read.csv(file = '../energyarima/benchmarks/2hrs-pv1.txt'), frequency=(24*60)/120, start=0),
    time_span = 120 #minutes
  ),
  '1hrs ph3' = list(
    series = ts(read.csv(file = '../energyarima/benchmarks/hourly-ph3.txt'), frequency=(24*60)/60, start=0), 
    time_span = 60 #minutes
  ),
  '1hrs ph2' = list(
    series = ts(read.csv(file = '../energyarima/benchmarks/hourly-ph2.txt'), frequency=(24*60)/60, start=0), 
    time_span = 60 #minutes
  ),
  '1hrs ph1' = list(
    series = ts(read.csv(file = '../energyarima/benchmarks/hourly-ph1.txt'), frequency=(24*60)/60, start=0), 
    time_span = 60 #minutes
  ),
  '1hrs pv2' = list(
    series = ts(read.csv(file = '../energyarima/benchmarks/hourly-pv2.txt'), frequency=(24*60)/60, start=0), 
    time_span = 60 #minutes
  ),
  '1hrs pv1' = list(
    series = ts(read.csv(file = '../energyarima/benchmarks/hourly-pv1.txt'), frequency=(24*60)/60, start=0), 
    time_span = 60 #minutes
  ),
  'ph3' = list(
    series = ts(read.csv(file = '../energyarima/benchmarks/fems20.csv')['Ph3'], frequency=(24*60)/5, start=0),
    time_span = 5 #minutes
  ),
  'ph2' = list(
    series = ts(read.csv(file = '../energyarima/benchmarks/fems20.csv')['Ph2'], frequency=(24*60)/5, start=0),
    time_span = 5 #minutes
  ),
  'ph1' = list(
    series = ts(read.csv(file = '../energyarima/benchmarks/fems20.csv')['Ph1'], frequency=(24*60)/5, start=0),
    time_span = 5 #minutes
  ),
  'pv2' = list(
    series = ts(read.csv(file = '../energyarima/benchmarks/fems20.csv')['PV2'], frequency=(24*60)/5, start=0),
    time_span = 5 #minutes
  ),
  'pv1' = list(
    series = ts(read.csv(file = '../energyarima/benchmarks/fems20.csv')['PV1'], frequency=(24*60)/5, start=0),
    time_span = 5 #minutes
  )
)
