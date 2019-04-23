#' rev_ts function
#' #' for main diagnostics on general time series data
#' @ parameter df input data
#' ...other important comments
#' .....
#' .....
#' to add: import, export, examples # ***

#' the main resources for this function are .............
#' the function performs the main checks of stationarity, auto-correlation for time series data

rev_ts <- function(df, ...) {

  # example instead of some data df or one of its columns

xxxx <- rnorm(100) # null
tseries::jarque.bera.test(xxxx)
xxxx <- runif(100) # alternative
tseries::jarque.bera.test(xxxx)

xxxx <- rnorm(1000) # is level stationary
tseries::kpss.test(xxxx)
yyyy <- cumsum(xxxx) # has unit root
tseries::kpss.test(yyyy)
xxxx <- 0.3*(1:1000)+rnorm(1000) # is trend stationary
tseries::kpss.test(xxxx, null = "Trend")
tseries::adf.test(rnorm(100))
#tseries::adf.test(rnorm(100))
#tseries::acf(rnorm(100))
#improved by Hyndman, in forecast package (maybe more in tidyvets)
forecast::Acf(xxxx)
forecast::Pacf(xxxx)

#cross-correlations of two series
forecast::Ccf(xxxx, yyyy)

##tapered versions as in Hyndman (2015), Discussion of High-dimensional autocovariance matrices ..."
#forecast::taperedacf(xxxx)
#forecast::taperedpacf(xxxx)


}
