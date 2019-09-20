#' rev_ts function
#'  for main diagnostics on general time series data
#' auto-correlation, cross-correlation, stationarity tests,
#' seasonality and related measures
#'
#' @parameter df input data
#' ...other important comments
#' @import anomalous
#' @import forecast
#' @import tseries
#' @import tsfeatures
#' @example
#' rev_ts_univ(tsuniv = ts(rnorm(1000)));
#'
#' tsmultiv_ex = ts(matrix(rnorm(3000),ncol=100),freq=4);
#' rev_ts_multiv(tsmultiv = tsmultiv_ex)
#'
#' @export

rev_ts_univ <- function(tsuniv, ...) {
# univariate ts
 rlist_univ <- list(
        tseries::jarque.bera.test(tsuniv),
        tseries::kpss.test(tsuniv),
        tseries::kpss.test(tsuniv, null = "Trend"),
        tseries::adf.test(tsuniv),
        forecast::Acf(tsuniv),
        forecast::Pacf(tsuniv),
        tsfeatures::tsfeatures(tsuniv)
         )
 return(rlist)

}

# tapered versions could be done as in Hyndman (2015), Discussion of High-dimensional autocovariance matrices ..."
# forecast::taperedacf(xxxx)
# forecast::taperedpacf(xxxx)

# multivariate ts
# using Hyndman´s anomalous and tsfeatures packages --------------
# https://robjhyndman.com/hyndsight/tscharacteristics/
#

rev_ts_multiv <- function(tsmultiv, ...) {
  library(anomalous) ## need to include this
  ## also issues with R version and some dependencies
  ## but it works
  y <- anomalous::tsmeasures(tsmultiv)
  anomalous::biplot.features(y)
  anomalous::anomaly(y)

  # all important characteristics of multivar ts in:
  all_characteristics <- tsfeatures::tsfeatures(tsmultiv)

  ## should include also
  # cross-correlations of any two series
  # ex
  # tsm1=tsmultiv_ex[,10]; tsm2 = tsmultiv_ex[,20]
  # forecast::Ccf(tsm1, tsm2)

return( all_characteristics)

}

