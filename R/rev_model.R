#' rev_model function
#'  for main diagnostics on goodness of fit
#'  residual testing
#'
#' @param mts mts=m means glm or lm model, mts=ts means time series model
#'
#' @parameter model_a,  any fitted glm or lm model
#'
#' ...other important comments
#' .....
#' .....
#' to add: import, export, examples # ***
#' @export

rev_model <- function( mts, model_a, ...) {

  if (mts==m)
    #model is regression type or glm
  {

  print(raintest(model_a, order.by="mahalanobis"))
  #------------------------ rainbow test for linearity
  ### J.M. Utts (1982), The Rainbow Test for Lack of Fit in Regression.
  ### Communications in Statistics -- Theory and Methods 11,2801--2815.

  dwtest(model_a)
  #--------------Durbin-Watson-Test on autocorrelation of disturbances

  dev.new()
  hist(residuals(model_a))

  #---------- normality test for residuals of models
  print(jarque.bera.test(residuals(model_a)))

  ##a GOF test based on an entropy measure
  #Justine Lequesne, Philippe Regnault. vsgoftest:
  #An R Package for Goodness-of-Fit Testing Based on
  #Kullback-Leibler Divergence. 2018. ￿hal-01816063
  vsgoftest::vs.test(residual(model_a), dnorm)

  }

  else
  # time series model
  {

  # ts models ***

  print(Box.test (residuals(model_a), lag = 1, type="Ljung"))
  #----------- portmanteau tests for model residuals, ts models


  dev.new()
  opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
  plot(model_a, las = 1)
  dev.new()
  lag.plot(residuals(model_a), lags=12, do.lines=FALSE)

  print(kpss.test(residuals(model_a)))  #-----------------------------stat
  print(adf.test(residuals(model_a)))  #-------- non-stationary

  dev.new()
  par(mfrow=c(2,1))
  acf(residuals(model_a))  # -------- estimate of autocorrelation fct,
  uni/multi-variate
  pacf(residuals(model_a)) # -------- estimate of partial autocorrelation fct, uni/multi-variate

  }
}
