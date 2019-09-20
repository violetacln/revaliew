#' rev_model functions
#'  for residual testing and goodness of fit testing
#'  where the model is glm type or a time series model
#' @parameter model_a,  any fitted glm or lm model; or a time-series model
#'
#' ...other important comments
#' @import lmtest
#' @examples
#' x <- c(1:30); y <- x^2+rnorm(30,0,2); model_ex = lm(y~x);
#' rev_model(model_a= model_ex)
#' model_ex_ts # to add an example
#' rev_model_ts(model_a=model_ex_ts)
#' @export

rev_model <- function(model_a, ...) {
  #------- model is regression type or glm ----------------
  raintesting <- lmtest::raintest(model_a, order.by="mahalanobis")
  print(raintesting)
  #------------------------ rainbow test for linearity
  ### J.M. Utts (1982), The Rainbow Test for Lack of Fit in Regression.
  ### Communications in Statistics -- Theory and Methods 11,2801--2815.

  durbin_watson_testing <- lmtest::dwtest(model_a)
  #--------------Durbin-Watson-Test on autocorrelation of disturbances

  plot_resid_distr <- hist(residuals(model_a))
  plot(plot_resid_distr)

  #---------- normality test for residuals of models
  jarque_bera_testing <- tseries::jarque.bera.test(residuals(model_a))
  print(jarque_bera_testing)

  ##a GOF test based on an entropy measure
  ##Justine Lequesne, Philippe Regnault. vsgoftest:
  ##An R Package for Goodness-of-Fit Testing Based on
  ##Kullback-Leibler Divergence, 2018
 #library("vsgoftest")
vs_goodness_of_fit_testing <- vsgoftest::vs.test(x=residuals(model_a), densfun="dnorm")
print(vs_goodness_of_fit_testing)

  out_model_glm <- list( raintesting, durbin_watson_testing,
                         jarque_bera_testing
                         ,vs_goodness_of_fit_testing)

  return(out_model_glm)
}


#----- model is a time series model ------------------------------
rev_model_ts <- function(model_a, ...) {

  #---- portmanteau tests for model residuals, ts models
  portmanteau_testing_ts_resid <- Box.test (residuals(model_a), lag = 1, type="Ljung")
  print(portmanteau_testing_ts_resid)
  # visualization, model diagnostics
  dev.new()
  opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
  plot(model_a, las = 1)
  # visualization, residuals diagnostics
  dev.new()
  lag.plot(residuals(model_a), lags=12, do.lines=FALSE)

  kpss_testing <- kpss.test(residuals(model_a))
  print(kpss_testing)  #------- stationarity

  adf_testing <- adf.test(residuals(model_a))
  print( adf_testing)  #-------- non-stationarity

  #visualization: (auto-) correlation
  dev.new()
  par(mfrow=c(2,1))
  acf(residuals(model_a))  # -------- estimate of autocorrelation fct,uni/multi-variate
  pacf(residuals(model_a)) # -------- estimate of partial autocorrelation fct, uni/multi-variate

out_model_ts <- list(portmanteau_testing_ts_resid, kpss_testing,  adf_testing)

return(out_model_ts)
}
