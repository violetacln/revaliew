#' check_assumptions functions:
#' checking main assumptions about data:
#' made by the data distribution itself, at a different sampling point in time
#' or by using tests/models
#'
#' @param df input data
#' @param df1 input data
#' @param df2 input data
#' @param assumption the name of the assumption of the test
#' @param test_name the name of the statistical test
#' @param model_name the name of the statistical model
#' @import tidyverse
#' @import magrittr
#' @import sm
#' @import LaplacesDemon
#' @examples check_assumptions_data(df1 = iris[1:50,]$Petal.Length, df2= iris[101:150,]$Petal.Length)
#' @export


check_assumptions_data <- function(df1, df2) {
  print("the assumption about data to be checked: distributional difference")

  #---- check if df1 and df2 come from different distributions:
  # use resampling methods and/or KL measure

  #continuous
  library(LaplacesDemon)
  kld <- LaplacesDemon::KLD(px=df1,py=df2)

  #or resampling based tests
  #sm::density.compare
  #Bowman, A.W. and Azzalini, A. (1997). Applied Smoothing Techniques for Data Analysis: the Kernel Approach with S-Plus Illustrations. Oxford University Press, Oxford.
  # where two groups, each of length: length(px), length(py) need to be compared
  group.index <- rep( 1:2, c(length(df1), length(df2)) )
  ## collect data together and use sm.density.compare()
  den <- sm::sm.density.compare(c(px=df1,py=df2), group = group.index, model = "equal")
  ## plot is generated automatically by this function

compared <- list(den, kld)
return()

}


check_assumptions_test <- function(df, test_name) {
 #--------------
  print("the test assumptions about data to be checked")
}


check_assumptions_model <- function(model_name) {
  #---------------
  print("the model assumptions about data to be checked")
}
