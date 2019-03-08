
#' view_univar function
#' univariate analysis: distribution functions
#' @param df input data
#' ....other important comments
#'
#' @import tidyverse
#' @import magrittr
#' @import DataExplorer
#' @import funModeling
#' @import ggplot2
#' @import  rmarkdown
#' @export
#' @examples overview_univar(df = ggplot2::diamonds)
#'
#'
#'
view_univar <- function(df, ...) {


  # names of variables which are discrete and continuous, using DataExplorer
  dnames <-names(split_columns(df)$discrete)
  cnames <- names(split_columns(df)$continuous)

  # univariate distributions, densities and cumulative, for discrete and continuous variables-------------------
  # pdf's of numerical varaibles
  funModeling::plot_num(df, bins=10)
  #or
  plot_histogram(df)

  # Cumulative_df's of numerical variables
  # using points
  lapply(cnames, FUN=function(var) {
    ggplot(df, aes(df[[var]])) + stat_ecdf(geom = "point") +
      xlab(df[[var]])
  }
  )

  # pdf's of categorical variables
  # plot_bar(df, maxcat=400L)
  DataExplorer::plot_bar(df)

  # categorical ecdf not relevant :)


}

