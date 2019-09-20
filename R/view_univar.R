
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
#' @examples view_univar(df = ggplot2::diamonds)
#' @export

view_univar <- function(df, ...) {

  # names of variables which are discrete and continuous, using DataExplorer
  dnames <-names(DataExplorer::split_columns(df)$discrete)
  cnames <- names(DataExplorer::split_columns(df)$continuous)

  # marginal distributions are already obtained with view_data()

  # Cumulative_df's of numerical variables
  # using points
  plots_cumulative <- lapply(cnames, FUN=function(var) {
    ggplot2::ggplot(df, ggplot2::aes(df[[var]])) +
      ggplot2::stat_ecdf(geom = "point") +
      ggplot2::xlab(var) +
      ggplot2::ylab("cumulative prob")
  }
  )

  #----------------------
 return(plots_cumulative)

}

