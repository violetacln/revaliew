
#' view_univar function
#' univariate analysis: distribution functions and outliers
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
  #or with lines
  #ggplot(df, aes(depth)) + stat_ecdf(geom = "step")


  # pdf's of categorical variables
  # plot_bar(df, maxcat=400L)
  DataExplorer::plot_bar(df)




  ##lapply(dnames, FUN = function(x) {
  ##   x <- df$cut
  #    df %>%
  #    mutate(cut = cut %>% fct_infreq() %>% fct_rev()) %>%
  #             ggplot(aes(cut))+
  #             geom_bar()
  ##  }
  ##  )

  # categorical ecdf not relevant :)


}

