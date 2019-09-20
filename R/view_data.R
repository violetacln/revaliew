
#' view_data function
#' short description of data: only summaries and summarizing plots
#' @param df input data
#' ....other important comments
#'
#' @import tidyverse
#' @import magrittr
#' @import DataExplorer
#' @import ggplot2
#' @import rmarkdown
#' @import tabplot
#' @import ff
#' @examples view_data(df = ggplot2::diamonds)
#' @export

view_data <- function(df, ...) {

  # useful names of variables: discrete and continuous
  dnames <-names(DataExplorer::split_columns(df)$discrete)
  cnames <- names(DataExplorer::split_columns(df)$continuous)

  #short overview
   description <- Hmisc::describe(df)
 # data univariate plots

  p1 <- DataExplorer::plot_intro(df)

  p2 <- DataExplorer::plot_missing(df)

  p3 <- DataExplorer::plot_bar(df)

  p4 <- DataExplorer::plot_histogram(df)


  # table plots from tabplot package:

 if (nrow(df) > 10000) {  df = df[sample(rownames(df), size=10000), ]  }    ### set this as on option whih could be adjusted

  plot_list <-
  lapply(cnames, FUN=function(x0) {
   tabplot::tableplot(dat=df, sortCol=x0)$plot
  }
  )

  plots_all <- list(p1,p2,p3,p4,plot_list)

#--------------------------------------------------

  return(list(description, plots_all))

}


