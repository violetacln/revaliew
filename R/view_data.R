
#' view_data function
#' short description of data: only summaries and summarizing plots
#' @param df input data
#' ....other important comments
#'
#' @import tidyverse
#' @import magrittr
#' @import DataExplorer
#' @import funModeling
#' @import ggplot2
#' @import rmarkdown
#' @import tabplot
#' @import skimr
#' @import ff
#' @export
#' @examples view_data(df = ggplot2::diamonds)
#'
#'

view_data <- function(df, ...) {

  # make sure characters as factors
   df %<>% dplyr::mutate_if(is.character, as.factor)

  #shortest overview
  funModeling::df_status(df)
  #few more details
  skim(df) %>% skimr::kable()

  # continuous variables
  # using funModeling
  funModeling::profiling_num(df)

  # discrete
  # using dataExplorer
  DataExplorer::plot_bar(df)

  # from dataExplorer; useful names of variables: discrete and continuous
  dnames <-names(split_columns(df)$discrete)
  cnames <- names(split_columns(df)$continuous)

  DataExplorer::plot_intro(df)

  DataExplorer::plot_missing(df)



  # table plots from tabplot package
    # make ff package work correctly:
    # with: set options(fftempdir = "path/to/your/folder") to a folder where you have access to
  #getOption("fftempdir")
    ##ff:setOptions("fftempdir" = getwd())
  if (nrow(df) > 50000) {  df = df[sample(rownames(df), size=50), ]  }    ### set this as on option whih could be adjusted

  plot_list <-
  lapply(cnames, FUN=function(x0) {
    tabplot::tableplot(dat=df, sortCol=df[[x0]])
  }
  )

#------------------------------------------------------------------------

}
