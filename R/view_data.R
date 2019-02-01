
#' view_data function
#' short description of data: summaries and plots
#' @param df input data
#' ....other important comments
#'  
#' @import tidyverse
#' @import magrittr
#' @import DataExplorer
#' @import funModeling
#' @import ggplot2
#' @import  rmarkdown 
#' @import  tabplot
#' @examples overview_data(df = ggplot2::diamonds)
#' 
#'

view_data <- function(df, ...) {
  
  # make sure characters as factors 
  df %<>% mutate_if(is.character, as.factor)   
  
  #we may have other factors, too, represented by discrete values
  
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
  
  ###-------- to do --------------------------------------------------------------------------------------
  ### remove some features since too much missing data ***, ex
  # dat[, -which(colMeans(is.na(dat)) > 0.5)]
  #
  ### regroup some features since too many categories ***
  #***********************
  ### reorder categories by some continuous variable when interesting ...
  #-----------------------------------------------------------------------
  
  DataExplorer::plot_intro(df)  
  
  DataExplorer::plot_missing(df)  
  
  # table plots from tabplot package
  plot_list <- lapply(cnames, FUN=function(x) {
    tableplot(dat=df, sortCol=x)
  }
  )
  
  
  ### to do:  save all these plots into a unique report file
  
  #------------------------------------------------------------------------
  
}
