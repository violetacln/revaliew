

#' view_outliers function
#' short description of data: plots and scores for outlier detection
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
#' @import outliers
#'
#' @export
#' @examples view_data(df = ggplot2::diamonds)
#'
#'

view_outliers <- function(df, ...) {


# names of variables which are discrete and continuous, using DataExplorer
dnames <-names(split_columns(df)$discrete)
cnames <- names(split_columns(df)$continuous)


# outliers, univariate -----------------------------------------------
# using Tukey
lapply(cnames, FUN=function(x0) {
  c(
    x0,
    funModeling::tukey_outlier(as.data.frame(df)[[x0]])
  )
}
)

# using Hampel
lapply(cnames, FUN=function(x0) {
  c(
    x0,
    funModeling::hampel_outlier(df[[x0]])
  )
}
)

#--- more univariate: package O3
#....


# qqplots ----------------------------------------------------------------
DataExplorer::plot_qq(df)

# boxplots ----------------------------------------------------------------
#
lapply(dnames, FUN=function(var) {
  DataExplorer::plot_boxplot( df, df[[var]] )
  }
      )

##more tests for outliers ------------ to create option for prob ***
lapply(cnames, FUN=function(x0){

  outliers::scores(x0, type="z", prob=0.95)

  outliers::scores(x0, type="z", prob=0.95)

  outliers::scores(x0, type="z", prob=0.95)
                             }
      )

#----- comparing many maethods, using package O3 ---------
O3m <- O3prep(df, method=c("HDo", "PCS"))
O3m1 <- O3plotM(O3m)
grid.arrange(O3m1$gO3, O3m1$gpcp, ncol=1)



#--- to add more tests ***

###-------- multivariate outlier detection-------------***
# use depth function or other distance functions, modeling
#.....
#---------------

}
