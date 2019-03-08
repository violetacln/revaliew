
#' view_multivar function
#' multivariate analysis and views: pairwise plots, correlations, cumulative distributions
#' @param df input data
#' ....other important comments
#' @import GGally
#' @import tidyverse
#' @import magrittr
#' @import DataExplorer
#' @import funModeling
#' @import ggplot2
#' @import rmarkdown
#' @import d3heatmap
#' @import lattice
#' @import graphics
#' @import coin
#' @export
#' @examples overview_multivar(df = ggplot2::diamonds)
#'
#'

view_multivar <- function(df, ...) {

#-------- multivariate analysis ---------------------------------------

#---------- pairwise plots for all variables -------


# names of variables which are discrete and continuous, using funModeling
dnames <- names(DataExplorer::split_columns(df)$discrete)
cnames <- names(DataExplorer::split_columns(df)$continuous)

theme_set(theme_bw())


# it creates several pages of plots
# thin it out:
if (nrow(df) > 50000) {  df = df[sample(rownames(df), size=50), ]  }
                    ### set this as on option whih could be adjusted
lapply(dnames, FUN=function(var) {
    GGally::ggpairs(
                  df
                  #select(df,cnames)
                 ,aes(
                        colour = df[[var]]
                      )
                  )
                                  }
  )


## ------ most general correlation plots: for any type of var

DataExplorer::plot_correlation(data=df, type = c("all", "discrete", "continuous"),
          maxcat = 20L, cor_args = list(),
          title = NULL,
          ggtheme = theme_gray(),
          theme_config = list(legend.position ="bottom",
          axis.text.x = element_text(angle=90)))


#-----------------------------



  #---Notes ---------------------------------------------------------------
  #For larger plots and numerical (*or transf into numerical),
  #one might want to try the
  #d3heatmap or heatmaply packages, which create interactive plots.
  ### as mentioned at from: r4ds.had.co.nz --------------------------------------

  # empirical cumulative distribution functions, even for multivariate case ------------
  #library(Hmisc)
  # Ecdf()

  # may do Trellis plots also...
  #--------------------------------------------------------------------

}
