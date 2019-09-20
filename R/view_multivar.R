
#' view_multivar function
#' multivariate analysis and views: pairwise plots, correlations, cumulative distributions
#' @param df input data
#' ....other important comments
#' @import GGally
#' @import DataExplorer
#' @import ggplot2
#' @import rmarkdown
#' @examples view_multivar(df = ggplot2::diamonds)
#' @export


view_multivar <- function(df, ...) {

#------1. pairwise plots for all variables -----------

# names of variables which are discrete and continuous
dnames <- names(DataExplorer::split_columns(df)$discrete)
cnames <- names(DataExplorer::split_columns(df)$continuous)
ggplot2::theme_set(ggplot2::theme_bw())

# thin it out:
if (nrow(df) > 10000) {  df = df[sample(rownames(df), size=10000), ]  }
### to set this as on option whih could be adjusted

plots_discr <- lapply(dnames, FUN=function(var) {
                GGally::ggpairs(
                  df,
                  ggplot2::aes(
                        colour = df[[var]],
                        cardinality_threshold=50
                        )
                  )
                                  }
  )


## ---2. general correlation plots: for any type of variables

plot_correl <- DataExplorer::plot_correlation(data=df,
          type = c("all"),
          maxcat = 50L, cor_args = list(),
          title = NULL,
          ggtheme = ggplot2::theme_gray(),
          theme_config = list(legend.position ="bottom",
          axis.text.x = ggplot2::element_text(angle=90)))

#-----------------------------

plots_multivar <- list(plots_discr, plot_correl)

return(plots_multivar)

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
