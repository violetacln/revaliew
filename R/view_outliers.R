
#' view_outliers function
#' plots and scores for outlier detection
#' @param df input data
#' ....other important comments
#'
#' @import tidyverse
#' @import ggplot2
#' @import rmarkdown
#' @import DataExplorer
#' @import funModeling
#' @import outliers
#' @import DDoutlier
#' @examples view_outliers(df = datasets::iris[,1:4])
#' @export

view_outliers <- function(df, ...) {

# names of variables which are discrete and continuous, using DataExplorer
dnames <-names(DataExplorer::split_columns(df)$discrete)
cnames <- names(DataExplorer::split_columns(df)$continuous)

# qqplots of continuous variables ------------------
outliers_cont <- DataExplorer::plot_qq(df)

# boxplots by each discrete -----------------
outliers_by_Discretes <- lapply(dnames, FUN=function(varr) {
    DataExplorer::plot_boxplot( df, by=varr , geom_boxplot_args = list("outlier.color"="red"))
    }
    )

# univar limits, using Tukey (interquartiles)
outliers_table_Tukey <-
  knitr::kable(
  lapply(cnames, FUN=function(x0) {
  c(
    x0,
    funModeling::tukey_outlier(as.data.frame(df)[[x0]])
  )
}
)
#, format="markdown"
, col.names = " ", caption="Interquartiles based: Tukey method"
)

# univar limits, using Hampel (median based )
outliers_table_Hampel <-
   knitr::kable(
   lapply(cnames, FUN=function(x0) {

    c( x0, funModeling::hampel_outlier(df[[x0]]) )
  }
  )
  #, format="markdown"
  , col.names = " ", caption="Median based: Hampel"
)



#--- one score for each record, for each variable, as in package outliers
#lapply(cnames, FUN=function(x0){
#  which(
#    outliers::scores(df[[x0]], type="t") == TRUE
#  )
  ## outliers::scores(df[[x0]], type="z")
  ## outliers::scores(df[[x0]], type="iqr")
#}
#)

#----- comparing many methods, using package "OutliersO3" ---------
# it works on continuous variables
# comparing two methods
# could add four more, at least: "BAC", "adjOut", "DDC, "MCD"
# but this is rather slow even with two
#O3m <- OutliersO3::O3prep(df[cnames], method=c("HDo", "PCS"))
#O3m1 <- OutliersO3::O3plotM(O3m)
# gridExtra::grid.arrange(O3m1$gO3, O3m1$gpcp, ncol=1)
##rm(O3m)
##rm(O3m1)


#---------------multivariate outlier detection-------------------------
# use depth function or other distance functions, modeling
#

# --new: v1-- testing "dobin" package of ------------
# https://github.com/sevvandi/dobin
# based on the paper:
# https://robjhyndman.com/papers/dobin.pdf
#set.seed(1)
#X <- data.frame(
#    x1 = c(rnorm(400, mean=5), rnorm(5,mean=0, sd=0.2), rnorm(400, mean=-5)),
#    x2 = rnorm(805),
#    x3 = rnorm(805),
#    x4 = rnorm(805),
#    x5 = rnorm(805),
#    x6 = rnorm(805)
#   )
#labs <- c(rep("Norm", 400), rep("Out", 5), rep("Norm", 400))
#out <- dobin::dobin(X)
#XX <- cbind.data.frame(out$coords[ , 1:2], as.factor(labs))
#colnames(XX) <- c("DC1", "DC2", "labs")
#ggplot2::ggplot(XX, ggplot2::aes(DC1, DC2, color=labs)) +
#      ggplot2::geom_point() +
#       ggplot2::theme_bw()


#---v2---- testing DDoutlier ------------
# https://cran.r-project.org/web/packages/DDoutlier/index.html
#

# dataset
X <- df
# Connectivity based outlier detection, by setting an optional k
outlier_score <- DDoutlier::COF(dataset=X, k=8)
# Sort and find index for most outlying observations
names(outlier_score) <- 1:nrow(X)
sort(outlier_score, decreasing = TRUE)
# Inspect the distribution of outlierscores
DD_hist <- hist(outlier_score)
# Classify observations (outlier detection based on distance calculations)
Distance_Based_classif <- cls_observations <- DDoutlier::DB(dataset=X, d=1, fraction=0.05)$classification
# Remove outliers from dataset if you really want to
#XX <- X[cls_observations=='Inlier',]

# should also investigate the non-parametric bootstrap procedure based on:
# the bootlier test

# should also investigate the "extended isolated forest" algorithm for detecting anomalous patterns in data
# this is more than outliers ***

#---------------

outliers_all <- list(outliers_cont, outliers_by_Discretes,
                    outliers_table_Tukey, outliers_table_Hampel,
                    DD_hist, Distance_Based_classif  )

return(outliers_all)

}
