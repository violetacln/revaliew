#' rev_variability function for general data sets
#' @param df input data
#' ...other important comments
#' @import boot
#' @import DataExplorer
#' @export
#' @examples rev_variability(df=ggplot::diamonds)
#' the main resource for this function is the package boot
citation("boot")
#' the basic methods of resampling with replacement, paramteric and nonparametric, are general enough to cover
#' main statistics for general data sets and basic modeling formulae
#' with confidence intervals and corresponding plots
#' for details one is referred to the boot package manual

rev_variability <- function(df, ...) {


#---------------------- continuous variables
dnames <- names(DataExplorer::split_columns(df)$discrete)
cnames <- names(DataExplorer::split_columns(df)$continuous)


# could also use : moments and cumulants of continuous variables, up to 6th order
# as in
# SimMultiCorrData::calc_moments()


set.seed(111)
lapply(cnames, FUN=function(var) {
        stats <- c(mean(var),median(var), sd(var), skewness(var))
                        # to introduce naming of these dimensions for reporting
        res <- boot::boot(data=df,statistics=stats, R=1000)
        lapply(c(1:length(stats)), FUN=function(i) {
                    plot(res, index=i)
                    boot.ci(res, type="bca", index=i)

                    })

       })

#---------------------- categorical variables
# use Shannon's entropy for example as a measure
# see github---- funModeling---information_theory.R functions


}
