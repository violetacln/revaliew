#' rev_variability function for general data sets
#' @param df input data
#' ...other important comments
#' @import boot
#' @import DataExplorer
#' @export
#' @examples rev_variability(df=ggplot2::diamonds)
#' the main resource for this function is the package boot \code(citation("boot"))
#' the basic methods of resampling with replacement, paramteric and nonparametric, are general enough to cover
#' main statistics for general data sets and basic modeling formulae
#' with confidence intervals and corresponding plots
#' for details one is referred to the boot package manual

rev_variability <- function(df, R) {

dnames <- names(DataExplorer::split_columns(df)$discrete)
cnames <- names(DataExplorer::split_columns(df)$continuous)


#---------------------- continuous variables
set.seed(111)
lapply(cnames, FUN=function(vv) {

    stats <- function(dff, i) {
    c(mean(dff[[vv]][i]),median(dff[[vv]][i]), sd(dff[[vv]][i]),
      moments::skewness(dff[[vv]][i]) )
    # to introduce naming of these dimensions for reporting
    }
     # requires big memory, so we tke few replicates here
        res <- boot::boot(data=df,statistic=stats, R=100)
        lapply(c(1:length(stats)), FUN=function(i) {
                    plot(res, index=i)
                    boot::boot.ci(res, type="bca", index=i)

                    })
       })

# note:
# could also use : moments and cumulants of continuous variables, up to 6th order
# as in
# SimMultiCorrData::calc_moments()


#---------------------- categorical variables
# use funModeling and information_theory measures, all:
# entropy (en), mutual information (mi), information gain (ig), gain ratio (gr)

d_rec <- c()
var1 <- character()
var2 <- character()
for (var1 in dnames){
  for (var2 in dnames)
    {
    d_res <- rbind(var1, var2,funModeling::infor_magic(input = df[[var1]],target =df[[var2]]))
    }
}
d_res

# super!


}
