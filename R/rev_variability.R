#' rev_variability function for general data sets
#' @param df input data
#' ...other important comments
#' @import boot
#' @import DataExplorer
#' @import SimMultiCorrData
#' @import funModeling
#' @export
#' @examples rev_variability(df=ggplot2::diamonds)
#'


rev_variability <- function(df, ...) {

dnames <- names(DataExplorer::split_columns(df)$discrete)
cnames <- names(DataExplorer::split_columns(df)$continuous)

#-------continuous variables -----------------------
print("mean, sd, skewness, standardised kurtosis and standardised 5th and 6th cumulants are
           calculated for continuous variables, by using the package SimMultiCorrData ")

c_res <- c("var1","", "", "", "", "", "")
var1 <- character()
for (var1 in cnames){
    c_res <- cbind(c_res, c(var1,SimMultiCorrData::calc_moments(df[[var1]])))
     }

# should be able to bootstrap when needed --------- to correct this: ---
#set.seed(111)
#res1 <- lapply(cnames, FUN=function(vv){
#        stats <- function(df, i) {
#            c(mean(df[[vv]][i])
#            ,median(dff[[vv]][i]), sd(dff[[vv]][i]),
#            moments::skewness(dff[[vv]][i])
#            )
#            # to introduce naming of these dimensions for reporting
#            }
#                                        }
#              )

#"boot" requires big memory, so we take few replicates only -->> to make an option
    # res2 <- boot::boot(data=df,statistic=stats, R=1000)
    #    res2plot <- lapply(c(1:length(stats)), FUN=function(i) {
    #                plot(res2, index=i)
    #                boot::boot.ci(res2, type="bca", index=i)
    #
    #                })
    #   })


#-------------- categorical variables-------------------------------

# use funModeling and information_theory measures, all:
# entropy (en), mutual information (mi), information gain (ig), gain ratio (gr)

print("information measures for discrete variables inter-variability are calculated by using the package funModeling ")

d_res <- c("var1","var2","","","", "")
var1 <- character()
var2 <- character()
for (var1 in dnames){
  for (var2 in dnames)
    {
    d_res <- cbind(d_res, c(var1, var2,funModeling::infor_magic(input = df[[var1]],target =df[[var2]])))
    }
}

variability <- list(d_res, c_res)

return(variability)


}
