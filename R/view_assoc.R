#' view association rules
#'
#' @param df input data
#' ....other important comments
#'
#' @import tidyverse
#' @import magrittr
#' @import arules
#' @import DataExplorer
#' @import funModeling
#' @import ggplot2
#' @import rmarkdown
#' @examples view_assoc(df = datasets::iris)
#' @export

#' this can be used for rule discovery!!!

view_assoc <- function(df, ...) {

# reading
# http://r-statistics.co/Association-Mining-With-R.html
#main package used by this function:
#(citation("arules"))

# when data is a data.frame, but we need a transaction-style,
# in order for the arules package to work:
# we transform it into the required format
library(arules)  # must do this for the next line to work !
tdata <- as(df, "transactions")
#method 1 of clustering: eclat algorithm
eclat_res <- inspect(eclat(tdata, parameter = list(supp=0.07, maxlen=15)))
eclat_plot <- itemFrequencyPlot(tdata, topN=10, type="absolute", main="item freguency")
summary_data <- summary(tdata)
eclat_summary <- summary(eclat(tdata, parameter = list(supp=0.07, maxlen=15)))
#method 2 of clustering: apriori algorithm
rules <- apriori(tdata)
apriori_summary <- summary(rules)
apriori_res <- inspect(rules)
#look at subsets
#r1 <- subset(rules, subset=rhs %in% "Species=setosa")
#inspect(head(r1, n=3, by="confidence"))

clusters_found <- list(eclat_res, apriori_res, eclat_plot)
return(clusters_found)
}
