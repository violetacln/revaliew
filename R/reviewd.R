#' fast reporting results function
#' calling all functions or as needed
#' @param df input data
#' ....other important comments
#'
#' @examples reviewd(df = ggplot2::diamonds)
#' @export

reviewd <- function(df, ...) {

  getwd()
  rmarkdown::render("rmd_reports/report_example.Rmd")

}
