#' view_clusters function
#'
#' @param df input data
#' ....other important comments
#'
#' @import tidyverse
#' @import magrittr
#' @import DataExplorer
#' @import funModeling
#' @import ggplot2
#' @import rmarkdown
#'
#' @import factoextra
#' @import cluster
#' @examples view_clusters(df = iris[,1:4])
#' @export

view_clusters <- function(df) {

  print("finding clusters in data")

  #example, testing factoextra package
  # find optimal number of clusters
  dff <- scale(df)
  factoextra::fviz_nbclust(dff, kmeans, method = "gap_stat")
  # compute and visualise
  set.seed(123)
  km.res <- kmeans(dff, 3, nstart = 25)
  # Visualize
  plot_kmeans <- factoextra::fviz_cluster(km.res, data = dff,
               ellipse.type = "convex",
               palette = "jco",
               repel = TRUE,
               ggtheme = ggplot2::theme_minimal())
  # compare with PAM clustering
  # Compute PAM
  pam.res <- cluster::pam(dff, 3)
  # Visualize
  plot_pam <- factoextra::fviz_cluster(pam.res)

clusters2methods <- list(plot_kmeans, plot_pam)
return(clusters2methods)

}
