
#' view_multivar function
#' multivariate analysis
#' @param df input data
#' ....other important comments
#'  
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
#' @examples overview_multivar(df = ggplot2::diamonds)
#' 
#'

view_multivar <- function(df, ...) {
  
  # -------- multivariate analysis ---------------------------------------
  
  # names of variables which are discrete and continuous, using funModeling
  dnames <-names(split_columns(df)$discrete)     
  cnames <- names(split_columns(df)$continuous)  
  
  
  ## dataExplorer style
  #correlationa
  plot_correlation(df)
  # principal components  
  plot_prcomp(df)
  
  # scatterplots, for each of the continuous variables  
  # select only one, randomly, since this is very slow
  seed=99
  random_feature <- sample(cnames, 1)
  DataExplorer::plot_scatterplot(df, by= random_feature )
  
  
  ### to generalize the following
  #ddf <- split_columns(df)$discrete
  #i <- 
  #j <- 
  #k <- 
  ## two categorical variables ------------------
  #ddf %>% data.frame()
  #count(i,j ) %>%
  #  ggplot(mapping= aes(x=i, y=j)) +
  #  geom_tile(mapping=aes(fill=n))
  
  # ggplot(data=df) +
  #   geom_count(mapping= aes(x=i, y=j))
  
  #ggplot(data=df) +
  #  geom_count(mapping= aes(x=i, y=j))  
  
  #ggplot(data=df) +
  #  geom_count(mapping= aes(x=i, y=k)) 
  
  #df %>% 
  #  count(j, i) %>%
  #  ggplot(mapping= aes(x=j, y=i)) +
  #  geom_tile(mapping=aes(fill=n))
  
  #df %>% 
  #  count(k, i) %>%
  #  ggplot(mapping= aes(x=k, y=i)) +
  #  geom_tile(mapping=aes(fill=n))
  
  # for correlations, using information theory, for categorical and numerical variables
  # from funModeling, although too slow
  # var_rank_info(df,j)  %>% kable() 
  
  ### to do ###-----------------------
  ## might add more on correlation based on information theory
  ## may use the algorithm called MINE; it gives MIC (between 0 and 1 where 1=perfect correlation), etc
  ###
  #### NOTE: converting all categorical into numerical (0 1nd 1):----------------
  #library(caret)
  #dummyVars("~.", data=someData)   ### this converts all which need to be 
  ### ----------------------------------------------------------------------------
  
  
  ## when input is a categ var, output is also categ, then we have a table of counts of input by output 
  #using coin package
    #df1 <- select(df, c(i, k)) 
    #xt <-xtabs(~i+k, data=df1) 
    #xtabs(~cut+clarity, df1)
    #spineplot(xt)
  ### may add:
  # more stratified cases (3 variables), i.e.  in the style
  # xtabs(X~Y|Z) # then  do
  # spineplot(xtabs())
  
  ##more graphs (multivariables)
  
  ##library(lattice)
  #histogram for two-way data  #### also for numerical
  #lattice::histogram(~ X | Y + Z,
  #          data=Data,
  #          layout=c(3,2)      #  
  #  )
  
  ##or 
  # library(graphics)
  #ex
  #pairs(data=df, ~ depth + price  )  ####  for numerical only
  
  ## or
  #library(vcd)  
  #mosaic(~i + j, data=df, shade=TRUE, legend=TRUE) 
  #mosaic(~Kyn + i, data=df, shade=TRUE, legend=TRUE)  
  
  
  
  #----------- then, also: ---------3.5.---
  # case 1. ### all variables versus a "target" which has only two values; gives counts and percentages   --------------
  # cross_plot(df, input=c("j", "k"), target="Kyn", auto_binning = FALSE)
  
  ## when no input, it should do all variables, but it did not work well
  
  # ---------------------------------3.6----------------------------
  # case 2. ### target has more than two categories, can be multi-class; input is numeric
  # plotar(data=df, input="j", target="i", plot_type = "histdens")
  # it works but not so easy to look at when too many categories of target
  
  # ------------------------3.7--------------------------------------------
  # case 3. for a set of numerical variables, we have a matrix of all possible combinations
  # may do corrplot()
  
  #glimpse(diamonds)
  #only numeric variables
  # diamonds %>% select(carat, price, depth) %>% cor() %>% corrplot(method="number")
  # M <- diamonds %>% select(carat, price, depth) %>% cor() 
  # MM <- M %>% kable()
  # res1 <- diamonds %>% select(carat, price, depth) %>% cor.mtest(conf.level=.95)
  #    corrplot(M,p.mat=res1$p, sig.level = .2)
  #
  
  #-------------------------------------------
  
  ### Note ---------------------------------------------------------------
  #For larger plots and numerical (*or transf into numerical), 
  #you might want to try the 
  #d3heatmap or heatmaply packages, which create interactive plots.
  ### as mentioned at from: r4ds.had.co.nz --------------------------------------
  
  #ex: for numerical variables only
  #ddiam <- diamonds %>% select(carat, price, depth)
  #x <- data.frame(ddiam)
  #x_cor <- cor(x)
  #x_cor %>% d3heatmap(k_row = 2, k_col = 2)
  # this is interactive indeed
  
  # ------------------------------------------------------------------------------- 
  
  
  # outliers --------***--------------------------------
  
  # multivariate outliers: see some distance based, like leverage, Cooks's:
  # need to do: model <- lm(Y~., data=someData) for example; then do:  cooks.distance(model);
  
  # can do boxplots for bivar
  # ------------------------------------------------------    
  
  # empirical cumulative distribution functions, even for multivariate case ------------
  
  #library(Hmisc)
  #pre.test <- rnorm(100, 50, 10)
  #post.test <- rnorm(100, 55, 10)
  #m <- data.frame(pre.test, post.test, sex=sample(c('male','female'),100,TRUE)) 
  ##for
  #e1 <- Ecdf(pre.test)
  #e1b <- Ecdf(post.test)
  ##or
  #e2 <- Ecdf(m, group=m$sex, datadensity='rug')
  
  # may do Trellis also...***
  
  # to do :managing plots
  #--------------------------------------------------------------------
  
}
