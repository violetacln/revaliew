
# it works!

# outliers, univariate -----------------------------------------------
# using Tukey
lapply(cnames, FUN=function(x0) {
  c(
    x0,
    funModeling::tukey_outlier(as.data.frame(df)[[x0]])
  )
}
)

# using Hampel
lapply(cnames, FUN=function(x0) {
  c(
    x0,
    funModeling::hampel_outlier(df[[x0]])
  )
}
)

#--- more univariate

# qqplots ----------------------------------------------------------------
DataExplorer::plot_qq(df)

# boxplots ----------------------------------------------------------------
#
lapply(dnames, FUN=function(x) {
  DataExplorer::plot_boxplot(df, x )
}
)

##to add more tests for outliers ------------------***


# see some distance based, like leverage, Cooks's dist, depth function, etc

# can do boxplots for bivar ....


###-------- multivariate outlier detection-------------***
# use depth function
#.....
#---------------
