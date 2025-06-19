# When publishing app to Shiny.io Rcpp is sometimes not compatible.
# This script ensures that the latest version is installed and the application is publishable.
install.packages("Rcpp", type = "binary")