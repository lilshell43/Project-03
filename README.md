# Project-03

Packages required:

Install package to grab NBA data

devtools::install_github("abresler/nbastatR")

Install package for biplot

devtools::install_github("vqv/ggbiplot")

install.packages(c("plotly", "dplyr", "shiny", "shinythemes", "ggplot2", "caret", "randomForest"))

These 2 packages below might not be needed to run. When installing nbastatR package, had to delete these packages and reinstall.

install.packages("glue")

install.packages("digest")

R code to run code from github:

shiny::runGitHub("Project-03", "lilshell43", subdir = "ShinyApp")
