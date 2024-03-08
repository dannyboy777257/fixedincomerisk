


p <- c("shiny",
       "tidyverse",
       "DT",
       "lubridate",
       "bslib",
       "tidyquant",
       "GGally",
       "grDevices",
       "plotly",
       "Rcpp",
       "stats")
new.packages <- p[!(p %in% installed.packages()[, "Package"])]
if (length(new.packages)) {
  install.packages(new.packages, dependencies = TRUE)
}
