# load_reqs <- function(reqs) {
#   for(pkg in reqs) {
#     if (!(pkg %in% installed.packages())) { install.packages(pkg)}
#     
#     library(pkg, character.only = T)
#   }
# }
# 
# # pkgs <- c("data.table", "lubridate", "ggplot2", "ggrepel", 
# #           "ggiraph", "plotly", "caret", "bit64", "DataExplorer", 
# #           'pROC', 'ISOweek', 'mltools', 'pROC', 'caret')
# 
# pkgs <- c("data.table", "lubridate", "ggplot2", "ggrepel", 
#           "plotly", "caret", "bit64", "DataExplorer", 
#           'pROC', 'ISOweek', 'mltools', 'pROC', 'caret')
# load_reqs(pkgs)

if(!"pacman" %in% installed.packages()) {install.packages("pacman")}
pacman::p_load("data.table", "lubridate", "ggplot2", "ggrepel", 
                         "plotly", "caret", "bit64", "DataExplorer", 
                         'pROC', 'ISOweek', 'mltools', 'pROC', 'caret',
                          "DMwR", "knitr")

library(ggplot2)
options(scipen = 999)
theme_set(theme_minimal(base_size = 16))

sapply(paste0("./Code/", list.files("./Code/")), source)