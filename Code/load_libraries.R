load_reqs <- function(reqs) {
  for(pkg in reqs) {
    if (!(pkg %in% installed.packages())) { install.packages(pkg)}
    
    library(pkg, character.only = T)
  }
}

pkgs <- c("data.table", "lubridate", "ggplot2", "ggrepel", 
          "ggiraph", "plotly", "caret", "bit64", "DataExplorer", 
          'pROC', 'ISOweek', 'mltools', 'pROC', 'caret')

load_reqs(pkgs)

library(ggplot2)
options(scipen = 999)
theme_set(theme_minimal(base_size = 16))

sapply(paste0("./Code/utils/", list.files("./Code/utils")), source)