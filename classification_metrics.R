# Taken from class codes on campus

library(pROC)
library(caret)


classification_summary <- function(data, lev=c("yes", "no"), model=NULL) {
  metrics <- c(precision(data=data$pred, reference=data$obs),
               recall(data=data$pred, reference=data$obs),
               defaultSummary(data, lev)
               )
  
  names(metrics) <- c("Precision", "Recall", "Accuracy", "Kappa")
  
  return(metrics)
}




