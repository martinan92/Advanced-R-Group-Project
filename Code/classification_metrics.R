source('Code/load_libraries.R')

# Taken from class codes on campus
accuracy <- function(data) {
  cf<-table(data$pred,data$obs)
  # Fix this to TRUE/FALSE later
  TP<-cf['yes','yes']
  FN<-cf['no','yes']
  TN<-cf['no','no']
  FP<-cf['yes','no']
  
  return((TP+TN)/(TN+FP+TP+FN))
}

classification_summary <- function(data, lev=NULL, model=NULL) {
  metrics <- c(twoClassSummary(data, lev=lev),
               precision(data=data$pred, reference=data$obs),
               accuracy(data=data)
  )
  
  names(metrics) <- c("AUC", "Sensitivity", "Specificity", "Precision", "Accuracy")
  
  return(metrics)
}
