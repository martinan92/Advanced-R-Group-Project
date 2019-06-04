evaluate_model <- function(model, holdout, model_name) {
  
  cv_results <- model$results[c("AUC", "Sensitivity", "Specificity", "Precision", "Accuracy")]
  names(cv_results) <- paste0("cv_", c("AUC", "Sensitivity", "Specificity", "Precision", "Accuracy"))

  preds <- predict(model, holdout)
  
  holdout_results <- c(
    sensitivity(data=preds, reference=factor(holdout$y, levels=c("yes", "no")),positive="yes"),
    specificity(data=preds, reference=factor(holdout$y, levels=c("yes", "no")),positive="yes"),
    precision(data=preds, reference=factor(holdout$y, levels=c("yes", "no")),positive="yes"),
    accuracy(data=data.frame(pred=preds, obs=holdout$y))
  )
  
  names(holdout_results) <- paste0("holdout_", c("Sensitivity", "Specificity", "Precision", "Accuracy"))

  cf <- table(preds, holdout$y)
  TP <- cf['yes','yes']
  FN <- cf['no','yes']
  TN <- cf['no','no']
  FP <- cf['yes','no']
  
  confusion <- c(TP, FP, FN, TN)
  names(confusion) <- paste0("holdout_", c("TP", "FP", "FN", "TN"))
  
  model_name <- c(model_name)
  names(model_name) <- c("model_name")
  
  results <- c(model_name, holdout_results, cv_results)
  
  return(results)
}