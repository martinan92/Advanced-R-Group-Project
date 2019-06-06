evaluate_model <- function(model, holdout, model_name) {

  cv_results <- model$results[model$results$Sensitivity==max(model$results$Sensitivity),
                              c("AUC", "Sensitivity", "Specificity", "Precision", "Accuracy")]
  
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
  
  model_name <- model_name
  names(model_name) <- "model_name"
  
  best_params <- paste(paste(names(model$bestTune), sep=":", model$bestTune), collapse=", ")
  names(best_params) <- "best_params"
  
  
  results <- c(model_name, holdout_results, confusion, cv_results, best_params)
  
  return(results)
}