library("data.table")
library("caret")

pipeline_casero <- function(df, preprocessing=c(), creation=c(), selection=c(), 
                            model="ranger", tunegrid=NULL, tunelength=10, k=3) {
  df <- data.table(df)
  
  print(paste("Beginning preprocessing step at", Sys.time()))
  for (f in preprocessing) {
    df <- f(df)
  }
  
  print(paste("Beginning feature creation step at", Sys.time()))
  for (f in creation) {
    df <- f(df)
  }
  
  print(paste("Beginning feature selection step at", Sys.time()))
  for(f in selection) {
    df <- f(df)
  }
  
  tc <- trainControl(method="cv", number=k, search="random")
  
  print(paste("Beginning modeling step at", Sys.time()))
  trained <- train(y~.,
                   df,
                   method=model,
                   metric="Accuracy",
                   maximize=FALSE,
                   trControl=tc,
                   tuneGrid=tunegrid)
  
  print(paste("Finished!", Sys.time()))
  
  return(trained)
}


rf_baseline <- pipeline_casero(bank_train)

saveRDS(rf_baseline, "./saved_models/rf_baseline.rds")


