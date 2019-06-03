library("mltools")
library("data.table")


# This will need edits 
make_predictions <- function(df, preprocessing=c(), feature_creation=c(), train_obj) {
  df <- data.table(df)

  
  return(predict(train_obj, df))
}

preds <- make_predictions(bank_test, train_obj=rf_baseline)