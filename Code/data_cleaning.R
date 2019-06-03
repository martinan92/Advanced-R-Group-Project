fix_types <- function(df){
  
  df1 <- data.table(df)
  
  ## fix data type
  df1[ , names(df1)[sapply(df1, is.character)]:=
         lapply(.SD,as.factor),.SDcols = 
         names(df1)[sapply(df1, is.character)]]
  
  df1[ , names(df1)[sapply(df1, is.integer)]:=
         lapply(.SD,as.numeric),.SDcols = 
         names(df1)[sapply(df1, is.integer)]]
  
  df1$day <- as.factor(df1$day)
  
  return(df1)    
  
}

scale_df <- function(df){
  df1 <- data.table(df)
  
  numeric_vars <- names(df1)[sapply(df1, is.numeric)]
  
  ## Scale 
  df1[, (numeric_vars) := lapply(.SD, scale), .SDcols=numeric_vars]
  
  return(df1)  
  
}



