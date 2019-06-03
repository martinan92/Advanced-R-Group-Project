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
  df1$weekday <- as.factor(df1$weekday)
  
  df1$y_num <- FALSE
  
  df1[df1$y == 'yes', 'y_num'] <- TRUE
  
  df1$y <- NULL
    
  colnames(df1)[which(names(df1) == "y_num")] <- "y"
  
  return(df1)    
  
}

scale_df <- function(df){
  df1 <- data.table(df)
  
  numeric_vars <- names(df1)[sapply(df1, is.numeric)]
  
  ## Scale 
  df1[, (numeric_vars) := lapply(.SD, scale), .SDcols=numeric_vars]
  
  return(df1)  
  
}



