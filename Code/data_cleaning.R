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



