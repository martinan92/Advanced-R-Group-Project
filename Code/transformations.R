drop_variables <- function(df){
  df_sub <- df[, -c('duration', 'pdays', 'month', 'day')]
  
  return(df_sub)
}

drop_day <- function(df) {
  df_sub <- df[, -c('day')]
  
  return(df_sub)
  
}

drop_pdays <- function(df) {
  df_sub <- df[, -c('pdays')]
  
  return(df_sub)
  
}

drop_duration <- function(df) {
  df_sub <- df[, -c('duration')]
  
  return(df_sub)
  
}

create_weekday <- function(df){
  df1 <- data.table(df)
  
  df1$month <- factor(df1$month, levels = c("jan", "feb", "mar", 
                                            "apr", "may", "jun", 
                                            "jul", "aug", "sep", 
                                            "oct", "nov", "dec"))
  
  df1$month_lev <- as.integer(df1$month)
  
  
  df1$date <- ISOdate(2014, df1$month_lev , df1$day)
  
  df1$weekday <- ISOweekday(df1$date)
  
  return(df1[,-c('month_lev', 'date')])
  
}

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
  
  df1$y <- factor(df1$y, levels=c("yes", "no"))
  
  return(df1)    
  
}

scale_df <- function(df){
  df1 <- data.table(df)
  
  numeric_vars <- names(df1)[sapply(df1, is.numeric)]
  
  ## Scale 
  df1[, (numeric_vars) := lapply(.SD, scale), .SDcols=numeric_vars]
  
  return(df1)  
  
}

total_contacts <- function(df){
  df1 <- data.table(df)
  
  df1$total_contact <- df1$campaign + df1$previous
  
  return(df1[,-c('campaign', 'previous')])
}

clustering <- function(df_train, df_test, n_clusters){
  
  df_train$train <- 1
  df_test$train <- 0
  
  df1 <- rbind(df_train, df_test, fill = TRUE)
  
  if(is.null(df1$total_contact)) {
    df1 <- total_contacts(df1)
  }
  
  set.seed(1912)
  clusters <- kmeans(scale(df1[,c("age", 'balance', 'total_contact')]),n_clusters)
  
  df1$cluster <- as.factor(clusters$cluster)
  
  df_train <- df1[df1$train == 1, -'train']
  df_test <- df1[df1$train == 0, -'train']
  
  return(list("train" = df_train, "test" = df_test))
  
}





