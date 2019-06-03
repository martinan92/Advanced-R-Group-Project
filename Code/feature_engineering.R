scale_df <- function(df){
  df1 <- data.table(df)
  
  numeric_vars <- names(df1)[sapply(df1, is.numeric)]
  
  ## Scale 
  df1[, (numeric_vars) := lapply(.SD, scale), .SDcols=numeric_vars]
  
  return(df1)  
  
}

drop_variables <- function(df){
  df_sub <- df[, -c('duration', 'pdays', 'month', 'day')]
  
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
  df1$weekday <- as.factor(df1$weekday)
  
  return(df1[,-c('month_lev', 'date')])
  
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
  
  set.seed(1912)
  clusters <- kmeans(scale(df1[,c("age", 'balance', 'total_contact')]),n_clusters)
  
  df1$cluster <- as.factor(clusters$cluster)
  
  df_train <- df1[df1$train == 1, -'train']
  df_test <- df1[df1$train == 0, -'train']
  
  return(list("train" = df_train, "test" = df_test))
  
}


IQR.outliers <- function(x,iqr = 1.5) {
  Q3<-quantile(x,0.75)
  Q1<-quantile(x,0.25)
  IQR<-(Q3-Q1)
  left<- (Q1-(iqr*IQR))
  right<- (Q3+(iqr*IQR))
  return(c(left,right))
}

remove_outliers <- function(df, n = 1.5){
  df1 <- data.table(df)
  numeric_vars <- names(df1)[sapply(df1, is.numeric)]
  
  for(var in numeric_vars){
    lim <- IQR.outliers(df1[,get(var)], n)
    df1[get(var) < lim[1], var] <- lim[1]
    df1[get(var) > lim[2], var] <- lim[2]
  }
  
  return(df1)  
  
}


