source('Scripts/load_libraries.R')

##### Data Loading ######

raw_data_train <-fread('Data/BankCamp_train.csv', stringsAsFactors = F)
raw_data_test <-fread('Data/BankCamp_test.csv', stringsAsFactors = F)

str(raw_data_train)

##### Data Exploration #####

summary(raw_data_train)
introduce(raw_data_train)
plot_intro(raw_data_train)

plot_intro(raw_data_test)

plot_density(train_data)
plot_boxplot(train_data, by = "y")

plot_bar(raw_data_train)

plot_correlation(raw_data_train, type = "d")

###### Data Transformation ######


fix_types <- function(df){
      
  df1 <- data.table(df)
  
  ## fix data type
  df1[ , names(df1)[sapply(df1, is.character)]:=
                   lapply(.SD,as.factor),.SDcols = 
                   names(df1)[sapply(df1, is.character)]]
  
  df1[ , names(df1)[sapply(df1, is.integer)]:=
               lapply(.SD,as.numeric),.SDcols = 
               names(df1)[sapply(df1, is.integer)]]
  
  return(df1)    
  
}

scale_df <- function(df){
  df1 <- data.table(df)
  
  numeric_vars <- names(df1)[sapply(df1, is.numeric)]
  
  ## Scale 
  df1[, (numeric_vars) := lapply(.SD, scale), .SDcols=numeric_vars]
  
  return(df1)  
  
}

IQR.outliers <- function(x,iqr) {
  Q3<-quantile(x,0.75)
  Q1<-quantile(x,0.25)
  IQR<-(Q3-Q1)
  left<- (Q1-(iqr*IQR))
  right<- (Q3+(iqr*IQR))
  return(c(left,right))
}

remove_outliers <- function(df, n){
  df1 <- data.table(df)
  numeric_vars <- names(df1)[sapply(df1, is.numeric)]
  
  for(var in numeric_vars){
    lim <- IQR.outliers(df1[,var], n)
    df1[df1[,var] < lim[1], var] <- lim[1]
    df1[df1[,var] > lim[2], var] <- lim[2]
  }
  
  return(df1)  
  
}

IQR.outliers(raw_data_train,5)

summary(train_data$age)



IQR.outliers(raw_data_train,5)

str(raw_data_train[raw_data_train$campaign > 13,])

train_data <- remove_outliers((fix_types(raw_data_train)),3)
test_data <- scale_df(fix_types(raw_data_test))

str(train_data)
plot_density(train_data)

plot_density(raw_data_train[raw_data_train$pdays >-1, pdays])
IQR.outliers(raw_data_train[, pdays], 1.5)

str(raw_data_train[raw_data_train$pdays > 621, ])
