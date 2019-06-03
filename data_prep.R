source('Scripts/load_libraries.R')
source('Scripts/f_partition.R')
source('Scripts/classification_metrics.R')

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


train_data <- create_weekday(raw_data_train)

train_data <- fix_types(train_data)
str(train_data)

train_data <- drop_variables(train_data)

train_data <- scale_df(train_data)



train_data <- total_contacts(train_data)


str(train_data)



train_val <- f_partition(train_data, seed = 1414)

train_clust <- clustering(train_val$train, train_val$test, 5)
str(train_clust)

baseline <- glm(y ~ ., data=train_val$train, family=binomial)

test_glm<-predict(baseline, newdata = train_val$test, type='response')

f_metrics_simple(train_val$test$y, factor(test_glm>=0.5))

## SCORES (accuracy)

#drop_variables: 0.8923
#scale: 0.8923

#Weekday: 0.89342

#total_contact: 0.8916
#50 clusters: 0.8898
#10 clusters: 0.8907
#5 clusters: 0.8913
#3 clusters: 0.8912

#




