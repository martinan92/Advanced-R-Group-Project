#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(caret)
library(pROC)

### Data
heart<-fread('~/Desktop/IE_MBD/3rd_Term/R/Classification/data_heart_ready.csv')

### Partition Data
f_partition<-function(df,test_proportion=0.2, seed=NULL){
  
  if(!is.null(seed)) set.seed(seed)
  
  train_index<-sample(nrow(df), floor(nrow(df)*(1-test_proportion)), replace = FALSE)
  df_train<-df[train_index]
  df_test<-df[-train_index]
  
  return(list(train=df_train, test=df_test))
}


whole_data<-f_partition(df=heart,
                        test_proportion = 0.2,
                        seed = 872367823)

whole_data<-lapply(whole_data,function(x) setnames(x,'target_1','target'))

### Tree
tree_0<-rpart(formula = formula, data = whole_data$train, method = 'class', model = TRUE)
test_tree<-predict(tree_0, newdata = whole_data$test,type = 'prob')[, 'TRUE']

### Random Forest
rf_0<-ranger(formula=formula, data=whole_data$train,num.trees = 500, probability = T)
test_rf<-predict(rf_0, whole_data$test)$predictions[, 'TRUE']

### XGB
xgb_0<-xgboost(booster='gbtree',
               data=as.matrix(whole_data$train[, !'target', with=F]),
               label=whole_data$train$target==1,
               nrounds = 100,
               objective='binary:logistic')
test_xgb<-predict(xgb_0, newdata = as.matrix(whole_data$test[, !'target', with=F]))


#################################################################################################

## ROC Curve Point
f_roc_point <- function(real,predicted,t){
  
  cf<-table(predicted=factor(predicted>=t),real=real)
  
  TP<-cf['TRUE','TRUE']
  FN<-cf['FALSE','TRUE']
  TN<-cf['FALSE','FALSE']
  FP<-cf['TRUE','FALSE']
  
  
  sensitivity<-TP/(TP+FN)  
  specificity<-TN/(TN+FP)
  
  
  return(c(sensitivity=sensitivity,specificity=specificity))
}


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Model Evaluation with ROC Curves"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("type",
                  "Algorithms:",
                  choices = c('Tree','Random_Forest','XGBoost'),
                  selected = 'Tree'),
      sliderInput('threshold','Threshold Value',min=0,max=1,value=0.5,step=0.01),
      p('Slide the bar to set the value of the cutoff point')
    ),
    
    mainPanel(
      p('Accuracy'),
      textOutput("Accuracy"),
      hr(),
      p('Sensitivity'),
      textOutput("Sensitivity"),
      hr(),
      tabsetPanel(
        tabPanel("ROC Curve", plotOutput("myplot",width="100%")), 
        tabPanel("Confusion Matrix", plotOutput("cmplot",width="100%")) 
      )
    )
  )
)

server <- function(input, output) {
  ### Accuracy Text Output
  output$Accuracy <- renderText({ 
    ## Tree
    if (input$type == 'Tree'){
      cm_tree<-confusionMatrix(data = factor(test_tree>=input$threshold), reference=df_pred$output,positive='TRUE')
      cm_tree$byClass[['Balanced Accuracy']]
    } 
    ## Random Forest
    else if (input$type == 'Random_Forest'){
      cm_rf<-confusionMatrix(data = factor(test_rf >= input$threshold), reference=df_pred$output,positive='TRUE')
      cm_rf$byClass[['Balanced Accuracy']]
    }
    ## XG Boost
    else {
      cm_xgb<-confusionMatrix(data = factor(test_xgb>= input$threshold), reference=df_pred$output,positive='TRUE')
      cm_xgb$byClass[['Balanced Accuracy']]
    }
  })
  
  ### Sensitivity Text Output
  output$Sensitivity <- renderText({ 
    ## Tree
    if (input$type == 'Tree'){
      cm_tree<-confusionMatrix(data = factor(test_tree>=input$threshold), reference=df_pred$output,positive='TRUE')
      cm_tree$byClass[['Sensitivity']]
    } 
    ## Random Forest
    else if (input$type == 'Random_Forest'){
      cm_rf<-confusionMatrix(data = factor(test_rf >= input$threshold), reference=df_pred$output,positive='TRUE')
      cm_rf$byClass[['Sensitivity']]
    }
    ## XG Boost
    else {
      cm_xgb<-confusionMatrix(data = factor(test_xgb>= input$threshold), reference=df_pred$output,positive='TRUE')
      cm_xgb$byClass[['Sensitivity']]
    }
  })
  
  output$myplot<- renderPlot({
    if (input$type == 'Tree'){
      plot.roc(roc(response = df_pred$output,
                   predictor= test_tree), 
               title='Decision Tree',
               col="#377EB8"); grid()
      roc_point <- data.frame(f_roc_point(real=df_pred$output, predicted=test_tree, t = input$threshold))
      points(roc_point[1,1], roc_point[2,1], pch=18, col='red', cex =2)
    } 
    ## Random Forest
    else if (input$type == 'Random_Forest'){
      plot.roc(roc(response = df_pred$output,
                   predictor= test_rf), 
               title=' Random Forest',
               col="#ef3b2c"); grid()
      roc_point <- data.frame(f_roc_point(real=df_pred$output, predicted=test_rf, t = input$threshold))
      points(roc_point[1,1], roc_point[2,1], pch=18, col='red', cex =2)
    } 
    ## XGBoost 
    else {
      plot.roc(roc(response = df_pred$output,
                   predictor= test_xgb), 
               title='XG Boost',
               col="#4DAF4A"); grid()
      roc_point <- data.frame(f_roc_point(real=df_pred$output, predicted=test_xgb, t = input$threshold))
      points(roc_point[1,1], roc_point[2,1],pch=18, col='red', cex =2)
    }
  })
  ## Confusion Matrix Plot
  output$cmplot<- renderPlot({
    ## Tree
    if (input$type == 'Tree'){
      cm_tree<-confusionMatrix(data = factor(test_tree>=input$threshold), reference=df_pred$output,positive='TRUE')
      fourfoldplot(cm_tree$table, color = c("#99CCFF", "#6699CC"))
    } 
    ## Random Forest
    else if (input$type == 'Random_Forest'){
      cm_rf<-confusionMatrix(data = factor(test_rf >= input$threshold), reference=df_pred$output,positive='TRUE')
      fourfoldplot(cm_rf$table,  color = c("#fb6a4a", "#ef3b2c"))
    }
    ## XG Boost
    else {
      cm_xgb<-confusionMatrix(data = factor(test_xgb>= input$threshold), reference=df_pred$output,positive='TRUE')
      fourfoldplot(cm_xgb$table, color = c("#addd8e", "#78c679"))
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

