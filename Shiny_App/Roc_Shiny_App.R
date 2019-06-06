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
logit<- read.csv('logit_cluster_prob_predictions.csv')
rf <- read.csv('rf_pred_probabilities.csv')
xgb <- read.csv('pred_probs_xgb.csv')

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
  titlePanel("Classification Result"),
  fluidRow(
    column(3,
      hr(),
      h4("Inputs", style = "color: #377EB8"),
      selectInput("type",
                  "Algorithms",
                  choices = c('Logistic_Regression','Random_Forest','XGBoost'),
                  selected = 'Logistic_Regression'),
      hr(),
      sliderInput('threshold','Threshold Value',min=0,max=1,value=0.5,step=0.01),
      p('Slide the bar to set the value of the cutoff point', style = "color:blue")
    ),
    column(3,
      hr(),
      h4("Parameters Table", style = "color: #377EB8"),
      strong('Accuracy'),
      textOutput("Accuracy"),
      hr(),
      strong('Sensitivity'),
      textOutput("Sensitivity"),
      hr(),
      strong('Specificity'),
      textOutput("Specificity"), 
      hr(),
      strong('True Positive Rate'),
      textOutput("TPR"),
      hr(),
      strong('True Negative Rate'),
      textOutput("TNR")
    ),
    column(6,
      plotOutput("myplot",width="100%",height = "300px"),
      plotOutput("cmplot",width="100%",height = "300px") 
      
    )
  )
)

server <- function(input, output) {
  
  ### Accuracy Text Output
  output$Accuracy <- renderText({ 
    ## Logit Regression
    if (input$type == 'Logistic_Regression'){
      logit_cm <- confusionMatrix(data = factor(logit$yes>=input$threshold, levels=c("TRUE", "FALSE")), 
                                  reference=factor(as.character(logit$obs) == "yes", levels=c("TRUE", "FALSE")),
                                  positive='TRUE')
      logit_cm$overall[['Accuracy']]
    } 
    ## Random Forest
    else if (input$type == 'Random_Forest'){
      rf_cm <- confusionMatrix(data = factor(rf$yes>=input$threshold, levels=c("TRUE", "FALSE")), 
                               reference=factor(as.character(rf$obs) == "yes", levels=c("TRUE", "FALSE")),
                               positive='TRUE')
      rf_cm$byClass[['Balanced Accuracy']]
    }
    ## XG Boost
    else {
      xgb_cm <- confusionMatrix(data = factor(xgb$yes>=input$threshold, levels=c("TRUE", "FALSE")), 
                                reference=factor(as.character(xgb$obs) == "yes", levels=c("TRUE", "FALSE")),
                                positive='TRUE')
      xgb_cm$byClass[['Balanced Accuracy']]
    }
  })
  
  ### Sensitivity Text Output
  output$Sensitivity <- renderText({ 
    ## Logistic Regression
    if (input$type == 'Logistic_Regression'){
      logit_cm <- confusionMatrix(data = factor(logit$yes>=input$threshold, levels=c("TRUE", "FALSE")), 
                                  reference=factor(as.character(logit$obs) == "yes", levels=c("TRUE", "FALSE")),
                                  positive='TRUE')
      logit_cm$byClass[['Sensitivity']]
    } 
    ## Random Forest
    else if (input$type == 'Random_Forest'){
      rf_cm <- confusionMatrix(data = factor(rf$yes>=input$threshold, levels=c("TRUE", "FALSE")), 
                               reference=factor(as.character(rf$obs) == "yes", levels=c("TRUE", "FALSE")),
                               positive='TRUE')
      rf_cm$byClass[['Sensitivity']]
    }
    ## XG Boost
    else {
      xgb_cm <- confusionMatrix(data = factor(xgb$yes>=input$threshold, levels=c("TRUE", "FALSE")), 
                                reference=factor(as.character(xgb$obs) == "yes", levels=c("TRUE", "FALSE")),
                                positive='TRUE')
      xgb_cm$byClass[['Sensitivity']]
    }
  })
  
  ### Specificity Text Output
  output$Specificity <- renderText({ 
    ## Logistic Regression
    if (input$type == 'Logistic_Regression'){
      logit_cm <- confusionMatrix(data = factor(logit$yes>=input$threshold, levels=c("TRUE", "FALSE")), 
                                  reference=factor(as.character(logit$obs) == "yes", levels=c("TRUE", "FALSE")),
                                  positive='TRUE')
      logit_cm$byClass[['Specificity']]
    } 
    ## Random Forest
    else if (input$type == 'Random_Forest'){
      rf_cm <- confusionMatrix(data = factor(rf$yes>=input$threshold, levels=c("TRUE", "FALSE")), 
                               reference=factor(as.character(rf$obs) == "yes", levels=c("TRUE", "FALSE")),
                               positive='TRUE')
      rf_cm$byClass[['Specificity']]
    }
    ## XG Boost
    else {
      xgb_cm <- confusionMatrix(data = factor(xgb$yes>=input$threshold, levels=c("TRUE", "FALSE")), 
                                reference=factor(as.character(xgb$obs) == "yes", levels=c("TRUE", "FALSE")),
                                positive='TRUE')
      xgb_cm$byClass[['Specificity']]
    }
  })
  
  ### True Positive Rate 
  output$TPR <- renderText({ 
    ## Logistic Regression
    if (input$type == 'Logistic_Regression'){
      logit_cm <- confusionMatrix(data = factor(logit$yes>=input$threshold, levels=c("TRUE", "FALSE")), 
                                  reference=factor(as.character(logit$obs) == "yes", levels=c("TRUE", "FALSE")),
                                  positive='TRUE')
      logit_cm$byClass[['Pos Pred Value']]
    } 
    ## Random Forest
    else if (input$type == 'Random_Forest'){
      rf_cm <- confusionMatrix(data = factor(rf$yes>=input$threshold, levels=c("TRUE", "FALSE")), 
                                  reference=factor(as.character(rf$obs) == "yes", levels=c("TRUE", "FALSE")),
                                  positive='TRUE')
      rf_cm$byClass[['Pos Pred Value']]
    }
    ## XG Boost
    else {
      xgb_cm <- confusionMatrix(data = factor(xgb$yes>=input$threshold, levels=c("TRUE", "FALSE")), 
                               reference=factor(as.character(xgb$obs) == "yes", levels=c("TRUE", "FALSE")),
                               positive='TRUE')
      xgb_cm$byClass[['Pos Pred Value']]
    }
  })
  
  ### True Negative Rate 
  output$TNR <- renderText({ 
    ## Logistic Regression
    if (input$type == 'Logistic_Regression'){
      logit_cm <- confusionMatrix(data = factor(logit$yes>=input$threshold, levels=c("TRUE", "FALSE")), 
                                  reference=factor(as.character(logit$obs) == "yes", levels=c("TRUE", "FALSE")),
                                  positive='TRUE')
      logit_cm$byClass[['Neg Pred Value']]
    } 
    ## Random Forest
    else if (input$type == 'Random_Forest'){
      rf_cm <- confusionMatrix(data = factor(rf$yes>=input$threshold, levels=c("TRUE", "FALSE")), 
                               reference=factor(as.character(rf$obs) == "yes", levels=c("TRUE", "FALSE")),
                               positive='TRUE')
      rf_cm$byClass[['Neg Pred Value']]
    }
    ## XG Boost
    else {
      xgb_cm <- confusionMatrix(data = factor(xgb$yes>=input$threshold, levels=c("TRUE", "FALSE")), 
                                reference=factor(as.character(xgb$obs) == "yes", levels=c("TRUE", "FALSE")),
                                positive='TRUE')
      xgb_cm$byClass[['Neg Pred Value']]
    }
  })  
  
## Plotting Roc Curve
  output$myplot<- renderPlot({
    if (input$type == 'Logistic_Regression'){
      plot.roc(roc(response = logit$obs,
                   predictor= logit$yes), 
               title='Logistic_Regression',
               col="#377EB8"); grid()
      ### Drawing the roc point
      roc_point <- f_roc_point(real=factor(as.character(logit$obs) == "yes"),
                                          predicted=logit$yes, t = input$threshold)
      points(roc_point[2], roc_point[1], pch=18, col='red', cex =2)
    } 
    ## Random Forest
    else if (input$type == 'Random_Forest'){
      plot.roc(roc(response = rf$obs,
                   predictor= rf$yes), 
               title=' Random Forest',
               col="#ef3b2c"); grid()
      ### Drawing the roc point
      roc_point <- f_roc_point(real=factor(as.character(rf$obs) == "yes"),
                               predicted=rf$yes, t = input$threshold)
      points(roc_point[2], roc_point[1], pch=18, col='red', cex =2)
    } 
    ## XGBoost 
    else {
      plot.roc(roc(response = xgb$obs,
                   predictor= xgb$yes), 
               title='XG Boost',
               col="#4DAF4A"); grid()
      ### Drawing the roc point
      roc_point <- f_roc_point(real=factor(as.character(xgb$obs) == "yes"),
                               predicted=xgb$yes, t = input$threshold)
      points(roc_point[2], roc_point[1], pch=18, col='red', cex =2)
    }
  })
  ## Confusion Matrix Plot
  output$cmplot<- renderPlot({
    ## Logistic Regression
    if (input$type == 'Logistic_Regression'){
      logit_cm <- confusionMatrix(data = factor(logit$yes>=input$threshold, levels=c("TRUE", "FALSE")), 
                                  reference=factor(as.character(logit$obs) == "yes", levels=c("TRUE", "FALSE")),
                                  positive='TRUE')
      fourfoldplot(logit_cm$table, color = c("#99CCFF", "#6699CC"), conf.level = 0, margin = 1)
    } 
    ## Random Forest
    else if (input$type == 'Random_Forest'){
      rf_cm <- confusionMatrix(data = factor(rf$yes>=input$threshold, levels=c("TRUE", "FALSE")), 
                               reference=factor(as.character(rf$obs) == "yes", levels=c("TRUE", "FALSE")),
                               positive='TRUE')
      fourfoldplot(rf_cm$table,  color = c("#fb6a4a", "#ef3b2c"), conf.level = 0, margin = 1)
    }
    ## XG Boost
    else {
      xgb_cm <- confusionMatrix(data = factor(xgb$yes>=input$threshold, levels=c("TRUE", "FALSE")), 
                                reference=factor(as.character(xgb$obs) == "yes", levels=c("TRUE", "FALSE")),
                                positive='TRUE')
      fourfoldplot(xgb_cm$table, color = c("#addd8e", "#78c679"), conf.level = 0, margin = 1)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

