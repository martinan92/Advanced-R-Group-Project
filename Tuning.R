library("data.table")
source('pipeline_casero.R')

bank_train <- fread("Data/BankCamp_train.csv")
bank_test <- fread("Data/BankCamp_test.csv")

#################################### Logit Tuned ##################################### 
######################################################################################
######################################################################################
######################################################################################

#per https://stackoverflow.com/questions/47822694/logistic-regression-tuning-parameter-grid-in-r-caret-package,
#no tuning needed

logit_baseline <- pipeline_casero(bank_train, model="glm")

saveRDS(logit_baseline, "./saved_models/logit_baseline.rds")

###################################### RF Tuned ###################################### 
######################################################################################
######################################################################################
######################################################################################

tg_RF <- data.table(expand.grid(mtry = 2:4,
                                splitrule=c("gini", "extratrees"),
                                min.node.size = c(1, 5, 10)))
                    
rf_tuned <- pipeline_casero(bank_train, tunegrid=tg_RF)

saveRDS(rf_tuned, "./saved_models/rf_tuned.rds")

#################################### XG Boost Tuned ##################################
######################################################################################
######################################################################################
######################################################################################

tg_XG1 <- data.table(expand.grid(nrounds = seq(from = 200, to = nrounds, by = 50),
                               nrounds = seq(from = 200, to = nrounds, by = 50),
                               eta = c(0.025, 0.05, 0.1, 0.3),
                               max_depth = c(2, 3, 4, 5, 6),
                               gamma = 0,
                               colsample_bytree = 1,
                               min_child_weight = 1,
                               subsample = 1))

xg_tuned1 <- pipeline_casero(bank_train, model="xgbTree", tunegrid=tg_XG1)

tg_XG2 <- data.table(expand.grid(nrounds = seq(from = 50, to = nrounds, by = 50),
                               eta = xg_tuned1$bestTune$eta,
                               max_depth = ifelse(xg_tuned1$bestTune$max_depth == 2,
                                                  c(xg_tuned1$bestTune$max_depth:4),
                                                  xg_tuned1$bestTune$max_depth - 1:xg_tuned1$bestTune$max_depth + 1),
                               gamma = 0,
                               colsample_bytree = 1,
                               min_child_weight = c(1, 2, 3),
                               subsample = 1))

xg_tuned2 <- pipeline_casero(bank_train, model="xgbTree", tunegrid=tg_XG2)

tg_XG3 <- data.table(expand.grid(nrounds = seq(from = 50, to = nrounds, by = 50),
                               eta = xg_tuned1$bestTune$eta,
                               max_depth = xg_tuned2$bestTune$max_depth,
                               gamma = 0,
                               colsample_bytree = c(0.4, 0.6, 0.8, 1.0),
                               min_child_weight = xg_tuned2$bestTune$min_child_weight,
                               subsample = c(0.5, 0.75, 1.0)))
                   
xg_tuned3 <- pipeline_casero(bank_train, model="xgbTree", tunegrid=tg_XG3)

tg_XG4 <- data.table(expand.grid(nrounds = seq(from = 50, to = nrounds, by = 50),
                                eta = xg_tuned1$bestTune$eta,
                                max_depth = xg_tuned2$bestTune$max_depth,
                                gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0),
                                colsample_bytree = xg_tuned3$bestTune$colsample_bytree,
                                min_child_weight = xg_tuned2$bestTune$min_child_weight,
                                subsample = xg_tuned3$bestTune$subsample))

xg_tuned4 <- pipeline_casero(bank_train, model="xgbTree", tunegrid=tg_XG4)

tg_XG5 <- data.table(expand.grid(nrounds = seq(from = 100, to = 10000, by = 100),
                                eta = c(0.01, 0.015, 0.025, 0.05, 0.1),
                                max_depth = xg_tuned2$bestTune$max_depth,
                                gamma = xg_tuned4$bestTune$gamma,
                                colsample_bytree = xg_tuned3$bestTune$colsample_bytree,
                                min_child_weight = xg_tuned2$bestTune$min_child_weight,
                                subsample = xg_tuned3$bestTune$subsample))

xg_tuned5 <- pipeline_casero(bank_train, model="xgbTree", tunegrid=tg_XG5)

final_grid <- data.table(expand.grid(nrounds = tg_XG5$bestTune$nrounds,
                                    eta = tg_XG5$bestTune$eta,
                                    max_depth = tg_XG5$bestTune$max_depth,
                                    gamma = tg_XG5$bestTune$gamma,
                                    colsample_bytree = tg_XG5$bestTune$colsample_bytree,
                                    min_child_weight = tg_XG5$bestTune$min_child_weight,
                                    subsample = tg_XG5$bestTune$subsample))

xg_final <- pipeline_casero(bank_train, model="xgbTree", tunegrid=final_grid)

saveRDS(xg_final, "./saved_models/xg_final.rds")
                                         
                                         
                                         