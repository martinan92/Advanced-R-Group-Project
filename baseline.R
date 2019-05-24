library("data.table")

bank_train <- fread("BankCamp_train.csv")
bank_test <- fread("BankCamp_test.csv")


logit_baseline <- pipeline_casero(bank_train, model="glm")

saveRDS(logit_baseline, "./saved_models/logit_baseline.rds")

tg <- data.table(expand.grid(mtry=15,
                             splitrule='gini',
                             min.node.size=5))

rf_baseline <- pipeline_casero(bank_train, tunegrid=tg)

saveRDS(rf_baseline, "./saved_models/rf_baseline.rds")
