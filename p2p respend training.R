##import dataset
setwd("Users/scostanza/Documents/Modeling/P2P Respend")
respendtraining <- read.delim("~/Modeling/P2P Respend/respendtraining1.txt", header = TRUE, na.strings="?")

##validation frame
#respendvalidation <- read.delim("~/Modeling/P2P Respend/respendvalidation1.txt", header = TRUE, na.strings="?")


##change some column data types
respendtraining$cust_id=as.character(respendtraining$cust_id)
respendtraining$respend=as.factor(respendtraining$respend)
respendtraining$other_txn_yn=as.factor(respendtraining$other_txn_yn)
respendtraining$pp_me=as.factor(respendtraining$pp_me)
respendtraining$conf_card=as.factor(respendtraining$conf_card)
respendtraining$conf_bank=as.factor(respendtraining$conf_bank)
respendtraining$restriction=as.factor(respendtraining$restriction)
respendtraining$iss_fi=as.factor(respendtraining$iss_fi)
str(respendtraining)

# respendvalidation$cust_id=as.character(respendvalidation$cust_id)
# respendvalidation$respend=as.factor(respendvalidation$respend)
# respendvalidation$other_txn_yn=as.factor(respendvalidation$other_txn_yn)
# respendvalidation$pp_me=as.factor(respendvalidation$pp_me)
# respendvalidation$conf_card=as.factor(respendvalidation$conf_card)
# respendvalidation$conf_bank=as.factor(respendvalidation$conf_bank)
# str(respendvalidation)

##outlier treatment
respendtraining=respendtraining[respendtraining$last_rcv_nptv < 25000,]
respendtraining=respendtraining[respendtraining$tenure < 500,]
respendtraining=respendtraining[respendtraining$monetary < 1000000,]
respendtraining=respendtraining[respendtraining$frequency < 20000,]
respendtraining=respendtraining[respendtraining$rp2p_txn < 4500,]
respendtraining=respendtraining[respendtraining$rp2p_aov < 30000,]
respendtraining=respendtraining[respendtraining$n_withdraw < 460,]
respendtraining=respendtraining[is.na(respendtraining$ls_ratio) | (!is.na(respendtraining$ls_ratio) & respendtraining$ls_ratio < 20000),]

##initialize h20
library (h2o)
localH2O = h2o.init(max_mem_size = "4g")

respend.hex <- as.h2o(localH2O, respendtraining)

summary (respend.hex)


# validation.hex <- as.h2o(localH2O, respendvalidation)
# 
# summary (validation.hex)


#impute missing values - gender (mode), age group (median)
#h2o.impute(data, "gender", method = c("mode"), by = NULL)

#### Logistic regression
glm_model=h2o.glm(y = 29, x = 1:28, training_frame =
                    respend.hex, family = "binomial", nfolds = 3, alpha=0.5)
print(glm_model)
n_coeff1=as.data.frame(cbind(glm_model@model$coefficients_table$names, 
                            glm_model@model$coefficients_table$standardized_coefficients))
colnames(n_coeff1)=c("variable","importance")
n_coeff1[order(n_coeff1$importance, decreasing=T) , ]

#### GBM
gbm_model_wa=h2o.gbm(y = 29, x =1:28, training_frame = respend.hex, #validation_frame=validation.hex, 
                  nfolds=4,ntrees = 300, max_depth = 5, learn_rate = 0.1, importance=TRUE, score_each_iteration = TRUE)

gbm_model_wa
gbm.VI = gbm_model_wa@model$variable_importances

gbm.VI2=as.data.frame(cbind(gbm.VI$variable,gbm.VI$scaled_importance))
colnames(gbm.VI2)=c("variable","importance")
gbm.VI2[order(gbm.VI2$importance, decreasing=T) , ]

scoring_hist <- as.data.frame(gbm_model@model$scoring_history)
plot(scoring_hist$number_of_trees, scoring_hist$training_MSE, type="l",col="red", xlab="number of trees", ylab="Model MSE")
lines(scoring_hist$number_of_trees, scoring_hist$validation_MSE, col="blue")

#predict test
pred_base <- read.csv("~/Modeling/P2P Respend/pred_base.csv", header=T)
pred_base$cust_id=as.character(pred_base$cust_id)
pred_base$other_txn_yn=as.factor(pred_base$other_txn_yn)
pred_base$pp_me=as.factor(pred_base$pp_me)
pred_base$conf_card=as.factor(pred_base$conf_card)
pred_base$conf_bank=as.factor(pred_base$conf_bank)

modelTarget <- as.h2o(pred_base)

d.pred <- h2o.predict(gbm_model, newdata = modelTarget)
d.pred=as.data.frame(d.pred)
library(ggplot2)
qplot(d.pred$p1, binwidth = 0.01, fill = factor(d.pred$predict))

perf=h2o.performance(gbm_model,measure = "F1")


#save model
h2o.saveModel(object = gbm_model, path="file:///C:/Users/scostanza/Documents/Modeling/P2P_Respend", force = T)

  #caret
# library(caret)
# fitControl <- trainControl(## 10-fold CV
#   method = "repeatedcv",
#   number = 10,
#   ## repeated ten times
#   repeats = 10)
# gbmGrid <-  expand.grid(interaction.depth = c(5, 9, 15),
#                         n.trees = c(50, 100, 300, 1000, 5000),
#                         shrinkage = 0.1,
#                         n.minobsinnode = 20)
# 
# nrow(gbmGrid)
# 
# set.seed(825)
# gbmFit2 <- train(respend ~ ., data = respendtraining1,
#                  method = "gbm",
#                  trControl = fitControl,
#                  verbose = FALSE,
#                  ## Now specify the exact models 
#                  ## to evaluate:
#                  tuneGrid = gbmGrid)
# gbmFit2
# set.seed(998)
# inTrain <- createDataPartition(y=respendtraining1$respend,
#                                p=0.7, list=FALSE)
# training <- respendtraining1[inTrain,]
# testing <- respendtraining1[-inTrain,]
# dim(training); dim(testing)
# table(training$respend)
# table(testing$respend)

### Random forest
rf_model <- h2o.randomForest(y = 29, x = 1:28,
                      training_frame = respend.hex,
                      validation_frame = validation.hex,
                      ntrees=100,
                      max_depth=20,
                      nfolds=4, build_tree_one_node = TRUE)