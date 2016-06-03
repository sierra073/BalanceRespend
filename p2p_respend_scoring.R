rm(list=ls(all=TRUE))

##import dataset
pred_base <- read.csv("~/Modeling/P2P Respend/pred_base.csv", header=T)


##change some column data types
pred_base$cust_id=as.character(pred_base$cust_id)
pred_base$other_txn_yn=as.factor(pred_base$other_txn_yn)
pred_base$pp_me=as.factor(pred_base$pp_me)
pred_base$conf_card=as.factor(pred_base$conf_card)
pred_base$conf_bank=as.factor(pred_base$conf_bank)
str(pred_base)

##initialize h20
library (h2o)
localH2O = h2o.init(max_mem_size = "4g")

modelTarget <- as.h2o(pred_base)

m = h2o.loadModel(path = "file:///C:/Users/scostanza/Documents/Modeling/P2P_Respend/GBM_model_R", conn = h2o.getConnection())

d.pred <- h2o.predict(gbm_model, newdata = modelTarget)

h2o_output <- h2o.cbind(modelTarget$cust_id, d.pred$predict, d.pred$p1)

output=as.data.frame(h2o_output)
output$cust_id=as.character(output$cust_id)

write.table(file = "C:/Users/scostanza/Documents/Modeling/P2P Respend/scored.csv", sep=",",  x = output, quote = FALSE, row.names = FALSE, col.names=FALSE) 


