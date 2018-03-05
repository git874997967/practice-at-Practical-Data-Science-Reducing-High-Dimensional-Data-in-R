library(RCurl)
urlfile = 'https://archive.ics.uci.edu/ml/machine-learning-databases/gisette/GISETTE/gisette_train.data'
x = getURL(urlfile, ssl.verifypeer = F)
gisetteRaw = read.table(
  textConnection(x),
  sep = '',
  header = F,
  stringsAsFactors = F
)
dim(gisetteRaw)
urlfile = 'https://archive.ics.uci.edu/ml/machine-learning-databases/gisette/GISETTE/gisette_train.labels'
y = getURI(urlfile, ssl.verifypeer = F)
g_labels = read.table(
  textConnection(y),
  sep = '',
  header = F,
  stringsAsFactors = F
)
gisette_df = cbind(as.data.frame(sapply(gisetteRaw, as.numeric)), cluster =
                     g_labels$V1)

dim(gisette_df)
gisette_df$cluster = as.factor(ifelse(gisette_df$cluster == 1, 'yes', 'no'))
### change the cluster in to factor  from 1 -1
table(gisette_df$cluster)
head(gisette_df)
###split data
set.seed(4321)
sam = sample(3,
             nrow(gisette_df),
             replace = T,
             prob = c(0.5, 0.25, 0.25))
table(sam)
gissette_train = gisette_df[sam == 2, ]
gissette_test = gisette_df[sam == 3, ]
gissette_validate = gisette_df[sam == 1, ]



## init the model
library(tidyverse)
library(caret)
library(gbm)
library(xgboost)
fitControl = trainControl(
  method = 'cv',
  number = 3,
  returnResamp = 'none',
  verboseIter = T,
  summaryFunction = twoClassSummary,
  classProbs = T
)
gbm_model <-
  train(
    cluster ~ .,
    data = gissette_train,
    trControl = fitControl,
    method = "gbm",
    metric = 'roc'
  )
ranger_model = train(
  cluster ~ .,
  gissette_train,
  trControl = fitControl,
  method = 'ranger',
  metric = 'roc'
)
xgb_model = train(
  cluster ~ .,
  gissette_train,
  trControl = fitControl,
  method = 'xgbTree',
  metric = 'roc'
)
pred_gbm = predict(gbm_model, gissette_test[, -5001])
pred_ranger = predict(ranger_model, gissette_test[, -5001])
pred_xgbTree = predict(xgb_model, gissette_test[,-5001])
postResample(pred_gbm, gissette_test$cluster)
postResample(pred_ranger, gissette_test$cluster)
postResample(pred_xgbTree, gissette_test$cluster)
### summary   that is to build model with all vars  and we can get the imortant index   after that we can select how many vars we wannna use
###with varImp function     select the principal  components and then predict with the subset
### different varImp with diff model are different ranger model without the varImp obj

varImp(gbm_model)
varImp(ranger_model)
varImp(xgb_model)
plot(varImp(gbm_model), top = 20)
plot(varImp(xgb_model), top = 20)

####### compare the gbm ang xgboost with 20 vars only   which will more accurate
gbm_vimp = varImp(gbm_model)
xgb_vimp = varImp(xgb_model)
gbm_results = data.frame(
  "VariableName" = row.names(gbm_vimp$importance),
  "Weight" = gbm_vimp$importance$Overall
)
xgb_results = tibble(
  "VariableName" = row.names(xgb_vimp$importance),
  "Weight" = xgb_vimp$importance$Overall
)
gbm_results = gbm_results[order(gbm_results$Weight),]
### xgb head  gbm tail
t = which(gissette_train %in% tail(gbm_results, 20))
###this is wired
gbm_truncated = gissette_train[, c(tail(gbm_results$VariableName, 20), 5001)]
xgb_truncated = gissette_train[, c(head(xgb_results$VariableName, 10), 'cluster')]

###test it again
fitControl = trainControl(method = 'none',classProbs = T)
fitControl
set.seed(4321)
gbm_trunked = train(
  cluster ~ .,
  gbm_truncated,
  trControl = fitControl,
  method = 'gbm',
  metric = 'roc',
  tuneGrid=expand.grid(
    n.trees=150,
    interaction.depth=3,
    shrinkage=0.1,
    n.minobsinnode=10
  )
)
xgb_trunked = train(
  cluster ~ .,
  xgb_truncated,
  trControl = fitControl,
  method = 'xgbTree',
  metric = 'ROC',
  tuneGrid=expand.grid(
  
    nrounds = 100, max_depth = 3, eta = 0.3, gamma = 0, colsample_bytree = 0.6, min_child_weight = 1, subsample = 1
  )
)
 xgb_truncated
gbm_pred=predict(object = gbm_trunked,gissette_validate)
xgb_pred=predict(object=xgb_trunked,gissette_validate)
postResample(gbm_pred,gissette_validate$cluster)
##5 vars  90%   20vars   94%   10 vars  93%
postResample(xgb_pred,gissette_validate$cluster)
### to sum up   xgb is much better than the gbm 