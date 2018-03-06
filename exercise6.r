install.packages("fscaret", dependencies = c("Depends", "Suggests"))
library(fscaret)
library(caret)
library(mice)
data(funcRegPred)
print(funcRegPred)
data("funcClassPred")
print(funcClassPred)
###read the data
titanicDF <-
  read.csv('http://math.ucdenver.edu/RTutorial/titanic.txt', sep = '\t')
str(titanicDF)
###add new feature
titanicDF$Title = ifelse(grepl('Mrs', titanicDF$Name),
                         "Mrs",
                         ifelse(
                           grepl("Mr", titanicDF$Name),
                           "Mr",
                           ifelse(
                             grepl("Miss", titanicDF$Name),
                             "Miss",
                             ifelse(grepl('Master', titanicDF$Name), "Master", "Others")
                           )
                         ))
titanicDF[titanicDF$Title == 'Others', 1]
summary(titanicDF)
###fix the NA value
age = mice(
  data = titanicDF,
  maxit = 10,
  meth = 'rf',
  m = 5,
  seed = 4321
)
summary(age$imp$Age$`1`)

titanicDF$Age[is.na(titanicDF$Age)] = age$imp$Age$"3"
##now we have the complete data
## first step  remove the name col
data = titanicDF[, -1]
summary(data)
data$Title = as.factor(data$Title)
data$Survived = as.factor(data$Survived)
###MISO  is important
data = data[, c(1:3, 5)]
data = cbind(data, titanicDF$Survived)
colnames(data)[5] = 'Survived'
###change into  dummary vars
data_dummy = dummyVars("~.", data, fullRank = F)
## wanna select exact cols use predict function
titanic = as.data.frame(predict(data_dummy, data))
titanic$Survived = as.factor(ifelse(titanic$Survived == 1, 'yes', 'no'))
###split the dataset
set.seed(4321)
sam = sample(2,
             nrow(titanic),
             replace = T,
             prob = c(0.85, 0.15))
train = titanic[sam == 1,]
test = titanic[sam == 2,]
### select most important vars using fscaret
library(party)
library(kknn)
library(GAMBoost)
library(rpart)
library(gbm)
library(pls)
myFS.class <- fscaret(
  train,
  test,
  myTimeLimit = 20,
  preprocessData = TRUE,
  with.labels = TRUE,
  classPred = TRUE,
  regPred = FALSE,
  Used.funcClassPred = c("gbm", "rpart", "pls"),
  supress.output = FALSE,
  no.cores = NULL,
  saveModel = FALSE
)
###get the most important features in diff models
results = myFS.class$VarImp$matrixVarImp.MeasureError
results$Input_no <- as.numeric(results$Input_no)
results <- results[, setdiff(names(results), c('SUM%', 'ImpGrad'))]
myFS.class$PPlabels$Input_no <-
  as.numeric(rownames(myFS.class$PPlabels))
results <- merge(
  x = results,
  y = myFS.class$PPlabels,
  by = "Input_no",
  all.x = T
)
results <- results[order(-results$SUM),]
print(head(results))
###trunk the dataset from the result
traindf_truncated <-
  train[, c(head(as.character(results$Labels), 5),
              'Survived')]
dim(traindf_truncated)
###build predict cv with function pls
objControl <- trainControl(method='cv', number=5, returnResamp='none',
                           summaryFunction = twoClassSummary, classProbs = TRUE)
# pls model
set.seed(1234)
pls_model <- train(Survived~., data=traindf_truncated, method="pls",
                   metric='roc', trControl=objControl)
pls_predictions <- predict(object=pls_model,
                           test[,setdiff(names(traindf_truncated), 'Survived')], type='prob')
library(pROC)
print(auc(predictor=pls_predictions[[2]],response=
            ifelse(testdf$Survived=='yes',1,0)))
###simulate the fscaret by our self
head(train)
traindf_truncated=train[,c(3,5,9,12)]
head(traindf_truncated)
method_names <- c("C5.0", "gbm", "rf")
str(traindf_truncated)
for (method_name in method_names) {
  
  set.seed(1234)
  
  model_method_name <- train(Survived~., data=traindf_truncated,
                 method=method_name,
                 metric='roc', trControl=objControl)
  predictions <- predict(object=model,
                         test[,setdiff(names(traindf_truncated), 'Survived')], type='prob')
  # print(pROC::auc(predictor=predictions[[2]],response=
  #             ifelse(testdf$Survived=='yes',1,0)))
}
models=data.frame()
models=list()
models[1]=list(100)
model[2]=list(100)
model[3]=list(100)
models
for(i in 1:length(method_names)){
  print(method_names[i])
 A=train(Survived~., data=traindf_truncated,
        method=method_names[i],
        metric='roc', trControl=objControl)
 models[i]=A$bestTune
}
models[1] 
models[2] 
models
as.character(A$bestTune)
as.list(A$bestTune)
