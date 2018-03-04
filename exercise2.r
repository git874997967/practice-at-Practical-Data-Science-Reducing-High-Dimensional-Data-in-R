#### redycing high dimensional data in R

library(RCurl)
library(caret)
urlfile <-
  (
    'https://archive.ics.uci.edu/ml/machine-learning-databases/gisette/GISETTE/gisette_train.data'
  )
x <- getURL(urlfile, ssl.verifypeer = FALSE)
giseteRaw = read.table(
  textConnection(x),
  sep = "",
  header = F,
  stringsAsFactor = F
)
urlfile <-
  "https://archive.ics.uci.edu/ml/machine-learning-databases/gisette/GISETTE/gisette_train.labels"
y = getURL(urlfile, ssl.verifypeer = F)
g_labels = read.table(
  textConnection(y),
  sep = "",
  stringsAsFactors = F,
  header = F
)
# g_labels
# giseteRaw[1, c(2:100)]
####smaller the data set  which feature near zero percentage
#删除的变量是常数自变量，或者是方差极小的自变量
nzv = nearZeroVar(giseteRaw, saveMetrics = T)
print(paste(" Range ", range(nzv$percentUnique)))
new_gisette_nzv = giseteRaw[rownames(nzv[nzv$percentUnique > 0.1,])]
dim(new_gisette_nzv)
# make the dataframe
gisette_df = cbind(as.data.frame(sapply(new_gisette_nzv, as.numeric)),
                   cluster = g_labels$V1)
dim(gisette_df)
###split dataset
set.seed(1234)
sam = sample(2,
             nrow(gisette_df),
             replace = T,
             prob = c(0.85, 0.15))
table(sam)
traindf = gisette_df[sam == 1,]
testdf = gisette_df[sam == 2,]
traindf$cluster = as.factor(traindf$cluster)
fitControl = trainControl(method = 'none')
model <-
  train(
    cluster ~ .,
    data = traindf,
    tuneGrid = expand.grid(
      n.trees = 200,
      interaction.depth = 3,
      shrinkage = 0.1,
      n.minobsinnode = 10
    ),
    trControl = fitControl,
    method = "gbm",
    metric = 'roc'
  )
summary(model)
testdf$cluster=as.factor(testdf$cluster)

setdiff(names(testdf),'cluster')
prediction=predict(model,testdf[,setdiff(names(testdf),'cluster')],type='raw')
dim(testdf)
dim(testdf[,setdiff(names(testdf),'cluster')])
head(prediction)
prediction2=predict(model,testdf)
postResample(pred=prediction,obs=testdf$cluster)
postResample(pred=prediction2,obs=testdf$cluster)
#### utilize the pca  
##with only one row 
n.comp=3

### scale and prcomp
 pmatrix=scale(new_gisette_nzv)
princ=prcomp(new_gisette_nzv,scale=T)
 
 (head(princ))
### to find exact num of princ cols  with  predict function ??? 
dfComponents=predict(princ,pmatrix)[,1:n.comp]
dim(dfComponents)
 edit(head(dfComponents))
gisette_df=cbind(as.data.frame(dfComponents),cluster=g_labels$V1)
train1df=gisette_df[sam==1,]
test1df=gisette_df[sam==2,]
train1df$cluster=as.factor(train1df$cluster)
test1df$cluster=as.factor(test1df$cluster)
model1=train(cluster~., data=train1df,tuneGrid=expand.grid(n.trees=500,interaction.depth=3,shrinkage=0.1,n.minobsinnode=10),trControl=fitControl,method='gbm',metric='roc')

prediction1=predict(model1,test1df[,setdiff(names(test1df),'cluster')],type='raw')
 
postResample(pred=prediction1,obs=test1df$cluster)


 


 