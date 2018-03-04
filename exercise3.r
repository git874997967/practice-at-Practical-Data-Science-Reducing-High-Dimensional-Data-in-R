# 需要对变量进行初步降维。其中一种需要删除的变量是常数自变量，或者是方差极小的自变量，对应的命令是nearZeroVar。
# 另一类需要删除的是与其它自变量有很强相关性的变量，对应的命令是findcorrelation。自变量中还有可能存在多重共线性问题，可以用findLinearCombos命令将它们找出来。
# 我们还需要将数据进行标准化并补足缺失值，这时可以用preProcess命令，缺省参数是标准化数据，其高级功能还包括用K近邻和装袋决策树两种方法来预测缺失值。此外它还可以进行cox幂变换和主成分提取。
# 用createDataPartition将数据进行划分，分成75%的训练样本和25%检验样本，类似的命令还包括了createResample用来进行简单的自助法抽样，还有createFolds来生成多重交叉检验样本。
# 建模前还可以对样本数据进行图形观察，例如对前两个变量绘制箱线图。library(caret)
data(mdrr)
zerovar=nearZeroVar(mdrrDescr)
mdrrDescr[,22]
newdata1=mdrrDescr[,-zerovar]#删除的变量是常数自变量，或者是方差极小的自变量

descrCorr = cor(newdata1)
highCorr = findCorrelation(descrCorr, 0.90)
newdata2 = newdata1[, -highCorr]#找高度自相关的变量
comboInfo = findLinearCombos(newdata2)
newdata2=newdata2[, -comboInfo$remove]#找存在多重共线性问题变量

Process = preProcess(newdata2)
newdata3 = predict(Process, newdata2)#将数据进行标准化并补足缺失值

inTrain = createDataPartition(mdrrClass, p =0.75, list = FALSE)
traindata=newdata3[inTrain,]
textdata=newdata3[-inTrain,]#用createDataPartition将数据进行划分，分成75%的训练样本和25%检验样本
trainy = mdrrClass[inTrain]
testy = mdrrClass[-inTrain]

featurePlot(traindata[,1:2],trainy,plot='box')#对前两个变量绘制箱线图
