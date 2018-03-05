# practice-at-Practical-Data-Science-Reducing-High-Dimensional-Data-in-R
Exercise and notes in Practical Data Scienceú║Reducing High Dimensional Data in R   course from Udemy
学到基本知识有
   
 R语言中   用于主成份 分析的方法有  prcomp 和princomp


区别是  


对于  prcomp (网友推荐使用) ， 使用  奇异值分解的方法 根据官方文档 的说法这样会使得数字计算的更加准确 
选择 相应指标的的多个列 只需要运用predict 方法  将结果模型用在 待提取数据集即可 例如

predict（test，princ)[,1:5] 找出符合princ 降序的test属性前五个 
 总结  根据例子    抽取前20 个主要特征的预测结果进行建模 结果可能和使用全体特征 建模成绩相差无几  但是运行和调教速度有了质的飞跃
 完成了交叉验证和特征值选取的学习
 特征值通过varImp方法查看
 通过进行比较发现xgbtree比gbm好一些参数多对数据维度不是特别敏感速度快
 
glmnet的主成份分析参数选择不能通过简单的top选择
