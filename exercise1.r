library(factoextra)
prc_arrest = prcomp(USArrests, scale = T)
prc_mtcars = prcomp(mtcars, scale = T)
pri_arrest = princomp(USArrests, cor =F, scores = T)
pri_mtcars = princomp(mtcars, scores = T, cor = T)
summary(prc_arrest)
summary(pri_arrest)
plot(prc_arrest)
plot(pri_arrest)
biplot(prc_arrest)
biplot(pri_arrest)
##mtcars dataset pca
summary(prc_mtcars)
summary(pri_mtcars)
plot(prc_mtcars)
plot(pri_mtcars)
biplot(prc_mtcars)
biplot(pri_mtcars)
### the summary is    with cor=F two methods are different 
### from summary and plot func  we can tell the most important values in the cols and how important they are
fviz_eig(prc_mtcars)
fviz_pca_var(prc_mtcars,col.var = "contrib", # Color by contributions to the PC # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

 
