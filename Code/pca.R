cameron.pca <- prcomp(dtm, scale = TRUE)
summary(cameron.pca)
plot(cameron.pca)
head(cameron.pca$sdev)
cameron.pca$x[1:3, 1:3]

screeplot(cameron.pca, type="lines")
library (qcc)
variances <- cameron.pca$sdev^2  # variances
pareto.chart (variances, ylab="Variances")

library(devtools)

install_github("kassambara/factoextra")

library("factoextra")

library("FactoMineR")

res.pca <- PCA(inputData, graph = FALSE)

print (res.pca) 

##########################33

c6<-unique(cbind(as.character("document"),
                 as.character("term")))

plot(cameron.pca$x[,1], cameron.pca$x[,2],type="n")
text(cameron.pca$x[,1],cameron.pca$x[,2],labels=c6[,1],
     col=rank(c6[,2]))

plot(cameron.pca$x[,1],cameron.pca$x[,3],type="n")
text(cameron.pca$x[,1],cameron.pca$x[,3],labels=c6[,1],
     col=rank(c6[,2]))

head(sort(cameron.pca$x[,10],decreasing=TRUE),3)

biplot(cameron.pca)
partition6<-kmeans(cameron.pca$x[,1:30],10)
table(c6[,1],partition6$cluster)
round(partition6$centers, digits = 3)


pca_df <- data.frame(summary(cameron.pca))
