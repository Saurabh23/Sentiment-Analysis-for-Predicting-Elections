# build the data to specify response variable, training set, testing set.

library(RTextTools)
container = create_container(dtm, as.factor(cameron$Sentiment),
                             trainSize=1:4000, testSize=4001:5000,virgin=FALSE)

models = train_models(container, algorithms=c("SVM"))
results = classify_models(container, models)
table(as.factor(cameron[4001:5000, 6]), results[,"SVM_LABEL"])
recall_accuracy(as.factor(cameron[4001:5000, 6]), results[,"SVM_LABEL"])

N=2
set.seed(2014)
cross_validate(container,N,"MAXENT")
cross_validate(container,N,"TREE")
cross_validate(container,N,"SVM")
cross_validate(container,N,"RF")