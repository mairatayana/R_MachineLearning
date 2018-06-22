require(caret)

# Import data from CSV file
pacientes <- read.csv(file="C:\\Users\\maira\\OneDrive\\Documentos\\UNIFESP\\Mestrado\\AM\\TrabalhoFinal\\dadosRespiracao.csv", header=TRUE, sep=",")
View(pacientes)

# Retirando o atributo ID, que nao contribui para a predicao
pacientesPart = pacientes[-c(1)]
View(pacientesPart)

doNorm <- function(x) {(x - min(x))/(max(x)-min(x))}
pacientesPartNorm <- as.data.frame(lapply(pacientesPart, doNorm))
View(pacientesPartNorm)

library(caret)
#separa a classe dos atributos.
classes <- pacientesPartNorm[, "Respiracao0nasal1bucal"]
classes <- factor(ifelse(classes==0, "Zero", "One"))
predictors <- pacientesPartNorm[, -match(c("Respiracao0nasal1bucal"), colnames(pacientesPartNorm))]

# Usa pacote caret com leave one out para o modelo Logistic Model Trees
# http://topepo.github.io/caret/train-models-by-tag.html#model-tree
modelTree <- train(predictors, classes, method = "LMT", trControl = trainControl(method = "LOOCV"))
modelTree

# Usa pacote caret com leave one out para o modelo Bayesian Generalized Linear Model - classificador linear
# http://topepo.github.io/caret/train-models-by-tag.html#bayesian-model
modelBayes <- train(predictors, classes, method = "bayesglm", trControl = trainControl(method = "LOOCV"))
modelBayes

# Usando KNN do pacote caret
folds <- createFolds(classes, k = 10, list = FALSE)
folds
foldsPred <- createFolds(predictors$LordoseCervical, k = 10, list = FALSE)
foldsPred
# summarizing first training/test fold
classes <- as.data.frame(classes)
predictors <- as.data.frame(predictors)
trainClasses <- classes[folds != 1,]
trainPredictors <- predictors[foldsPred != 1,]
summary(trainClasses)
summary(trainPredictors)
estClasses <- classes[folds == 1,]
testPredictors <- predictors[folds == 1,]
summary(testClasses)

knn1 = knn3(trainPredictors, trainClasses, k = 1)
knn1
predict(knn1, head(testPredictors), type = "prob")

##############################################################################
# Daqui para baixo s??o os testes
##############################################################################
train_set <- createDataPartition(classes, p = 0.8, list = FALSE)
str(train_set)

# int [1:42, 1] 1 2 3 4 5 6 7 8 9 10 ...
#- attr(*, "dimnames")=List of 2
#..$ : NULL
#..$ : chr "Resample1"

train_predictors <- predictors[train_set, ]
train_classes <- classes[train_set]
test_predictors <- predictors[-train_set, ]
test_classes <- classes[-train_set]

seed <- 1809
set.seed(seed)
cv_splits <- createFolds(classes, k = 10, returnTrain = TRUE)
str(cv_splits)

predictors <- as.data.frame(pacientesPartNorm[-c(5)])
modelLM <- train(predictors, classes, method = "lm", trControl = trainControl(method = "LOOCV"))
classes <- factor(ifelse(classes==0, "Zero", "One"))
modelADA <- train(predictors, classes, method = "LMT", trControl = trainControl(method = "LOOCV"))

library(class)

# 1nn with irisPart
knn1_todos <- knn.cv(pacientesPartNorm[,1:4], pacientesPartNorm$Respiracao0nasal1bucal, k = 1)
#computing accuracy
ac = sum(knn1_todos == pacientesPartNorm$Respiracao0nasal1bucal) / length(pacientesPartNorm$Respiracao0nasal1bucal)
ac
# error rate
er = 1.0 - ac
er
# Confusion matrix
# 1nn with iris (total)
table(pacientesPartNorm$Respiracao0nasal1bucal,knn1_todos)

# 1nn with irisPart
knn1_sem1 <- knn.cv(pacientesPartNorm[,2:4], pacientesPartNorm$Respiracao0nasal1bucal, k = 1)
#computing accuracy
ac = sum(knn1_sem1 == pacientesPartNorm$Respiracao0nasal1bucal) / length(pacientesPartNorm$Respiracao0nasal1bucal)
ac
# error rate
er = 1.0 - ac
er
# Confusion matrix
# 1nn with iris (total)
table(pacientesPartNorm$Respiracao0nasal1bucal,knn1_sem1)

# ROC curves
# Based in: https://www.youtube.com/watch?v=ypO1DPEKYFo

install.packages("ROCR")
library(ROCR)
install.packages("nnet")
library(nnet)

# fitting a multinomial linear model to the data
model <- multinom(Respiracao0nasal1bucal~.,pacientesPartNorm)
# predicting on the same data (in practice, you should use test data)
p <- predict(model,pacientesPartNorm)
# confusion matrix using a threshold cut of 0.5
table(pacientesPartNorm$Respiracao0nasal1bucal,p)

# now taking the probabilities of the predictions
pred <- predict(model,pacientesPartNorm,type="prob")
# comparing the probabilities with the classes of the first and last examples in irisPart
head(pred)
head(pacientesPartNorm)
tail(pred)
tail(pacientesPartNorm)

# getting the ROC curve
pred2 <- prediction(pred,pacientesPartNorm$Respiracao0nasal1bucal)
roc <- performance(pred2,"tpr","fpr")
plot(roc)
abline(a=0,b=1)

# computing the AUC value
auc <- performance(pred2,"auc")
auc <- unlist(slot(auc,"y.values"))
auc



