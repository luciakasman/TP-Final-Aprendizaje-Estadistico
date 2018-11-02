#Random Forest
library(randomForest)
library(caret)

#Leo el training set
train <- read.table("/home/luciakasman/Documentos/Aprendizaje Estadístico/TpFinal/UCI HAR Dataset/UCI HAR Dataset/train/X_train_pca.txt", header = TRUE)
labels.train <- read.table("/home/luciakasman/Documentos/Aprendizaje Estadístico/TpFinal/UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt")

labels.train <- as.factor(unlist(labels.train))

#Reduzco los datos según los resultados de aplicar PCA
train <- train[,1:105]

train_set <- data.frame(cbind(labels.train, train))

#Leo el testing set
test <- read.table("/home/luciakasman/Documentos/Aprendizaje Estadístico/TpFinal/UCI HAR Dataset/UCI HAR Dataset/test/X_test_pca.txt", header = TRUE)
labels.test <- read.table("/home/luciakasman/Documentos/Aprendizaje Estadístico/TpFinal/UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt")

labels.test <- as.factor(unlist(labels.test))

#Reduzco los datos según los resultados de aplicar PCA
test <- test[,1:105]

test_set <- data.frame(cbind(labels.test, test))

#Mezclo aleatoriamente las filas, porque el dataset dado está bastante ordenado
set.seed(452)
train_set <- train_set[sample(nrow(train_set)),]
test_set <- test_set[sample(nrow(test_set)),]

rf.model.class <- randomForest(train_set$labels.train~., data=train_set, importance=TRUE)

rf.pred <- predict(rf.model.class, test_set)
CM.RandomForest <- as.data.frame(table(rf.pred, test_set$labels.test))
#La exactitud es: (472+399+350+395+435+537)/2947 = 0,8781812

ggplot(data = CM.RandomForest,
       mapping = aes(x = rf.pred,
                     y = Var2)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "yellow",
                      high = "red",
                      trans = "log") +
  xlab("Valores Predichos") +
  ylab("Valores Reales") +
  ggtitle("MC para Random Forest con features reduc")


#No se puede aplicar Cross Validation por falta de memoria en la computadora.
