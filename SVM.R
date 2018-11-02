#Support Vector Machine
library(e1071)
library(ggplot2)
#Leo el training set
train <- read.table("/home/luciakasman/Documentos/Aprendizaje Estadístico/TpFinal/UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt")
labels.train <- read.table("/home/luciakasman/Documentos/Aprendizaje Estadístico/TpFinal/UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt")

train_set <- data.frame(cbind(labels.train, train))

#Leo el testing set
test <- read.table("/home/luciakasman/Documentos/Aprendizaje Estadístico/TpFinal/UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt")
labels.test <- read.table("/home/luciakasman/Documentos/Aprendizaje Estadístico/TpFinal/UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt")

test_set <- data.frame(cbind(labels.test, test))

#Mezclo aleatoriamente las filas, porque el dataset dado está bastante ordenado
set.seed(452)
train_set <- train_set[sample(nrow(train_set)),]
test_set <- test_set[sample(nrow(test_set)),]

svm_model<- svm(train_set$V1~., data=train_set, cost=10,scale = FALSE, type="C-classification")

summary(svm_model)
#Se usan 1505 support vectors.

#Ahora predigo con el mejor modelo
test <- test_set[,-1]
Y2 <- test_set[,1]

datos_ajustados <- predict(svm_model,test)
confusion_matrix = as.data.frame(table(datos_ajustados, Y2))
#La exactitud es: (489+451+393+437+506+537)/2947 = 0,95453

ggplot(data = confusion_matrix,
       mapping = aes(x = datos_ajustados,
                     y = Y2)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "blue",
                      high = "red",
                      trans = "log") +
  xlab("Valores Predichos") +
  ylab("Valores Reales") +
  ggtitle("Matriz de confusión para SVM")
