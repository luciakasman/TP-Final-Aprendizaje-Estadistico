#Linear Discriminant Analysis
library(MASS)
library(caret)
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

lda.model <- lda(train_set$V1~., data = train_set)

pred <- predict(lda.model, test_set)$class
CM <- as.data.frame(table(pred,test_set$V1))
#La exactitud es (490+460+405+434+510+537)/2947 = 0,9623346

#Grafico la matriz de confusión
ggplot(data = CM,
       mapping = aes(x = pred,
                     y = Var2)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "blue",
                      high = "red",
                      trans = "log") +
  xlab("Valores Predichos") +
  ylab("Valores Reales") +
  ggtitle("Matriz de confusión para LDA")

