#Linear Discriminant Analysis
library(MASS)
library(caret)
#Leo el training set
train <- read.table("/home/luciakasman/Documentos/Aprendizaje Estadístico/TpFinal/UCI HAR Dataset/UCI HAR Dataset/train/X_train_pca.txt", header = TRUE)
labels.train <- read.table("/home/luciakasman/Documentos/Aprendizaje Estadístico/TpFinal/UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt")

#Reduzco los datos según los resultados de aplicar PCA
train <- train[,1:105]

train_set <- data.frame(cbind(labels.train, train))

#Leo el testing set
test <- read.table("/home/luciakasman/Documentos/Aprendizaje Estadístico/TpFinal/UCI HAR Dataset/UCI HAR Dataset/test/X_test_pca.txt", header = TRUE)
labels.test <- read.table("/home/luciakasman/Documentos/Aprendizaje Estadístico/TpFinal/UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt")

#Reduzco los datos según los resultados de aplicar PCA
test <- test[,1:105]

test_set <- data.frame(cbind(labels.test, test))

#Mezclo aleatoriamente las filas, porque el dataset dado está bastante ordenado
set.seed(452)
train_set <- train_set[sample(nrow(train_set)),]
test_set <- test_set[sample(nrow(test_set)),]

lda.model <- lda(train_set$V1~., data = train_set, cv = FALSE)

pred <- predict(lda.model, test_set)$class
CM_red <- as.data.frame(table(pred,test_set$V1))
#La exactitud es (495+428+369+351+470+537)/2947 = 0,8992195

#Grafico la matriz de confusión
ggplot(data = CM_red,
       mapping = aes(x = pred,
                     y = Var2)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "yellow",
                      high = "red",
                      trans = "log") +
  xlab("Valores Predichos") +
  ylab("Valores Reales") +
  ggtitle("MC para LDA con features reducidos")

#Usando Cross-Validation 
lda.model.cv <- lda(train_set$V1~., data = train_set, cv = TRUE)

pred.cv <- predict(lda.model.cv, test_set)$class
CM_red.cv <- as.data.frame(table(pred.cv,test_set$V1))
#La exactitud es (495+428+369+351+470+537)/2947 = 0,8992195

#Grafico la matriz de confusión
ggplot(data = CM_red.cv,
       mapping = aes(x = pred.cv,
                     y = Var2)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "yellow",
                      high = "red",
                      trans = "log") +
  xlab("Valores Predichos") +
  ylab("Valores Reales") +
  ggtitle("MC para LDA con features reducidos y CV")
