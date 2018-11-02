#Decision Tree
library(tree)
library(ggplot2)
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

#Creo el modelo

tree.model <- tree(as.factor(V1)~., data = train_set)
summary(tree.model)
#Misclassification error rate: 0.662 = 4862 / 7344   
tree.pred <- predict(tree.model,test_set,type="class")
confmatrix <- as.data.frame(table(tree.pred,test_set$V1))
#La exactitud es (430+334+272+400+423+537)/2947 = 0,8130302

#Grafico la matriz de confusión
ggplot(data = confmatrix,
       mapping = aes(x = tree.pred,
                     y = Var2)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "yellow",
                      high = "red",
                      trans = "log") +
  xlab("Valores Predichos") +
  ylab("Valores Reales") +
  ggtitle("MC para Decision Tree con features reducidos")

#Grafico el árbol
x11()
plot(tree.model)
text(tree.model, pretty = 0)

savePlot(filename = paste0("/home/luciakasman/Documentos/Aprendizaje Estadístico/TpFinal/Plots//Features Reducidos/Rplot."),
         type = "png",
         device = dev.cur())


#Cross Validation
cv.tree.model=cv.tree(tree.model,FUN=prune.misclass)
plot(cv.tree.model$size,cv.tree.model$dev,type="b", xlab = "Tamaño del árbol", ylab = "Tasa de error de validación cruzada",main = "Tamaño de árbol vs Error de validación cruzada")

#Cuál es el mejor tamaño?
best.size <- cv.tree.model$size[which(cv.tree.model$dev==min(cv.tree.model$dev))]
best.size
#Los mejores tamaños de árboles son 8 y 6.

#Hago nuevos árboles

#Árbol de tamaño 8
prune.tree.model <- prune.misclass(tree.model,best = 8)
summary(prune.tree.model)
#Misclassification error rate: 0,1159 = 852 / 7352 

#Grafico el árbol de tamaño 8
plot(prune.tree.model)
text(prune.tree.model, pretty = 0)

#Busco la matriz de confusión
tree.pred <- predict(prune.tree.model,test_set,type="class")
cm8 <- as.data.frame(table(tree.pred,test_set$V1))
#La precisión es (430+334+272+400+423+537)/2947 = 0,8130302

#Grafico la matriz de confusión
ggplot(data = cm8,
       mapping = aes(x = tree.pred,
                     y = Var2)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "yellow",
                      high = "red",
                      trans = "log") +
  xlab("Valores Predichos") +
  ylab("Valores Reales") +
  ggtitle("MC para DT con features reducidos de tam 8") 

#Árbol de tamaño 6
prune.tree.model2 <- prune.misclass(tree.model,best = 6)
summary(prune.tree.model2)
#Misclassification error rate: 0,1159 = 852 / 7352 
plot(prune.tree.model2)
text(prune.tree.model2, pretty = 0)

#Busco la matriz de confusión
tree.pred2 <- predict(prune.tree.model2,test_set,type="class")
cm6 <- as.data.frame(table(tree.pred2,test_set$V1))
#La precisión es (480+334+272+400+423+537)/2947 = 0,8299966

#Grafico la matriz de confusión
ggplot(data = cm6,
       mapping = aes(x = tree.pred2,
                     y = Var2)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "yellow",
                      high = "red",
                      trans = "log") +
  xlab("Valores Predichos") +
  ylab("Valores Reales") +
  ggtitle("MC para DT con features reducidos de tam 6")


#El árbol que da mejor es el de tamaño 6.


