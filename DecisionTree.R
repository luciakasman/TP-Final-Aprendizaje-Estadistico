#Decision Tree
library(tree)
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
set.seed(23)
train_set <- train_set[sample(nrow(train_set)),]
test_set <- test_set[sample(nrow(test_set)),]


#Creo el modelo
tree.model <- tree(as.factor(train_set$V1)~., data = train_set)
summary(tree.model)
#Misclassification error rate: 0,09725 = 715 / 7352 
tree.pred <- predict(tree.model,test_set,type="class")
confmatrix <- as.data.frame(table(tree.pred,test_set$V1))
#La exactitud es (425+405+272+400+425+537)/2947 = 0,8361045

#Grafico la matriz de confusión
ggplot(data = confmatrix,
       mapping = aes(x = tree.pred,
                     y = Var2)) +
  geom_tile(aes(fill = Freq)) +
  geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "blue",
                      high = "red",
                      trans = "log") +
  xlab("Valores Predichos") +
  ylab("Valores Reales") +
  ggtitle("Matriz de confusión para Decision Tree")

#Grafico el árbol
x11()
plot(tree.model)
text(tree.model, pretty = 0)

savePlot(filename = paste0("/home/luciakasman/Documentos/Aprendizaje Estadístico/TpFinal/Plots/Rplot."),
         type = "png",
         device = dev.cur())
