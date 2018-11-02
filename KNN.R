library(class)

#Función para estandarizar las variables:
estandarizacion <- function(x){
  
  aux <- (x-min(x))/(max(x)-min(x))
  return(aux)
}

#Leo el training set
train <- read.table("/home/luciakasman/Documentos/Aprendizaje Estadístico/TpFinal/UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt")
labels.train <- read.table("/home/luciakasman/Documentos/Aprendizaje Estadístico/TpFinal/UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt")

#Estandarizo el training set
train_est <- as.data.frame(lapply(train[1:NCOL(train)], estandarizacion))

train_set <- data.frame(cbind(labels.train, train_est))

#Leo el testing set
test <- read.table("/home/luciakasman/Documentos/Aprendizaje Estadístico/TpFinal/UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt")
labels.test <- read.table("/home/luciakasman/Documentos/Aprendizaje Estadístico/TpFinal/UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt")

#Estandarizo el testing set
test_est <- as.data.frame(lapply(test[1:NCOL(test)], estandarizacion))

test_set <- data.frame(cbind(labels.test, test_est))

#Mezclo aleatoriamente las filas, porque el dataset dado está bastante ordenado
set.seed(8)
train_set <- train_set[sample(nrow(train_set)),]
test_set <- test_set[sample(nrow(test_set)),]


#KNN 

#Separo los datos y los labels
train.labels <- train_set[,1]
train.data <- train_set[2:NCOL(train_set)]

test.labels <- test_set[,1]
test.data <- test_set[2:NCOL(test_set)]

#Cálculo del costo
total.cost <- function (predicted_vs_real){
  cost <- 0
  for(i in 1:nrow(predicted_vs_real)){
    if(predicted_vs_real[i,1] != predicted_vs_real[i,2]){
      cost <- cost + 1
    }
  } 
  return(cost/nrow(predicted_vs_real))
}

#Con k=10
knn.pred4 <- knn(train.data, test.data, train.labels, k = 10)

predicted.vs.real4 <- cbind(test.labels, knn.pred4)

costo4 <- total.cost(predicted.vs.real4)
#El costo es 0.1150322, por lo que la precisión es 0.8849678


#Con k = 13
knn.pred2 <- knn(train.data, test.data, train.labels, k = 13)

predicted.vs.real2 <- cbind(test.labels, knn.pred2)

costo2 <- total.cost(predicted.vs.real2)
#El costo es 0.1119783, por lo que la precisión es 0.8880217


#Con k = 15
knn.pred3 <- knn(train.data, test.data, train.labels, k = 15)

predicted.vs.real3 <- cbind(test.labels, knn.pred3)

costo3 <- total.cost(predicted.vs.real3)
#El costo es 0.110621, por lo que la precisión es 0.889379


#Con k=17
knn.pred5 <- knn(train.data, test.data, train.labels, k = 17)

predicted.vs.real5 <- cbind(test.labels, knn.pred5)

costo5 <- total.cost(predicted.vs.real5)
#El costo es 0.109603, por lo que la precisión es 0.890397


#Con k=20
knn.pred <- knn(train.data, test.data, train.labels, k = 20)

predicted.vs.real <- cbind(test.labels, knn.pred)

costo <- total.cost(predicted.vs.real)
#El costo es 0.1136749, por lo que la precisión es 0.8863251

#La mejor precisión es 0.890397, dada con k = 17.

#Grafico los valores de K vs. Accuracy
valores_k <- c(10, 13, 15, 17, 20)
acc <- c(0.8849678, 0.8880217, 0.889379, 0.890397, 0.8863251)

x11()
k.vs.acc <- cbind(valores_k, acc)
plot(k.vs.acc, xlab = "Valores de K", ylab = "Accuracy", main = "Valores de K vs. Exactitud")

v1 <- c(10, 13, 15, 17, 20)
v2 <- c("10", "13", "15", "17", "20")

axis(side = 1, 
     at = v1, 
     labels = v2,
     tck=-.05)

savePlot(filename = paste0("/home/luciakasman/Documentos/Aprendizaje Estadístico/TpFinal/Plots/Rplot."),
         type = "png",
         device = dev.cur())







