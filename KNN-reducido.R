library(class)

#Función para estandarizar las variables:
estandarizacion <- function(x){
  
  x = as.numeric(x)
  aux <- (x-min(x))/(max(x)-min(x))
  return(aux)
}

#Leo el training set
train <- read.table("/home/luciakasman/Documentos/Aprendizaje Estadístico/TpFinal/UCI HAR Dataset/UCI HAR Dataset/train/X_train_pca.txt", header = TRUE)
labels.train <- read.table("/home/luciakasman/Documentos/Aprendizaje Estadístico/TpFinal/UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt")

#Estandarizo el training set
train_est <- as.data.frame(lapply(train[1:NCOL(train)], estandarizacion))

#Reduzco los datos según los resultados de aplicar PCA
train_est <- train_est[,1:105]

train_set <- data.frame(cbind(labels.train, train_est))

#Leo el testing set
test <- read.table("/home/luciakasman/Documentos/Aprendizaje Estadístico/TpFinal/UCI HAR Dataset/UCI HAR Dataset/test/X_test_pca.txt", header = TRUE)
labels.test <- read.table("/home/luciakasman/Documentos/Aprendizaje Estadístico/TpFinal/UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt")

#Estandarizo el testing set
test_est <- as.data.frame(lapply(test[1:NCOL(test)], estandarizacion))

#Reduzco los datos según los resultados de aplicar PCA
test_est <- test_est[,1:105]

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
#El costo es 0,1370886, por lo que la precisión es 0,8629114

#Con k=13
knn.pred5 <- knn(train.data, test.data, train.labels, k = 13)

predicted.vs.real5 <- cbind(test.labels, knn.pred5)

costo5 <- total.cost(predicted.vs.real5)
#El costo es 0,1326773, por lo que la precisión es 0,8673227

#Con k = 15
knn.pred3 <- knn(train.data, test.data, train.labels, k = 15)

predicted.vs.real3 <- cbind(test.labels, knn.pred3)

costo3 <- total.cost(predicted.vs.real3)
#El costo es 0,133356, por lo que la precisión es 0,866644

#Con k = 17
knn.pred2 <- knn(train.data, test.data, train.labels, k = 17)

predicted.vs.real2 <- cbind(test.labels, knn.pred2)

costo2 <- total.cost(predicted.vs.real2)
#El costo es 0,1326773, por lo que la precisión es 0,8673227

#Con k=20
knn.pred <- knn(train.data, test.data, train.labels, k = 20)

predicted.vs.real <- cbind(test.labels, knn.pred)

costo <- total.cost(predicted.vs.real)
#El costo es 0,1347133, por lo que la precisión es 0,8652867

#La mejor precisión es 0.8673227, dada con k = 13 y k = 17.

#Grafico los valores de K vs. Accuracy
valores_k_red <- c(10, 13, 15, 17, 20)
acc_red <- c(0.8629114, 0.8673227, 0.866644, 0.8673227, 0.8652867)

x11()
k.vs.acc_red <- cbind(valores_k_red, acc_red)
plot(k.vs.acc_red, xlab = "Valores de K", ylab = "Accuracy", main = "Valores de K vs. Exactitud")

v1 <- c(10, 13, 15, 17, 20)
v2 <- c("10", "13", "15", "17", "20")

axis(side = 1, 
     at = v1, 
     labels = v2,
     tck=-.05)

savePlot(filename = paste0("/home/luciakasman/Documentos/Aprendizaje Estadístico/TpFinal/Plots/Features Reducidos/Rplot."),
         type = "png",
         device = dev.cur())


#Knn con cross validation

knn.cv <- knn.cv(train.data, train.labels, k = 13)
predicted.vs.real.cv <- cbind(test.labels, knn.cv)

costo.cv <- total.cost(predicted.vs.real.cv)
costo.cv


library(caret)
trControl <- trainControl(method  = "cv",
                          number  = 5)

knn.fit <- train(as.factor(V1) ~ .,
             method     = "knn",
             tuneGrid   = expand.grid(k = 1:20),
             trControl  = trControl,
             metric     = "Accuracy",
             data = train_set)

#El mejor k es 5, con exactitud 0,9287257
