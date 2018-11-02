#Feature Selection con PCA

#Leo el training set
train <- read.table("/home/luciakasman/Documentos/Aprendizaje Estadístico/TpFinal/UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt")
labels.train <- read.table("/home/luciakasman/Documentos/Aprendizaje Estadístico/TpFinal/UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt")

train_set <- data.frame(cbind(labels.train, train))

#Leo el testing set
test <- read.table("/home/luciakasman/Documentos/Aprendizaje Estadístico/TpFinal/UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt")
labels.test <- read.table("/home/luciakasman/Documentos/Aprendizaje Estadístico/TpFinal/UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt")

test_set <- data.frame(cbind(labels.test, test))

#Uno todos los datos
datos <- rbind(train_set,test_set)

#Mezclo aleatoriamente las filas, porque el dataset dado está bastante ordenado
set.seed(452)
datos <- datos[sample(nrow(datos)),]


#PCA
pca <- prcomp(datos , scale=TRUE)

pca.var <- pca$sdev^2
pca.pvar <- pca.var/sum(pca.var)

x11()
plot(cumsum(pca.pvar),xlab="Componentes Principales", ylab="Proporción de Varianza Explicada",type='b',main="Proporciones de los Componentes Principales",col="red")
abline(h=0.95)
abline(v=105)

savePlot(filename = paste0("/home/luciakasman/Documentos/Aprendizaje Estadístico/TpFinal/Plots/Rplot."),
         type = "png",
         device = dev.cur())

#En el gráfico se puede ver que el 95% de 
#la varianza puede ser explicada por los 
#primeros 105 componentes principales.

df <- data.frame(pca$x)

#Divido la misma cantidad de filas que tenia el training set (7352) y el testing set (2947).

train_set_pca <- df[1:7352,]
test_set_pca <- df[7353:10299,]

#Escribo nuevos archivos con los nuevos datos
write.table(train_set_pca, "/home/luciakasman/Documentos/Aprendizaje Estadístico/TpFinal/UCI HAR Dataset/UCI HAR Dataset/train/X_train_pca.txt")

write.table(test_set_pca, "/home/luciakasman/Documentos/Aprendizaje Estadístico/TpFinal/UCI HAR Dataset/UCI HAR Dataset/test/X_test_pca.txt")
