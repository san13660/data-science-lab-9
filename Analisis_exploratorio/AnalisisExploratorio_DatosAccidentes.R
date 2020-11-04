# Universidad del Valle de Guatemala
# Data Science - Seccion 10
# Laboratorio 8 y 9
# Maria Fernanda Estrada 14198
# Christopher Sandoval 13660
# *
# *
# Analisis exploratorio de los archivos de accidentes
# Octubre/2020



# Librerias
library("haven")
library(ggplot2)
library(caret)
library(nnet)
library(RWeka)
library(neural)
library(dummy)
library(neuralnet)
library(e1071)


#------------------------- SET DE DATOS ---------------------------

# Se importan los datos y se estandarizan los nombres de las columnas.
# Tambien se unen los diferentes archivos en una sola tabla
# Se seleccionan solo el tipo de vehiculo 4 (MOTOCICLETA)

accidentes_train<-read_sav("DatosAccidentes/accidentes_2009.sav")
accidentes_train<-accidentes_train[,c("dia_ocu", "mes_ocu", "dia_sem_ocu", "hora_ocu", "depto_ocu", "sexo_pil", "edad_pil", "estado_pil", "tipo_vehi", "color_vehi", "causa_acc")]
colnames(accidentes_train) <- c("dia_ocu", "mes_ocu", "dia_sem_ocu", "hora_ocu", "depto_ocu", "sexo_pil", "edad_pil", "estado_pil", "tipo_veh", "color_veh", "causa_acc")
accidentes_train <- sapply(accidentes_train, as.numeric)

temp_dataset<-read_sav("DatosAccidentes/accidentes_2010.sav")
temp_dataset<-temp_dataset[,c("dia_ocu", "mes_ocu", "dia_sem_ocu", "hora_ocu", "depto_ocu", "sexo_pil", "edad_pil", "estado_pil", "tipo_v", "color_v", "causa_ac")]
colnames(temp_dataset) <- c("dia_ocu", "mes_ocu", "dia_sem_ocu", "hora_ocu", "depto_ocu", "sexo_pil", "edad_pil", "estado_pil", "tipo_veh", "color_veh", "causa_acc")
temp_dataset <- sapply(temp_dataset, as.numeric)

accidentes_train<-rbind(accidentes_train,temp_dataset)

temp_dataset<-read_sav("DatosAccidentes/accidentes_2011.sav")
temp_dataset<-temp_dataset[,c("dia_ocu", "mes_ocu", "dia_sem_ocu", "hora_ocu", "depto_ocu", "sexo_pil", "edad_pil", "estado_pil", "tipo_vehiculo", "color_vehi", "causa_acc")]
colnames(temp_dataset) <- c("dia_ocu", "mes_ocu", "dia_sem_ocu", "hora_ocu", "depto_ocu", "sexo_pil", "edad_pil", "estado_pil", "tipo_veh", "color_veh", "causa_acc")
temp_dataset <- sapply(temp_dataset, as.numeric)

accidentes_train<-rbind(accidentes_train,temp_dataset)

temp_dataset<-read_sav("DatosAccidentes/accidentes_2012.sav")
temp_dataset<-temp_dataset[,c("dia_ocu", "mes_ocu", "dia_sem_ocu", "hora_ocu", "depto_ocu", "sexo_pil", "edad_pil", "condicion_pil", "tipo_vehi", "color_vehi", "causa_acc")]
colnames(temp_dataset) <- c("dia_ocu", "mes_ocu", "dia_sem_ocu", "hora_ocu", "depto_ocu", "sexo_pil", "edad_pil", "estado_pil", "tipo_veh", "color_veh", "causa_acc")
temp_dataset <- sapply(temp_dataset, as.numeric)

accidentes_train<-rbind(accidentes_train,temp_dataset)

temp_dataset<-read_sav("DatosAccidentes/accidentes_2013.sav")
temp_dataset<-temp_dataset[,c("dia_ocu", "mes_ocu", "dia_sem_ocu", "hora_ocu", "depto_ocu", "sexo_pil", "edad_pil", "estado_pil", "tipo_veh", "color_veh", "causa_acc")]
colnames(temp_dataset) <- c("dia_ocu", "mes_ocu", "dia_sem_ocu", "hora_ocu", "depto_ocu", "sexo_pil", "edad_pil", "estado_pil", "tipo_veh", "color_veh", "causa_acc")
temp_dataset <- sapply(temp_dataset, as.numeric)

accidentes_train<-rbind(accidentes_train,temp_dataset)

temp_dataset<-read_sav("DatosAccidentes/accidentes_2014.sav")
temp_dataset<-temp_dataset[,c("día_ocu", "mes_ocu", "día_sem_ocu", "hora_ocu", "depto_ocu", "sexo_con", "edad_con", "estado_con", "tipo_veh", "color_veh", "tipo_eve")]
colnames(temp_dataset) <- c("dia_ocu", "mes_ocu", "dia_sem_ocu", "hora_ocu", "depto_ocu", "sexo_pil", "edad_pil", "estado_pil", "tipo_veh", "color_veh", "causa_acc")
temp_dataset <- sapply(temp_dataset, as.numeric)

accidentes_train<-rbind(accidentes_train,temp_dataset)

accidentes_train <- as.data.frame(accidentes_train)
accidentes_train<-accidentes_train[(accidentes_train$tipo_veh == 4),]



#-------------------------------- LIMPIEZA DE DATOS ----------------------------------

# Se quitan los daton invalidos o ignorados
accidentes_train<-accidentes_train[(accidentes_train$sexo_pil < 9),]
accidentes_train<-accidentes_train[(accidentes_train$edad_pil > 14),]
accidentes_train<-accidentes_train[(accidentes_train$edad_pil < 999),]
accidentes_train<-accidentes_train[(accidentes_train$estado_pil < 9),]
accidentes_train<-accidentes_train[(accidentes_train$color_veh < 99),]
accidentes_train<-accidentes_train[(accidentes_train$causa_acc < 13),]

accidentes_train <- na.omit(accidentes_train)



#---------------------------------- ANALISIS EXPLORATORIO ------------------------------------------------

# Resumen
summary(accidentes_train)

# Histograma variable mes ocurrencia
ggplot(accidentes_train, aes(x = mes_ocu)) +
  geom_histogram(aes(y = stat(count)), bins = 12, color="black", fill=brewer.pal(n = 12, name = "Set3")) +
  xlim(c(1, 12))

#Histograma variable dia de la semana
ggplot(accidentes_train, aes(x = dia_sem_ocu)) +
  geom_histogram(aes(y = stat(count)), bins = 7, color="black", fill=brewer.pal(n = 7, name = "Set3")) +
  xlim(c(1, 7))

# Histograma hora ocurrencia
ggplot(accidentes_train, aes(x = hora_ocu)) +
  geom_histogram(aes(y = stat(count)), bins = 24, color="black", fill=append(brewer.pal(n = 12, name = "Set3"), brewer.pal(n = 12, name = "Set3"))) +
  xlim(c(0, 23))

# Histograma departamento
ggplot(accidentes_train, aes(x = depto_ocu)) +
  geom_histogram(aes(y = stat(count)), bins = 22, color="black", fill=append(brewer.pal(n = 12, name = "Set3"), brewer.pal(n = 10, name = "Set3"))) +
  xlim(c(1, 22))

# Histograma color vehiculo
ggplot(accidentes_train, aes(x = color_veh)) +
  geom_histogram(aes(y = stat(count)), bins = 16, color="black", fill=append(brewer.pal(n = 12, name = "Set3"), brewer.pal(n = 4, name = "Set3"))) +
  xlim(c(1, 16))

# Histograma causa accidente
ggplot(accidentes_train, aes(x = causa_acc)) +
  geom_histogram(aes(y = stat(count)), bins = 5, color="black", fill=brewer.pal(n = 5, name = "Set3")) +
  xlim(c(1, 5))

# Caja y bigotes mes de ocurrencia
ggplot(accidentes_train, aes(x = mes_ocu)) +
  geom_boxplot()

# Caja y bigotes dia de la semana
ggplot(accidentes_train, aes(x = dia_sem_ocu)) +
  geom_boxplot()

# Caja y bigotes hora de ocurrencia
ggplot(accidentes_train, aes(x = hora_ocu)) +
  geom_boxplot()

# Matriz correlacion
matriz_cor <- cor(accidentes_train)
corrplot(matriz_cor)

# Frecuencia mes de ocurrencia
frecuenciaPais=table(accidentes_train$mes_ocu)
df<-as.data.frame(frecuenciaPais)
head(df[order(-df$Freq),],12)

# Frecuencia dia de la semana
frecuenciaPais=table(accidentes_train$dia_sem_ocu)
df<-as.data.frame(frecuenciaPais)
head(df[order(-df$Freq),],7)

# Frecuencia hora de ocurrencia
frecuenciaPais=table(accidentes_train$hora_ocu)
df<-as.data.frame(frecuenciaPais)
head(df[order(-df$Freq),],24)

# Frecuencia sexo del piloto
frecuenciaPais=table(accidentes_train$sexo_pil)
df<-as.data.frame(frecuenciaPais)
head(df[order(-df$Freq),],2)



#----------------------------------CREACION DE MODELOS-------------------------------

# Modelo 1 usando caret y nnet
modeloCaret <- train(estado_pil~., data=accidentes_train, method="nnet", trace=F)

prediccion_caret<-predict(modeloCaret, newdata = accidentes_test[,2:7])

cfmCaret<-confusionMatrix(prediccion_caret,accidentes_test$estado_pil)
cfmCaret

modeloCaret

# Modelo 2 usando caret y pcaNNet

modeloCaretPCANNet <- train(estado_pil~., data=accidentes_train, method="pcaNNet", trace=F)

prediccion_caret<-predict(modeloCaretPCANNet, newdata = accidentes_test[,2:7])

cfmCaretPCANNet<-confusionMatrix(prediccion_caret,accidentes_test$estado_pil)
cfmCaretPCANNet

modeloCaretPCANNet

# Modelo 2 usando caret y pcaNNet

modeloSVM<-svm(estado_pil~sexo_pil+dia_sem_ocu+edad_pil+color_veh+tipo_veh,data=accidentes_train )

pred<-predict(modeloSVM,accidentes_test)
cfmSVM<-confusionMatrix(pred,accidentes_test$estado_pil)
cfmSVM

modeloSVM
