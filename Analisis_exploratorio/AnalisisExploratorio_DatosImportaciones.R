# Universidad del Valle de Guatemala
# Data Science - Seccion 10
# Laboratorio 8 y 9
# Maria Fernanda Estrada 14198
# Christopher Sandoval 13660
# *
# *
# Analisis exploratorio de los archivos de improtaciones de la SAT
# Octubre/2020



# Librerias
install.packages("RColorBrewer")
library("RColorBrewer")
library(ggplot2)
library(corrplot)


# Set de datos
data_importaciones <- read.csv("importacionesVehiculosSAT.csv", stringsAsFactors = FALSE)

# Solo motos
data_importaciones_moto<-data_importaciones[(data_importaciones$Tipo.de.Vehiculo == "MOTO"),]


# ------- Analisis variables cuantitativas ------

# Separacion variables cuantitativas
data_numeric_moto <- data_importaciones_moto[, c("Modelo.del.Vehiculo", "Centimetros.Cubicos",  "Valor.CIF", "Impuesto", "Anio", "Mes", "Dia", "DiaSem")]

# Resumen
summary(data_numeric_moto)

# Histograma de modelo del vehiculo
ggplot(data_numeric_moto, aes(x = Modelo.del.Vehiculo)) +
  geom_histogram(aes(y = stat(count)), bins = 20, color="black", fill=append(brewer.pal(n = 12, name = "Set3"), brewer.pal(n = 8, name = "Set3"))) +
  xlim(c(2000, 2020))

# Histograma de centimetros cubicos
ggplot(data_numeric_moto, aes(x = Centimetros.Cubicos)) +
  geom_histogram(aes(y = stat(count)), bins = 20, color="black", fill=append(brewer.pal(n = 12, name = "Set3"), brewer.pal(n = 8, name = "Set3"))) +
  xlim(c(0, 300))

# Histograma de dia de la semana en que se importo
ggplot(data_numeric_moto, aes(x = DiaSem)) +
  geom_histogram(aes(y = stat(count)), bins = 7, color="black", fill=brewer.pal(n = 7, name = "Set3")) +
  xlim(c(1, 7))

# Histograma de mes de importacion
ggplot(data_numeric_moto, aes(x = Mes)) +
  geom_histogram(aes(y = stat(count)), bins = 12, color="black", fill=brewer.pal(n = 12, name = "Set3")) +
  xlim(c(1, 12))

# Caja y bigotes modelo vehiculo
ggplot(data_numeric_moto, aes(x = Modelo.del.Vehiculo)) +
  geom_boxplot()

# Caja y bigotes centimetros cubicos
ggplot(data_numeric_moto, aes(x = Centimetros.Cubicos)) +
  geom_boxplot()

# Caja y bigotes dia de la semana
ggplot(data_numeric_moto, aes(x = DiaSem)) +
  geom_boxplot()

# Caja y bigotes mes
ggplot(data_numeric_moto, aes(x = Mes)) +
  geom_boxplot()

# Matriz de correlacion
matriz_cor <- cor(data_numeric_moto)
corrplot(matriz_cor)


# ------- Analisis variables cualitativas ------

# Frecuencia variable pais de proveniencia
frecuenciaPais=table(data_importaciones_moto$Pais.de.Proveniencia)
df<-as.data.frame(frecuenciaPais)
head(df[order(-df$Freq),],10)

# Frecuencia variable aduana de ingreso
frecuenciaAduana=table(data_importaciones_moto$Aduana.de.Ingreso)
df<-as.data.frame(frecuenciaAduana)
head(df[order(-df$Freq),],10)

# Frecuencia variable fecha de poliza
frecuenciaFecha=table(data_importaciones_moto$Fecha.de.la.Poliza)
df<-as.data.frame(frecuenciaFecha)
head(df[order(-df$Freq),],30)

# Frecuencia variable marca
frecuenciaMarca=table(data_importaciones_moto$Marca)
df<-as.data.frame(frecuenciaMarca)
head(df[order(-df$Freq),],10)

# Frecuencia variable linea
frecuenciaLinea=table(data_importaciones_moto$Linea)
df<-as.data.frame(frecuenciaLinea)
head(df[order(-df$Freq),],30)
