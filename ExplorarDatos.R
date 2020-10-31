install.packages("gdata")
library("gdata")
mydata = read.csv("importacionesVehiculosSAT.csv")
names(mydata)

head(mydata["Valor.CIF"][,1])

summary(mydata)
