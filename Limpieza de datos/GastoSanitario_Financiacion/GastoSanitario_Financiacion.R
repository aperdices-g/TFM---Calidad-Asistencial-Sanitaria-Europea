#En primer lugar leemos el fichero:
gasto_f<-read.csv("C:/temp/GastoSanitario_Financiacion.csv",sep= ",")
head(gasto_f)
colnames(gasto_f)
nrow(gasto_f)
ncol(gasto_f)
gasto_f<-gasto_f[,-6]

#Breve inspección de los datos
str(gasto_f)
#Tendríamos que convertir la columna Value a numérico porque se ha cargado como factor.

gasto_f$Value<-as.character(gasto_f$Value)
gasto_f$Value<-as.numeric(gsub(",",".",gasto_f$Value))

head(gasto_f$Value)
table(gasto_f$Value, useNA = "ifany")
#Qué indices tienen valores NA
idx<-which(is.na(gasto_f$Value))
length(idx)

#Tenemos muchos datos desconocidos, veamos si nos quedamos solo con un rango de años en concreto 
library(ggplot2)
library(scales)
g = ggplot(gasto_f, aes(TIME, fill=is.na(Value)) ) +
labs(title = "Valores Nulos")+ylab("") +
theme(plot.title = element_text(size = rel(2), colour = "blue"))

g+geom_bar(position="dodge") + scale_fill_manual(values = alpha(c("orange", "blue"), 1)) +
theme(axis.title.x = element_text(face="bold", size=10)) 

#Podemos trabajar con los valores reales o imputar los na con la media o con el algoritmo KNN.
library(VIM)
selected_values_na<-which(is.na(gasto_f$Value))
names<-colnames(gasto_f)
paises<-gasto_f$GEO
selected_vars<-which((names=="Value"))
selected_rows<-which((paises=='Czechia'))
gasto_f[,selected_vars]
table(gasto_f$Value, useNA = "ifany")


output<-kNN(gasto_f, variable=c("Value"),k=3) 
gasto_f<-output

#Comprobamos gráficamente que no tenemos valores nulos.
g = ggplot(gasto_f, aes(TIME, fill=is.na(Value)) ) +
labs(title = "Valores Nulos")+ylab("") +
theme(plot.title = element_text(size = rel(2), colour = "blue"))

g+geom_bar(position="dodge") + scale_fill_manual(values = alpha(c("orange", "blue"), 1)) +
theme(axis.title.x = element_text(face="bold", size=10)) 

str(gasto_f)
#comprobamos para el resto de columnas si tenemos valores NA
table(gasto_f$TIME, useNA = "ifany")
table(gasto_f$GEO, useNA = "ifany")
table(gasto_f$UNIT, useNA = "ifany")
table(gasto_f$ICHA11_HF, useNA = "ifany")

#Comprobemos si tenemos outliers o valores anomalos en la variable Value

boxplot(gasto_f$Value, main="Value")
#Comprobamos que no existen outliers ni valores anómalos.

#Creamos otro fichero con toda la información corregida.

write.csv(gasto_f, file="GastoSanitario_Financiacion_clean.csv", row.names = FALSE)
