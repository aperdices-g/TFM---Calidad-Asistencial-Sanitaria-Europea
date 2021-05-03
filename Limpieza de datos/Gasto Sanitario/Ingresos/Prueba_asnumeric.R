

ingreso<-read.csv("C:/temp/IngresosSanitario_Financiacion.csv",sep= ",")
print(ingreso$Value)
ingreso$Value<-as.character(ingreso$Value )
ingreso$Value<-(gsub(',','.',ingreso$Value) )
ingreso$Value<-substr(ingreso$Value,1,nchar(ingreso$Value)-3)
ingreso$Value<-as.numeric(ingreso$Value)

print(ingreso$Value)

str(ingreso)
