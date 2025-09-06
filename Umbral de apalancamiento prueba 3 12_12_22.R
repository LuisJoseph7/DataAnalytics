#I. EJERCICIO 1:ENFOQUE PREDICTIVO 
#IMPACTO DEL APALANCAMIENTO AL DETERIORO DEL PORTAFOLIO DE
#MICROCREDITOS

#I.1 ABRIR EL ARCHIVO: df_deudaingreso.txt

#rm(df_file01)
path <- "D:/DATOS LURIVERO/C/ESCRITORIO/Umbral de apalancamiento/BD 2 correcto despues de pruebas"
setwd(path)
df_file03 <- read.table(file="BD_2_Umbral_ok_csv.csv",header=T,sep="|")

# ver resumen de los resultados de ingreson con la fun
summary(df_file03$IngresoM)

#resultados de resumen IngresosM
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#200    1020    1732    2314    3018   10000 

hist(df_file03$IngresoM)

#resumen de saldos a noviembre
summary(df_file03$USD.Final.noviembre)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0    1457    3360    4959    6458  195788 }

hist(df_file03$USD.Final.noviembre)

#comparativa de resultados

boxplot(df_file03$IngresoM,df_file03$USD.Final.noviembre)


#I.2 VARIABLE DE RESPUESTA (Y)
#DETERIORO DEL PORTAFOLIO: MOROSIDAD

str(df_file03)

#CALCULO DE LA MORA MAXIMA
df_file03$MoraMax <- apply(df_file03[,11:18],1,FUN=function(x) max(x))

#CATEGORIZACION DE LA MORA
df_file03 <- within(df_file03,{
  MoraMax_cat=NA
  MoraMax_cat[MoraMax<=30]=1
  MoraMax_cat[MoraMax>30]=0
})

#FORMATO DE CATEGORIA DE Y
df_file03$MoraMax_cat <- factor(df_file03$MoraMax_cat,levels = c(0,1),
                                labels=c("Credito vencido","No Vencido"))
str(df_file03)

#I.3 VARIABLE EXPLICATIVA (X)
#RATIO DE APALANCAMIENTO: X=DEUDA/INGRESO 

#DEUDA MAXIMA
df_file03$DeudaMax <- apply(df_file03[,3:10], 1,FUN=function(x) max(x))

#DEUDA MAXIMA / INGRESO
df_file03$DeudaMax_Ing <- round(
  df_file03$DeudaMax/df_file03$IngresoM,
  2)


#VECTOR DE PUNTOS DECILES DE: DEUDA MAXIMA / INGRESO
v1<- quantile(df_file03$DeudaMax_Ing[!is.na(df_file03$DeudaMax_Ing)], 
              probs=seq(0,1,.10), na.rm=T, names=F)[2:11]

v2<-c(paste0("<=",v1[1:9]),paste0(">",v1[9]))

#COLOCANDO CATEGORIAS SEGUN LOS CORTES DE LOS DECILES
df_file03$DeudaMax_Ing_cat=NA
for (i in 1:length(v1)) {
  df_file03$DeudaMax_Ing_cat[is.na(df_file03$DeudaMax_Ing_cat)
                             & df_file03$DeudaMax_Ing<=v1[i]]=v2[i]
}

#PONIENDOLE FORMATO DE DATA TABLE PARA LA AGREGACION
#LIBRERIA PARA CREAR TABLAS DE CONTINGENCIA
#install.packages("reshape2")
library(reshape2)

#TABLA CRUZADA: DEUDA/INGRESO CATEGORIZADA vs MORA MAXIMA CAT
df_temp3 <- dcast(df_file03[!is.na(df_file03$DeudaMax_Ing),], DeudaMax_Ing_cat~MoraMax_cat , 
                 fun.aggregate = function(x) sum(!is.na(x)), value.var = "Nro.de.operación")


#AGREGAMOS EL RATIO DE DETERIORO
df_temp3$Porc_Vencido <- round(
  df_temp3$`Credito vencido`/(df_temp3$`No Vencido`+df_temp3$`Credito vencido`)
  ,2)      
##APARTE CALCULAMOS EL RATIO DE DETERIORO GLOBAL (PARA TOMARLO COMO UMBRAL)
Porc_Vencido_Total<-round(
  sum(df_temp3$`Credito vencido`)/sum(df_temp3$`No Vencido`+df_temp3$`Credito vencido`)
  ,2)  
#TABLA FINAL:     

df_temp3<-rbind(df_temp3,
               c("TOTAL",sum(df_temp3$`Credito vencido`),
                 sum(df_temp3$`No Vencido`),sum(df_temp3$`NA`),
                 Porc_Vencido_Total))

#CONCLUSION:

