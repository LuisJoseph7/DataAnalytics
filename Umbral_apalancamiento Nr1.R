#importar data
path <- "F:/Quantiplus/Scoring/Sesion 1"
path <- "D:/DATOS LURIVERO/C/ESCRITORIO/Umbral de apalancamiento/BD correcto despues de pruebas"
setwd(path)
df_file02 <- read.table(file="BD_umbral_corr_abr_nov_csv.csv",header=T,sep="|")

#I.2 VARIABLE DE RESPUESTA (Y)
#DETERIORO DEL PORTAFOLIO: MOROSIDAD

#CALCULO DE LA MORA MAXIMA
df_file02$MoraMax <- apply(df_file02[,11:18],1,FUN=function(x) max(x))

#CATEGORIZACION DE LA MORA
df_file02 <- within(df_file02,{
  MoraMax_cat=NA
  MoraMax_cat[MoraMax<=30]=1
  MoraMax_cat[MoraMax>30]=0
})

#FORMATO DE CATEGORIA DE Y
df_file02$MoraMax_cat <- factor(df_file02$MoraMax_cat,levels = c(0,1),
                                labels=c("No cumple","Cumple"))
#str(df_file01)

#I.3 VARIABLE EXPLICATIVA (X)
#RATIO DE APALANCAMIENTO: X=DEUDA/INGRESO 

#DEUDA MAXIMA
df_file02$DeudaMax <- apply(df_file02[,3:10], 1,FUN=function(x) max(x))

#DEUDA MAXIMA / INGRESO
df_file02$DeudaMax_Ing <- round(
  df_file02$DeudaMax/df_file02$Ingresos.mensuales,
  2)

#VECTOR DE PUNTOS DECILES DE: DEUDA MAXIMA / INGRESO
v1<- quantile(df_file02$DeudaMax_Ing[!is.na(df_file02$DeudaMax_Ing)], 
              probs=seq(0,1,.10), na.rm=T, names=F)[2:11]

# SE CONCATENA CON LA FUN paste0
v2<-c(paste0("<=",v1[1:9]),paste0(">",v1[9]))

#COLOCANDO CATEGORIAS SEGUN LOS CORTES DE LOS DECILES
df_file02$DeudaMax_Ing_cat=NA
for (i in 1:length(v1)) {
  df_file02$DeudaMax_Ing_cat[is.na(df_file02$DeudaMax_Ing_cat)
                             & df_file02$DeudaMax_Ing<=v1[i]]=v2[i]
}

#como se evalúa dos variables categoricas, con tablas doble entrada
#identificar si la variable (X) puede discriminar a la variable (Y)

#PONIENDOLE FORMATO DE DATA TABLE PARA LA AGREGACION
#LIBRERIA PARA CREAR TABLAS DE CONTINGENCIA
#install.packages("reshape2")
library(reshape2)

#TABLA CRUZADA: DEUDA/INGRESO CATEGORIZADA vs MORA MAXIMA CAT
df_temp2 <- dcast(df_file02[!is.na(df_file02$DeudaMax_Ing),], DeudaMax_Ing_cat~MoraMax_cat , 
                 fun.aggregate = function(x) sum(!is.na(x)), value.var = "Nro.de.operación")


#AGREGAMOS EL RATIO DE DETERIORO
df_temp2$Porc_Vencido <- round(
  df_temp2$`No cumple`/(df_temp2$`Cumple`+df_temp2$`No cumple`)
  ,2)      
##APARTE CALCULAMOS EL RATIO DE DETERIORO GLOBAL (PARA TOMARLO COMO UMBRAL)
Porc_Vencido_Total<-round(
  sum(df_temp2$`No cumple`)/sum(df_temp2$`Cumple`+df_temp2$`No cumple`)
  ,2)  
#TABLA FINAL:     

df_temp2<-rbind(df_temp2,
               c("TOTAL",sum(df_temp2$`No cumple`),
                 sum(df_temp2$`Cumple`),
                 Porc_Vencido_Total))

#CONCLUSION:

