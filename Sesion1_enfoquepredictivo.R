#I. EJERCICIO 1:ENFOQUE PREDICTIVO 
#IMPACTO DEL APALANCAMIENTO AL DETERIORO DEL PORTAFOLIO DE
#MICROCREDITOS

#I.1 ABRIR EL ARCHIVO: df_deudaingreso.txt

#rm(df_file01)
path <- "D:/DATOS LURIVERO/C/ESCRITORIO/Cursos Credit Scoring Quantiplus/Sesión 1/Material/Sesion 1"
setwd(path)
df_file01 <- read.table(file="df_deudaingreso.txt",header=T,sep=",")

#I.2 VARIABLE DE RESPUESTA (Y)
#DETERIORO DEL PORTAFOLIO: MOROSIDAD

#CALCULO DE LA MORA MAXIMA
df_file01$MoraMax <- apply(df_file01[,9:14],1,FUN=function(x) max(x))

#CATEGORIZACION DE LA MORA
df_file01 <- within(df_file01,{
  MoraMax_cat=NA
  MoraMax_cat[MoraMax<=30]=1
  MoraMax_cat[MoraMax>30]=0
})

#FORMATO DE CATEGORIA DE Y
df_file01$MoraMax_cat <- factor(df_file01$MoraMax_cat,levels = c(0,1),
                                labels=c("Credito vencido","No Vencido"))
#str(df_file01)

#I.3 VARIABLE EXPLICATIVA (X)
#RATIO DE APALANCAMIENTO: X=DEUDA/INGRESO 

#DEUDA MAXIMA
df_file01$DeudaMax <- apply(df_file01[,3:8], 1,FUN=function(x) max(x))

#DEUDA MAXIMA / INGRESO
df_file01$DeudaMax_Ing <- round(
                          df_file01$DeudaMax/df_file01$Ingreso_Normalizado1512,
                          2)

#VECTOR DE PUNTOS DECILES DE: DEUDA MAXIMA / INGRESO
v1<- quantile(df_file01$DeudaMax_Ing[!is.na(df_file01$DeudaMax_Ing)], 
              probs=seq(0,1,.10), na.rm=T, names=F)[2:11]

v2<-c(paste0("<=",v1[1:9]),paste0(">",v1[9]))

#COLOCANDO CATEGORIAS SEGUN LOS CORTES DE LOS DECILES
df_file01$DeudaMax_Ing_cat=NA
for (i in 1:length(v1)) {
  df_file01$DeudaMax_Ing_cat[is.na(df_file01$DeudaMax_Ing_cat)
                             & df_file01$DeudaMax_Ing<=v1[i]]=v2[i]
}

#PONIENDOLE FORMATO DE DATA TABLE PARA LA AGREGACION
#LIBRERIA PARA CREAR TABLAS DE CONTINGENCIA
#install.packages("reshape2")
library(reshape2)

#TABLA CRUZADA: DEUDA/INGRESO CATEGORIZADA vs MORA MAXIMA CAT
df_temp <- dcast(df_file01[!is.na(df_file01$DeudaMax_Ing),], DeudaMax_Ing_cat~MoraMax_cat , 
                  fun.aggregate = function(x) sum(!is.na(x)), value.var = "ID")


#AGREGAMOS EL RATIO DE DETERIORO
df_temp$Porc_Vencido <- round(
                        df_temp$`Credito vencido`/(df_temp$`No Vencido`+df_temp$`Credito vencido`)
                        ,2)      
##APARTE CALCULAMOS EL RATIO DE DETERIORO GLOBAL (PARA TOMARLO COMO UMBRAL)
Porc_Vencido_Total<-round(
                    sum(df_temp$`Credito vencido`)/sum(df_temp$`No Vencido`+df_temp$`Credito vencido`)
                    ,2)  
#TABLA FINAL:     

df_temp<-rbind(df_temp,
               c("TOTAL",sum(df_temp$`Credito vencido`),
                 sum(df_temp$`No Vencido`),
                 Porc_Vencido_Total))

#CONCLUSION:


