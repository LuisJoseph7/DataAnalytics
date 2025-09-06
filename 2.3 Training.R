
library(reshape2)
#install.packages("plyr")
library(plyr)
library(data.table)

###I. FABRICACION DEL MODELO DE SCORE:

##I.1 ABRIENDO LA DATA TRAINING:
#PORTAFOLIO DE TARJETAS DE CREDITO EN ATRASO AL CIERRE DE 201507

path<-"F:/Quantiplus/Scoring/Sesion 3"
setwd(path)

df_training <- read.table(file="df_training_collection1507.txt", header=T, sep=",")

#str(df_training)

##I.2 DANDOLE FORMATO DE VARIABLE CATEGORICA A NUESTRA VARIABLE TARGET:

df_training$Flag_Cliente <- factor(df_training$Flag_Cliente,levels=c(0,1),
                                labels=c("NoRecovery","Recovery"))

##I.3 TRANSFORMACION DE VARIABLES ADICIONALES

#I.3.1 UTILIZACION TC

df_training$UtilizacionTC = 100*(df_training$DeudaTC)/(df_training$LineaTC)

#I.3.2 MORA MAXIMA ULTIMOS 6 MESES

df_training$Moramaxima= apply(df_training[,7:12],1,FUN=function(x) max(x))

#I.3.3 MORA PROMEDIO PONDERADA ULTIMOS 6 MESES

df_training$Moraprompond<-data.matrix(df_training[,7:12])%*%matrix(1:6,6,1)*(1/21)

#I.3.3 VECES MORA>0

df_training$Vecesmora0= apply(df_training[,7:12],1,FUN=function(x) length(which(x>0)))

#I.3.4 VECES MORA>30

df_training$Vecesmora30= apply(df_training[,8:13],1,FUN=function(x) length(which(x>30)))
 
#I.3.5 DEUDA ACTUAL / DEUDA MAXIMA

df_training$Deuda_actymax = (df_training$DeudaTC)/df_training$SaldoTotal_maximo_u12m

#colnames(df_training)

#CREANDO UN DATA TABLE TEMPORAL
#library(data.table)   install.packages("data.table")
dt_temp1 <- data.table(df_training)

##I.4 Tabla de variables: 

df_fields <- data.frame(ID_var=1:16,
                        variable= c("Edad","Genero","Flag_Upgrade",
                                    "MOB","NroPagosMensuales_u6m",
                                    "FlagRefRCC_u12m","ClasRCC_u12m","NroEntRCC_TC",
                                    "NroTransacciones_PagaLaMitad_u12m",
                                    "NroTransacciones_DispEfectivo_u12m",
                                    "UtilizacionTC","Moramaxima","Moraprompond",
                                    "Vecesmora0","Vecesmora30","Deuda_actymax"
                                    ), stringsAsFactors = F)

#str(df_fields)

##I.6 CHAR ANALYSIS

n <- nrow(dt_temp1)
n_var <- nrow(df_fields)

for (i in 1:n_var) {
  
  dt_temp2 <- dt_temp1[,list(ID,x=get(df_fields$variable[ID_var=i]), Flag_Cliente)]
  dt_temp2 <- dt_temp2[!is.na(x),]
  
  v1<- quantile(dt_temp2$x, probs = seq(0,1,.10),na.rm = T, names = F)[2:11]
  dt_temp2$cat<-NA
  for (j in 1:length(v1)) {
    dt_temp2 <- within(dt_temp2,{
      cat[is.na(cat) & x<=v1[j]]=j})
  }
  
    df_table0 <- dcast(dt_temp2, cat~Flag_Cliente, fun.aggregate = function(x) sum(!is.na(x)), value.var = "ID")
    df_table0 <- cbind(df_table0, Recov_rel=round(100*cumsum(prop.table(df_table0$Recovery)),1),
                       NoRecov_rel=round(100*cumsum(prop.table(df_table0$NoRecovery)),1)  ,
                       Diff=abs(round(100*(cumsum(prop.table(df_table0$Recovery))-cumsum(prop.table(df_table0$NoRecovery))),1)))
    df_table1 <- data.frame(ID=i,variable=df_fields$variable[i])
    df_table1$KS <- max(df_table0$Diff)
    
    if (i==1) {df_varfinal <- df_table1}
    else {df_varfinal <- rbind(df_varfinal,df_table1)}
  
    }

#EL CAMPO "variable" a formato char
df_varfinal$variable <- as.character(df_varfinal$variable)
#str(df_varfinal)

#I.6 TABLA DE VARIABLES CON BUEN PERFORMANCE INDIVIDUAL

df_temp1<-data.frame(dt_temp1[, 
          mget(df_varfinal$variable[df_varfinal$KS>=20])])

t<-length(colnames(df_temp1))

df_varperf<- data.frame(id_var=1:t,var=colnames(df_temp1))

#I.7 MULTICOLINEALIDAD

#EL CAMPO "var" a formato char
df_varperf$var <- as.character(df_varperf$var)

for (k in 1:t) {
  p=k+1
  while (p<=t) {
    
    df_temp <- df_temp1[!is.na(df_temp1[,k]) & !is.na(df_temp1[,p]),]    
    df_cor <- data.frame(var1=k,var2=p, CoefCorr= cor(df_temp[,k],df_temp[,p],method = "pearson"))
    
    if (p==2) {df_varcor<- df_cor} else {df_varcor<-rbind(df_varcor,df_cor)}
    p=p+1
  }
}

matrixcorr <- dcast(df_varcor, var1~var2,
                    fun.aggregate = function(x) mean(x), 
                    value.var = "CoefCorr")

##I.8 EXCLUSION DE VARIABLES REDUNDANTES (CRITERIO DE REDUNDANCIA / NEGOCIO)

#TABLA CON VARIABLES FINALES:  

df_model <- data.frame(dt_temp1[,mget(df_varperf$var[c(1:2,4:6,8:10)]),
                     list(ID,Flag_Cliente)])

##I.9 CATEGORIZACION DE LAS VARIABLES:

##VARIABLE: EDAD

df_model <- within(df_model,{
  Edad_cat=NA
  Edad_cat[Edad<=27]=0
  Edad_cat[Edad %in% 28:32]=1
  Edad_cat[Edad %in% 33:37]=2
  Edad_cat[Edad %in% 38:44]=3
  Edad_cat[Edad %in% 45:50]=4
  Edad_cat[Edad >50]=5
  })

df_model$Edad_cat <- factor(df_model$Edad_cat)

##VARIABLE: MOB

df_model <- within(df_model,{
  MOB_cat=NA
  MOB_cat[MOB<=36]=0
  MOB_cat[MOB>36 & MOB<=48]=1
  MOB_cat[MOB>48 & MOB<=108]=2
  MOB_cat[MOB>108]=3
})

df_model$MOB_cat <- factor(df_model$MOB_cat)

#library(partykit)
#n=nrow(df_model)
#tree1 <- partykit::ctree(Flag_Cliente ~ MOB, df_model,
#                         control=ctree_control(mincriterion=.95, minsplit=.10*n,
#                                               minbucket=.05*n,maxdepth=3)
#                         )
#plot(tree1)
##VARIABLE: ClasRCC_u12m

df_model <- within(df_model,{
  ClasRCC_u12m_cat=NA
  ClasRCC_u12m_cat[ClasRCC_u12m==0]=1
  ClasRCC_u12m_cat[ClasRCC_u12m>0]=0
})

df_model$ClasRCC_u12m_cat <- factor(df_model$ClasRCC_u12m_cat)

##VARIABLE: NroEntRCC_TC

df_model <- within(df_model,{
  NroEntRCC_TC_cat=NA
  NroEntRCC_TC_cat[NroEntRCC_TC==1]=3
  NroEntRCC_TC_cat[NroEntRCC_TC==2]=2
  NroEntRCC_TC_cat[NroEntRCC_TC==3]=1
  NroEntRCC_TC_cat[NroEntRCC_TC>3]=0
})

df_model$NroEntRCC_TC_cat <- factor(df_model$NroEntRCC_TC_cat)

##VARIABLE: UtilizacionTC

df_model <- within(df_model,{
  UtilizacionTC_cat=NA
  UtilizacionTC_cat[UtilizacionTC<=30]=4
  UtilizacionTC_cat[UtilizacionTC>30 & UtilizacionTC<=50]=3
  UtilizacionTC_cat[UtilizacionTC>50 & UtilizacionTC<=80]=2
  UtilizacionTC_cat[UtilizacionTC>80 & UtilizacionTC<=100]=1
  UtilizacionTC_cat[UtilizacionTC>100]=0
})

df_model$UtilizacionTC_cat <- factor(df_model$UtilizacionTC_cat)

##VARIABLE: Mora promedio ponderado

df_model <- within(df_model,{
  Moraprompond_cat=NA
  Moraprompond_cat[Moraprompond==0]=2
  Moraprompond_cat[Moraprompond>0 & Moraprompond<=13]=1
  Moraprompond_cat[Moraprompond>13]=0
})

df_model$Moraprompond_cat <- factor(df_model$Moraprompond_cat)

##VARIABLE: Vecesmora0

df_model <- within(df_model,{
  Vecesmora0_cat=NA
  Vecesmora0_cat[Vecesmora0==0]=2
  Vecesmora0_cat[Vecesmora0==1]=2
  Vecesmora0_cat[Vecesmora0 %in% 2:3]=1
  Vecesmora0_cat[Vecesmora0>3]=0
})

df_model$Vecesmora0_cat <- factor(df_model$Vecesmora0_cat)

##VARIABLE: Vecesmora30

df_model <- within(df_model,{
  Vecesmora30_cat=NA
  Vecesmora30_cat[Vecesmora30==0]=2
  Vecesmora30_cat[Vecesmora30==1]=1
  Vecesmora30_cat[Vecesmora30>1]=0
  })

df_model$Vecesmora30_cat <- factor(df_model$Vecesmora30_cat)
#str(df_model)


##I.10 REGRESION LOGIT:

glm_1 <- glm(Flag_Cliente ~ Edad_cat+MOB_cat+ClasRCC_u12m_cat+NroEntRCC_TC_cat+UtilizacionTC_cat+
             Moraprompond_cat+Vecesmora0_cat+Vecesmora30_cat, data=df_model, family = "binomial"
             )

#summary(glm_1)

df_model$Prob_Recov <- predict(glm_1,df_model,type="response")

df_coef<-data.frame(coef=names(glm_1$coefficients), value=as.numeric(glm_1$coefficients))


##I.11 ALINEAMIENTO: LOGIT a SCORE (VER archivo excel: Alineamiento.xlsx)

df_model$Score=round(43.28085123*log(df_model$Prob_Recov/(1-df_model$Prob_Recov)) + 330.6843143,0)



