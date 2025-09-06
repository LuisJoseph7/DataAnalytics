library(plyr)
library(pROC)
#install.packages("pROC")
### II.TESTING

##II.1 ABRIENDO LA DATA TESTING:

path<-"F:/Quantiplus/Scoring/Sesion 3"
setwd(path)

df_testing <- read.table(file="df_testing_collection1607.txt", header=T, sep=",")

##II.2 FORMATO DE LA VARIABLE TARGET:

df_testing$Flag_Cliente <- factor(df_testing$Flag_Cliente,levels=c(0,1),
                                   labels=c("NoRecovery","Recovery"))

##II.3 REPLICANDO LAS VARIABLES DE LA DATA TRAINING:

df_testing$UtilizacionTC = 100*(df_testing$DeudaTC)/(df_testing$LineaTC)
df_testing$Moraprompond<-data.matrix(df_testing[,7:12])%*%matrix(1:6,6,1)*(1/21)
df_testing$Vecesmora0= apply(df_testing[,7:12],1,FUN=function(x) length(which(x>0)))
df_testing$Vecesmora30= apply(df_testing[,8:13],1,FUN=function(x) length(which(x>30)))

##VARIABLE: EDAD

df_testing <- within(df_testing,{
  Edad_cat=NA
  Edad_cat[Edad<=27]=0
  Edad_cat[Edad %in% 28:32]=1
  Edad_cat[Edad %in% 33:37]=2
  Edad_cat[Edad %in% 38:44]=3
  Edad_cat[Edad %in% 45:50]=4
  Edad_cat[Edad >50]=5
})

df_testing$Edad_cat <- factor(df_testing$Edad_cat)

df_testing <- within(df_testing,{
  MOB_cat=NA
  MOB_cat[MOB<=36]=0
  MOB_cat[MOB>36 & MOB<=48]=1
  MOB_cat[MOB>48 & MOB<=108]=2
  MOB_cat[MOB>108]=3
})

df_testing$MOB_cat <- factor(df_testing$MOB_cat)

##VARIABLE: ClasRCC_u12m

df_testing <- within(df_testing,{
  ClasRCC_u12m_cat=NA
  ClasRCC_u12m_cat[ClasRCC_u12m==0]=1
  ClasRCC_u12m_cat[ClasRCC_u12m>0]=0
})

df_testing$ClasRCC_u12m_cat <- factor(df_testing$ClasRCC_u12m_cat)

##VARIABLE: NroEntRCC_TC

df_testing <- within(df_testing,{
  NroEntRCC_TC_cat=NA
  NroEntRCC_TC_cat[NroEntRCC_TC==1]=3
  NroEntRCC_TC_cat[NroEntRCC_TC==2]=2
  NroEntRCC_TC_cat[NroEntRCC_TC==3]=1
  NroEntRCC_TC_cat[NroEntRCC_TC>3]=0
})

df_testing$NroEntRCC_TC_cat <- factor(df_testing$NroEntRCC_TC_cat)

##VARIABLE: UtilizacionTC

df_testing <- within(df_testing,{
  UtilizacionTC_cat=NA
  UtilizacionTC_cat[UtilizacionTC<=30]=4
  UtilizacionTC_cat[UtilizacionTC>30 & UtilizacionTC<=50]=3
  UtilizacionTC_cat[UtilizacionTC>50 & UtilizacionTC<=80]=2
  UtilizacionTC_cat[UtilizacionTC>80 & UtilizacionTC<=100]=1
  UtilizacionTC_cat[UtilizacionTC>100]=0
})

df_testing$UtilizacionTC_cat <- factor(df_testing$UtilizacionTC_cat)

##VARIABLE: Mora promedio ponderado

df_testing <- within(df_testing,{
  Moraprompond_cat=NA
  Moraprompond_cat[Moraprompond==0]=2
  Moraprompond_cat[Moraprompond>0 & Moraprompond<=13]=1
  Moraprompond_cat[Moraprompond>13]=0
})

df_testing$Moraprompond_cat <- factor(df_testing$Moraprompond_cat)

##VARIABLE: Vecesmora0

df_testing <- within(df_testing,{
  Vecesmora0_cat=NA
  Vecesmora0_cat[Vecesmora0==0]=2
  Vecesmora0_cat[Vecesmora0==1]=2
  Vecesmora0_cat[Vecesmora0 %in% 2:3]=1
  Vecesmora0_cat[Vecesmora0>3]=0
})

df_testing$Vecesmora0_cat <- factor(df_testing$Vecesmora0_cat)

##VARIABLE: Vecesmora30

df_testing <- within(df_testing,{
  Vecesmora30_cat=NA
  Vecesmora30_cat[Vecesmora30==0]=2
  Vecesmora30_cat[Vecesmora30==1]=1
  Vecesmora30_cat[Vecesmora30>1]=0
})

df_testing$Vecesmora30_cat <- factor(df_testing$Vecesmora30_cat)

##II.4 PROBABILIDAD DE RECUPERACION 

##II.4.1 PRIMERA FORMA:

df_testing$Prob_Recov <- predict(glm_1,df_testing,type="response")

##II.4.2 SEGUNDA FORMA:

#CREANDO VARIALES DUMMIES
df_testing$Edad_cat1[df_testing$Edad_cat==1]=1
df_testing$Edad_cat1[is.na(df_testing$Edad_cat) | df_testing$Edad_cat!=1]=0
df_testing$Edad_cat2[df_testing$Edad_cat==2]=1
df_testing$Edad_cat2[is.na(df_testing$Edad_cat) | df_testing$Edad_cat!=2]=0
df_testing$Edad_cat3[df_testing$Edad_cat==3]=1
df_testing$Edad_cat3[is.na(df_testing$Edad_cat) | df_testing$Edad_cat!=3]=0
df_testing$Edad_cat4[df_testing$Edad_cat==4]=1
df_testing$Edad_cat4[is.na(df_testing$Edad_cat) | df_testing$Edad_cat!=4]=0
df_testing$Edad_cat5[df_testing$Edad_cat==5]=1
df_testing$Edad_cat5[is.na(df_testing$Edad_cat) | df_testing$Edad_cat!=5]=0
df_testing$MOB_cat1[df_testing$MOB_cat==1]=1
df_testing$MOB_cat1[is.na(df_testing$MOB_cat) | df_testing$MOB_cat!=1]=0
df_testing$MOB_cat2[df_testing$MOB_cat==2]=1
df_testing$MOB_cat2[is.na(df_testing$MOB_cat) | df_testing$MOB_cat!=2]=0
df_testing$MOB_cat3[df_testing$MOB_cat==3]=1
df_testing$MOB_cat3[is.na(df_testing$MOB_cat) | df_testing$MOB_cat!=3]=0
df_testing$ClasRCC_u12m_cat1[df_testing$ClasRCC_u12m_cat==1]=1
df_testing$ClasRCC_u12m_cat1[is.na(df_testing$ClasRCC_u12m_cat) | df_testing$ClasRCC_u12m_cat!=1]=0
df_testing$NroEntRCC_TC_cat1[df_testing$NroEntRCC_TC_cat==1]=1
df_testing$NroEntRCC_TC_cat1[is.na(df_testing$NroEntRCC_TC_cat) | df_testing$NroEntRCC_TC_cat!=1]=0
df_testing$NroEntRCC_TC_cat2[df_testing$NroEntRCC_TC_cat==2]=1
df_testing$NroEntRCC_TC_cat2[is.na(df_testing$NroEntRCC_TC_cat) | df_testing$NroEntRCC_TC_cat!=2]=0
df_testing$NroEntRCC_TC_cat3[df_testing$NroEntRCC_TC_cat==3]=1
df_testing$NroEntRCC_TC_cat3[is.na(df_testing$NroEntRCC_TC_cat) | df_testing$NroEntRCC_TC_cat!=3]=0
df_testing$UtilizacionTC_cat1[df_testing$UtilizacionTC_cat==1]=1
df_testing$UtilizacionTC_cat1[is.na(df_testing$UtilizacionTC_cat) | df_testing$UtilizacionTC_cat!=1]=0
df_testing$UtilizacionTC_cat2[df_testing$UtilizacionTC_cat==2]=1
df_testing$UtilizacionTC_cat2[is.na(df_testing$UtilizacionTC_cat) | df_testing$UtilizacionTC_cat!=2]=0
df_testing$UtilizacionTC_cat3[df_testing$UtilizacionTC_cat==3]=1
df_testing$UtilizacionTC_cat3[is.na(df_testing$UtilizacionTC_cat) | df_testing$UtilizacionTC_cat!=3]=0
df_testing$UtilizacionTC_cat4[df_testing$UtilizacionTC_cat==4]=1
df_testing$UtilizacionTC_cat4[is.na(df_testing$UtilizacionTC_cat) | df_testing$UtilizacionTC_cat!=4]=0
df_testing$Moraprompond_cat1[df_testing$Moraprompond_cat==1]=1
df_testing$Moraprompond_cat1[is.na(df_testing$Moraprompond_cat) | df_testing$Moraprompond_cat!=1]=0
df_testing$Moraprompond_cat2[df_testing$Moraprompond_cat==2]=1
df_testing$Moraprompond_cat2[is.na(df_testing$Moraprompond_cat) | df_testing$Moraprompond_cat!=2]=0
df_testing$Vecesmora0_cat1[df_testing$Vecesmora0_cat==1]=1
df_testing$Vecesmora0_cat1[is.na(df_testing$Vecesmora0_cat) | df_testing$Vecesmora0_cat!=1]=0
df_testing$Vecesmora0_cat2[df_testing$Vecesmora0_cat==2]=1
df_testing$Vecesmora0_cat2[is.na(df_testing$Vecesmora0_cat) | df_testing$Vecesmora0_cat!=2]=0
df_testing$Vecesmora30_cat1[df_testing$Vecesmora30_cat==1]=1
df_testing$Vecesmora30_cat1[is.na(df_testing$Vecesmora30_cat) | df_testing$Vecesmora30_cat!=1]=0
df_testing$Vecesmora30_cat2[df_testing$Vecesmora30_cat==2]=1
df_testing$Vecesmora30_cat2[is.na(df_testing$Vecesmora30_cat) | df_testing$Vecesmora30_cat!=2]=0

df_testing<- within(df_testing,{
Prob_Recov2=NA
Prob_Recov2= 1/(1+exp(-1*(-4.507458824+
Edad_cat1*0.145250169+Edad_cat2*0.345752803+Edad_cat3*0.416418461+
Edad_cat4*0.409261323+Edad_cat5*0.590341556+MOB_cat1*0.555429683+
MOB_cat2*0.831092383+MOB_cat3*1.03214038+ClasRCC_u12m_cat1*0.900915182+
NroEntRCC_TC_cat1*0.179131996+NroEntRCC_TC_cat2*0.332551343+NroEntRCC_TC_cat3*0.935828472+
UtilizacionTC_cat1*0.249212297+UtilizacionTC_cat2*0.729153404+UtilizacionTC_cat3*0.907046937+
UtilizacionTC_cat4*1.156702137+Moraprompond_cat1*0.100675031+Moraprompond_cat2*1.22450005+
Vecesmora0_cat1*0.09375791+Vecesmora0_cat2*0.379416737+Vecesmora30_cat1*0.703647709+
Vecesmora30_cat2*1.685403974)))
})

###II.5 REPLICANDO EL SCORE

df_testing$Score=round(43.28085123*log(df_testing$Prob_Recov/(1-df_testing$Prob_Recov)) + 330.6843143,0)

##II.6 METRICAS DE VALIDACION

#II.6.1 INPUT: CALCULO DE KS

v1<-quantile(df_testing$Score, probs = seq(0,1,.05), na.rm = T, names=F)[2:21]

df_testing$Score_cat=NA
for (i in 1:length(v1)){
  df_testing=within(df_testing,{
  Score_cat[is.na(Score_cat) & Score<=v1[i]]=i
  })
}

tt<-ddply(df_testing,~Score_cat,summarize,min=min(Score),max=max(Score))
  
tt$label <- paste0(tt$min," - ",tt$max)

df_testing$Score_cat <- factor(df_testing$Score_cat, levels=1:20,
                               labels=tt$label)

df_perf1 <-  dcast(df_testing,Score_cat~Flag_Cliente,
             fun.agg=function(x) sum(!is.na(x)), value.var = "ID")

write.table(df_perf1, paste0(path,"/df_perf1.txt"),sep=",",quote=F,
            row.names = F, col.names = T)

##II.6.2 INPUT: CURVA COR & GINI

v2<-quantile(df_testing$Prob_Recov, probs = seq(0,1,.05), na.rm = T, names=F)[2:21]

q<-quantile(0:1,probs = seq(0,1,.05),names = F)
df_testing$Prob_Recov_cat <- cut(df_testing$Prob_Recov,breaks=q)

df_perf2 <-  dcast(df_testing,Prob_Recov_cat~Flag_Cliente,
                   fun.agg=function(x) sum(!is.na(x)), value.var = "ID")

write.table(df_perf2, paste0(path,"/df_perf2.txt"),sep=";",quote=F,
            row.names = F, col.names = T)

##UTILIZANDO LA LIBRERIA pROC
#install.packages("pROC")
#library(pROC)

auc1 <- auc(df_testing$Flag_Cliente,df_testing$Prob_Recov2)

plot.roc(df_testing$Flag_Cliente,df_testing$Prob_Recov2)
ttt<- roc(df_testing$Flag_Cliente,df_testing$Prob_Recov2)

df_coordenadas <- data.frame(prob=ttt$thresholds,sen=ttt$sensitivities,esp=ttt$specificities)

##II.6.3 INPUT: ESTABILIDAD

df_model$Score_cat<- cut(df_model$Score,breaks=seq(100,500,50))
df_testing$Score_cat2<- cut(df_testing$Score,breaks=seq(100,500,50))
#colnames(df_testing)

df_stab <- data.frame(Cat=names(table(df_model$Score_cat)),
           Benchmark=as.numeric(table(df_model$Score_cat)),
           Actual=as.numeric(table(df_testing$Score_cat2)))

write.table(df_stab, paste0(path,"/df_stab.txt"),sep=";",quote=F,
            row.names = F, col.names = T)

##II.6.4 INPUT: CALIBRACION

#rm(df_calib)
df_calib<-ddply(df_testing,~Score_cat2,summarize,PD=median(1-Prob_Recov))

df_calib$NoRecov<-as.numeric(table(df_testing$Score_cat2,df_testing$Flag_Cliente)[,1])
df_calib$Recov<-as.numeric(table(df_testing$Score_cat2,df_testing$Flag_Cliente)[,2])

write.table(df_calib, paste0(path,"/df_calib.txt"),sep=";",quote=F,
            row.names = F, col.names = T)

#II.6.4.1 Estimamos el factor Aditivo y multiplicativo de la calibración:

#II.6.4.1.A: Por minimo cuadrado ordinarios:

y<-c(-2.251291799,-2.602689685,-1.868299546,
     -1.122142786,-0.239582278,0.867670045,
     2.068268241,4.365219516
)#Copiado del excel: O5:O12

x<-c(-4.258246527,-3.432552617,-2.378409612,
     -1.308731887,-0.139095594,0.964358394,
     2.121060531,3.022818572
)#Copiado del excel: M5:M12

#Correr para ver el intercepto y a pendiente:
lm(y~x)
#(Intercept)            x  
#0.4961       0.8785  

#II.6.4.1.B: Por minimo cuadrado ponderados:

y<-c(-2.251291799,-2.602689685,-1.868299546,
     -1.122142786,-0.239582278,0.867670045,
     2.068268241,4.365219516
)#Copiado del excel: O5:O12

x<-c(-4.258246527,-3.432552617,-2.378409612,
     -1.308731887,-0.139095594,0.964358394,
     2.121060531,3.022818572
)#Copiado del excel: M5:M12

w<-c(1.80952381,11.17241379, 38.11550152,
     73.92982456,101.2895377,83.10275689,
     39.95012469,2.962343096
)#Copiado del excel: Q5:Q12 (Varianzas que en la estimación serán los pesos o ponderaciones)

#Correr para ver el intercepto y a pendiente:
lm(y~x, weights = sqrt(w))
#(Intercept)            x  
#0.1721       0.8671 
