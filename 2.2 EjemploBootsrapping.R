###BOOTSTRAPPING

#1. Base de Ingresos

df_temp <- data.frame(ID=1:500, 
                      Ingreso=round(runif(500,min=1000,max=10000))) 

hist(df_temp$Ingreso)

###BOOTSTRAPPING
###BUSCANCO UN MARGEN DE ERROR (QUE TAN VARIABLE ES EL INGRESO)

k=350
B=1000

t <- numeric(B)

for (i in 1:B) {
  t[i] <- mean(df_temp[sample(1:nrow(df_temp),k,replace = T),]$Ingreso)
}

hist(t)

#Limite inferior del ingreso:
quantile(t,0.025)
#Limite superior del ingreso:
quantile(t,0.975)

#Indicar entre que limites de variación esta el Ingreso Promedio:

