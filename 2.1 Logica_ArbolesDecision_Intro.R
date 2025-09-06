#install.packages("rpart")
library(rpart) #CART
#install.packages("rpart.plot")
library(rpart.plot)
#install.packages("partykit")
library(partykit)
#install.packages("CHAID",repos="http://R-Forge.R-project.org")
library(CHAID)

###DATA: df_ActivaTC.txt
#Muestra de clientes a quienes se vendió una tarjeta de credito
#Y donde muchos de ellos no la activaron.

#I. ABRIMOS EL ARCHIVO DE TRABAJO: df_ActivaTC.txt
path <- "F:/Quantiplus/Scoring/Sesion 2/Scoring_Sesion2"
setwd(path)
df_file01 <- read.table("df_Antiguedad_Riesgo.txt", sep=";",header = T, stringsAsFactors = F)

#Categorizando la variable X:
df_file01$Antiguedad_meses <- as.factor(df_file01$Antiguedad_meses)

#Categorizando la variable de respuesta:
df_file01$Default<-factor(df_file01$Default,
                                levels=c(0,1),
                                labels=c("good","bad"))

#UTILIZANDO LA FUNCION RPART: (PARTICIONES RECURSIVAS)

n=nrow(df_file01)
tree1 <- rpart(Default ~ Antiguedad_meses, data=df_file01,
               method = "class", 
               control=rpart.control(minsplit = .1*n, minbucket =.05*n, 
                                     maxdepth = 2)
)       

summary(tree1)
rpart.plot(tree1)
plot(as.party(tree1)) 

#UTILIZANDO LA FUNCION CTREE: ARBOLES POR INFERENCIA CONDICIONAL 

n=nrow(df_file01)
tree2<- partykit::ctree(Default ~ Antiguedad_meses, data=df_file01,
                        control=ctree_control(mincriterion = .95, minsplit = .1*n,
                                              minbucket = .05*n, maxdepth = 2))
print(tree2)
plot(tree2)
str(df_file01)

#CHAID

tree3 <- chaid(Default ~ Antiguedad_meses, data=df_file01,
               control = chaid_control(alpha4 = .05, minsplit = .1*n,
                                       minbucket = .05*n))

print(tree3)
plot(tree3)



