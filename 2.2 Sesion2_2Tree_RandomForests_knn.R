
#DATA: df_churn
#REALIZAR UN MODELO DE PROPENSION DE FUGA

###I. ABRIENDO EL ARCHIVO DE TRABAJO: df_churn.txt

path <- "F:/Quantiplus/Scoring/Sesion 2"
setwd(path)
df_file02 <- read.table(file="df_churn.txt",sep=",", header=T)

#str(df_file02)

df_file02$Flag_Churn<-factor(df_file02$Flag_Churn,
                                levels=c(0,1),
                                labels=c("Fuga","No Fuga"))

table(df_file02$Flag_Churn)
###II. AGREGANDO VARIABLES NUEVAS:

#II.1 Utilizacion TC:

df_file02$UtilizacionTC <- 100*df_file02$SaldoTotal/df_file02$LineadeCredito

#II.2 Recencia vs Frecuencia:

df_file02$RyF <- df_file02$Recency_Global/df_file02$Frecuency_Global

##III. TREE (ALGORITMO CTREE)
#library(partykit)

n=nrow(df_file02)
tree1 <- partykit::ctree( Flag_Churn ~ UtilizacionTC+RyF+
                            BehaviorScoring+MonetaryValue_Global,
                     data = df_file02, 
                     control=ctree_control(mincriterion = .95, minsplit = .1*n,
                                           minbucket = .05*n, maxdepth = 5)
                     )
print(tree1)
plot(tree1)

df_file02$nodo_tree1 <- predict(tree1, df_file02, type="node")
df_file02$prob_tree1 <- predict(tree1, df_file02, type="prob")
df_file02$churn_pred_tree1 <- predict(tree1, df_file02, type="response")

table(df_file02$Flag_Churn, df_file02$churn_pred_tree1)
#Error de clasificacion: (156+604)/(1034+604+156+1916)

##IV. RANDOM FOREST
install.packages("randomForest", repos="https://cran.rstudio.com")
library(randomForest) 

rf1 <- randomForest( Flag_Churn ~ UtilizacionTC+RyF+BehaviorScoring+
                       MonetaryValue_Global,
                     data = df_file02, ntree=500, mtry = 2, importance = TRUE,
                         do.trace = 10)
#print(rf1)

df_file02$prob_rf1 <- predict(rf1, df_file02, type="prob")
df_file02$churn_pred_rf1 <- predict(rf1, df_file02, type="response")

#Matriz de confusion:
table(df_file02$Flag_Churn,df_file02$churn_pred_rf1)

importance(rf1)
varImpPlot(rf1)

##V. K NEAREST NEIGHBOR (KNN)
#install.packages("class")
library(class)

#k=sqrt(nrow(df_file02))
knn1 <- knn(df_file02[,c(3,7,9,10)] , df_file02[,c(3,7,9,10)],
                     cl=df_file02$Flag_Churn, k=7)

df_file02$churn_pred_knn1 <- knn1

#Matriz de confusion:
table(df_file02$Flag_Churn,df_file02$churn_pred_knn1)
#Error de clasificacion: (290+605)/(1033+290+605+1782)

#Exactitud: RF > TREE > KNN



