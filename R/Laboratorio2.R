library(tidyverse)
library(dplyr)

DAQ1<-datos50CM

DAQ2<-datos20CM
colnames(DAQ2)

hist(DAQ2$ultrasonido,breaks=10) ## distribucion normal variable aleatoria
hist(DAQ2$laser,breaks=10) ##distribucion normal variable aleatoria

colnames(DAQ1)
hist(DAQ1$ultrasonido,breaks=25) ## distribucion normal variable aleatoria
hist(DAQ1$laser,breaks=25)
plot(DAQ1[2:3])

##prediccion ultrasonido minimos cuadrados
X<-DAQ1$ultrasonido
Y<-DAQ1$real
B<-cov(X,Y)/var(X)
A<-mean(Y)-B*mean(X)
##------------------------------------------------------
##prediccion laser minimos cuadrados
x<-DAQ1$laser
y<-DAQ1$real
b<-cov(x,y)/var(x)
a<-mean(y)-b*mean(x)

DAQ1<-mutate(DAQ1,regresion_ultrasonido=A+ultrasonido*B)
DAQ1<-mutate(DAQ1,regresion_laser=a+laser*b)


model<-lm(Y~X+x)
DAQ1<-mutate(DAQ1,regresion_mutiple=predict(model))
names(NEWDAQ)
library(reshape)
prediccion<-NEWDAQ
NEWDAQ=rename(NEWDAQ,c(ultrasonido="X"))
NEWDAQ=rename(NEWDAQ,c(laser="x"))
names(NEWDAQ)

prediccion<-mutate(prediccion,predict=predict(model,NEWDAQ))
NEWDAQ=rename(NEWDAQ,c(X="ultrasonido"))
NEWDAQ=rename(NEWDAQ,c(x="laser"))
##summary(DAQ)##ANALISIS PROBATORIO DE DATOS


##KNN_______________________________________________
normalise <-function(x){##
  return((x-min(x))/(max(x)-min(x)))}

DAQ5=DAQKNN2
DAQ5<-mutate(DAQ5,regresion_ultrasonido=A+ultrasonido*B)
DAQ5<-mutate(DAQ5,regresion_laser=a+laser*b)

##convertir muro a factor si no es char
plot(DAQ5[2:3])
plot(DAQ5[5:6])
hist(DAQ5$ultrasonido,breaks=15)
hist(DAQ5$laser,breaks=50)
library(psych)
pairs.panels(DAQ5[5:6], pch=21, main=("Concavo =rojo, convexo=verde, plano=azul")
             , bg=c("red","green2","blue")[unclass(DAQ5$muro)])##si se distingen los grupos es util para machine

prop.table((table(DAQ5$ultrasonido))) ##
prop.table((table(DAQ5$laser))) ##virificar si el dataset es util si el numero es pequeño esta balanceado
##---------------------------------
##ingenieria de caracteristicas


normData<-DAQ5
standarData<-DAQ5
normData$ultrasonido<-normalise(normData$ultrasonido)
normData$laser<-normalise(normData$laser)
##normalizacion z score probar opciones
standarData$ultrasonido<-scale(normData$ultrasonido)
standarData$laser<-scale(normData$laser)

colnames(standarData)[5:6]<-c("ultrasonido","laser")
sample.index<-sample(1:nrow(DAQ5)
                     , nrow(DAQ5)*0.7
                     , replace=FALSE)
##------------------------------------------
## entrenamiento
k<-1
predictors<-c("regresion_ultrasonido","regresion_laser")
train.data<-DAQ5[sample.index##70%
                ,c(predictors,"muro")
                , drop=F]
test.data<-DAQ5[-sample.index##30%
                ,c(predictors,"muro")
                , drop=F]
library(class)
predictions<- knn(train=train.data[predictors]## entrenamiento variables predictoras
                  ,test=test.data[predictors]## prueba variables predictoras
                  ,cl=train.data$muro ##variable clase
                  ,k=k)

## verificar el rendimiento
library(gmodels)
CrossTable(x=test.data$muro,y=predictions)

