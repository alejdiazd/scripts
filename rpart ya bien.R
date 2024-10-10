
### Not Run

### rpart: Ejemplo datos abiertos covid19 Mexico

### Árbol de clasificación sobre hospitalizados vs ambulatorios 
### ¿Qué factores influyen para que alguien sea hospitalizado?


### Datos obtenidos de esta que es la dirección oficial:
### https://www.gob.mx/salud/documentos/datos-abiertos-152127
### Nota: bajar datos de forma manual por si cambian campos
### Nota: cambiar la fecha en el nombre del archivo 

library(haven) ## para stata, spss 

base <- read.csv("200823COVID19MEXICO.csv")

###base1 <- read.csv("200510COVID19MEXICO.csv")


dim(base1)
names(base1)

### si se requieren bases anteriores, aquí están del 12 abr en adelante
### https://www.gob.mx/salud/documentos/datos-abiertos-bases-historicas-direccion-general-de-epidemiologia


### Ahora vamos a recodificar según Catálogo y Descriptores
### Nota: revisar Catálogo y Descriptores por si cambian campos

install.packages("car")

### a ver, aquí hay difuntitos

table(base1$FECHA_DEF)
table(base1$Fech_df)


library(car)



base1$Fech_df<- (base1$FECHA_DEF)
base1$Fech_df<-recode(base1$Fech_df,"9999-99-99=1;else=0")


table(base$FECHA_DEF)



difuntos <- subset(base1, FECHA_DEF!="9999-99-99")

dim(difuntos)

table(difuntos$FECHA_DEF)

write.csv(difuntos, file="difuntos.csv")

table(base$TIPO_PACIENTE)
table(base$RESULTADO)

table(difuntos$RESULTADO, difuntos$NEUMONIA)

mytable<-table(difuntos$NEUMONIA, difuntos$RESULTADO )
margin.table(mytable, 1) # A frequencies (summed over B)
margin.table(mytable, 2) # B frequencies (summed over A)
prop.table(mytable) # cell percentages
prop.table(mytable, 1) # row percentages
prop.table(mytable, 2) # column percentages
ftable(mytable) # print table
summary(mytable) # chi-square test of indepedence


dif_pos <- subset(difuntos,difuntos$RESULTADO==1)
dim(dif_pos)

########## base con puro positivo

mytable<-ftable(base1$RESULTADO, base1$TIPO_PACIENTE,base1$difuntos) 

ftable(base1$RESULTADO, base1$Hospitalizado,base1$difuntos)


base$difuntos<- (base$FECHA_DEF)
base$difuntos<-recode(base$difuntos,"'9999-99-99'=0;else=1")

summary(base)
base1$Hospitalizado<-(base1$TIPO_PACIENTE)
base1$Hospitalizado<-recode(base1$Hospitalizado,"1=0;2=1")



base <- subset(base, RESULTADO==1)
dim(base)





#### recode


base$Mujer<-(base$SEXO)
base$Mujer<-recode(base$Mujer,"1=1;2=0")

base$Hospitalizado<-(base$TIPO_PACIENTE)
base$Hospitalizado<-recode(base$Hospitalizado,"1=0;2=1")
 
base$uci<-(base$UCI)
base$uci<-recode(base$uci,"1=1;2=0;97=0;99=0")
 
base$Embarazo<-(base$EMBARAZO)
base$Embarazo<-recode(base$Embarazo,"1=1;2=0;97=0;98=0")
 
base$Diabetes<-(base$DIABETES)
base$Diabetes<-recode(base$Diabetes,"1=1;2=0;98=0")

base$Epoc<-(base$EPOC)
base$Epoc<-recode(base$Epoc,"1=1;2=0;98=0")

base$Asma<-(base$ASMA)
base$Asma<-recode(base$Asma,"1=1;2=0;98=0")

base$Hipertension<-(base$HIPERTENSION)
base$Hipertension<-recode(base$Hipertension,"1=1;2=0;98=0")

base$Cardiovascular<-(base$CARDIOVASCULAR)
base$Cardiovascular<-recode(base$Cardiovascular,"1=1;2=0;98=0")

base$Obesidad<-(base$OBESIDAD)
base$Obesidad<-recode(base$Obesidad,"1=1;2=0;98=0")

base$Tabaquismo<-(base$TABAQUISMO)
base$Tabaquismo<-recode(base$Tabaquismo,"1=1;2=0;98=0")

base$Renal<-(base$RENAL_CRONICA)
base$Renal<-recode(base$Renal,"1=1;2=0;98=0")

base$Centinela<-(base$ORIGEN)
base$Centinela<-recode(base$Centinela,"1=1;2=0")

base$Neumonia<-(base$NEUMONIA)
base$Neumonia<-recode(base$Neumonia,"1=1;2=0;99=0")

base$Intubado<-(base$INTUBADO)
base$Intubado<-recode(base$Intubado,"1=1;2=0;97=0;99=0")

base$Resultado_Pos<-(base$RESULTADO)
base$Resultado_Pos<-recode(base$Resultado_Pos,"1=1;2=0;3=0")

base$Resultado_Neg<-(base$RESULTADO)
base$Resultado_Neg<-recode(base$Resultado_Neg,"1=0;2=1;3=0")

base$Resultado_Pend<-(base$RESULTADO)
base$Resultado_Pend<-recode(base$Resultado_Pend,"1=0;2=0;3=1")

base$Inmusupr<-(base$INMUSUPR)
base$Inmusupr<-recode(base$Inmusupr, "1=1;2=0;98=0")


base$CruzRoja<-(base$SECTOR)
base$CruzRoja<-recode(base$CruzRoja,"1=1;2=0;3=0;4=0;5=0;6=0;7=0;8=0;9=0;10=0;11=0;12=0;13=0;99=0")

base$DIF<-(base$SECTOR)
base$DIF<-recode(base$DIF, "2=1;1=0;else=0")

base$Estatal<-(base$SECTOR)
base$Estatal<-recode(base$Estatal, "3=1;1=0;2=0;else=0")

base$IMSS<-(base$SECTOR)
base$IMSS<-recode(base$IMSS, "4=1;else=0")

base$IMSS_Bienestar<-(base$SECTOR)
base$IMSS_Bienestar<-recode(base$IMSS_Bienestar, "5=1;else=0")

base$ISSSTE<-(base$SECTOR)
base$ISSSTE<-recode(base$ISSSTE, "6=1;else=0")

base$Municipal<-(base$SECTOR)
base$Municipal<-recode(base$Municipal, "7=1;else=0")

base$Pemex<-(base$SECTOR)
base$Pemex<-recode(base$Pemex, "8=1;else=0")

base$Privado<-(base$SECTOR)
base$Privado<-recode(base$Privado, "9=1;else=0")

base$Sedena<-(base$SECTOR)
base$Sedena<-recode(base$Sedena, "10=1;else=0")

base$Semar<-(base$SECTOR)
base$Semar<-recode(base$Semar, "11=1;else=0")

base$Salud<-(base$SECTOR)
base$Salud<-recode(base$Salud, "12=1;else=0")

base$Universitario<-(base$SECTOR)
base$Universitario<-recode(base$Universitario, "13=1;else=0")

base$Hosp_No_Especificado<-(base$SECTOR)
base$Hosp_No_Especificado<-recode(base$Hosp_No_Especificado, "99=1;else=0")


base$AGS<-(base$ENTIDAD_UM)
base$AGS<-recode(base$AGS, "1=1;else=0")

base$BC<-(base$ENTIDAD_UM)
base$BC<-recode(base$BC,   "2=1;else=0")

base$BCS<-(base$ENTIDAD_UM)
base$BCS<-recode(base$BCS, "3=1;else=0")

base$CAM<-(base$ENTIDAD_UM)
base$CAM<-recode(base$CAM, "4=1;else=0")

base$COA<-(base$ENTIDAD_UM)
base$COA<-recode(base$COA, "5=1;else=0")

base$COL<-(base$ENTIDAD_UM)
base$COL<-recode(base$COL, "6=1;else=0")

base$CHIA<-(base$ENTIDAD_UM)
base$CHIA<-recode(base$CHIA,"7=1;else=0")

base$CHIH<-(base$ENTIDAD_UM)
base$CHIH<-recode(base$CHIH,"8=1;else=0")

base$CDMX<-(base$ENTIDAD_UM)
base$CDMX<-recode(base$CDMX,"9=1;else=0")

base$DUR<-(base$ENTIDAD_UM)
base$DUR<-recode(base$DUR, "10=1;else=0")

base$GUA<-(base$ENTIDAD_UM)
base$GUA<-recode(base$GUA, "11=1;else=0")

base$GUE<-(base$ENTIDAD_UM)
base$GUE<-recode(base$GUE, "12=1;else=0")

base$HID<-(base$ENTIDAD_UM)
base$HID<-recode(base$HID, "13=1;else=0")

base$JAL<-(base$ENTIDAD_UM)
base$JAL<-recode(base$JAL, "14=1;else=0")

base$MEX<-(base$ENTIDAD_UM)
base$MEX<-recode(base$MEX, "15=1;else=0")

base$MICH<-(base$ENTIDAD_UM)
base$MICH<-recode(base$MICH,"16=1;else=0")

base$MOR<-(base$ENTIDAD_UM)
base$MOR<-recode(base$MOR,"17=1;else=0")

base$NAY<-(base$ENTIDAD_UM)
base$NAY<-recode(base$NAY,"18=1;else=0")

base$NL<-(base$ENTIDAD_UM)
base$NL<-recode(base$NL,"19=1;else=0")

base$OAX<-(base$ENTIDAD_UM)
base$OAX<-recode(base$OAX,"20=1;else=0")

base$PUE<-(base$ENTIDAD_UM)
base$PUE<-recode(base$PUE,"21=1;else=0")

base$QUE<-(base$ENTIDAD_UM)
base$QUE<-recode(base$QUE,"22=1;else=0")

base$QROO<-(base$ENTIDAD_UM)
base$QROO<-recode(base$QROO,"23=1;else=0")

base$SLP<-(base$ENTIDAD_UM)
base$SLP<-recode(base$SLP,"24=1;else=0")

base$SIN<-(base$ENTIDAD_UM)
base$SIN<-recode(base$SIN,"25=1;else=0")

base$SON<-(base$ENTIDAD_UM)
base$SON<-recode(base$SON,"26=1;else=0")

base$TAB<-(base$ENTIDAD_UM)
base$TAB<-recode(base$TAB,"27=1;else=0")

base$TAM<-(base$ENTIDAD_UM)
base$TAM<-recode(base$TAM,"28=1;else=0")

base$TLA<-(base$ENTIDAD_UM)
base$TLA<-recode(base$TLA,"29=1;else=0")

base$VER<-(base$ENTIDAD_UM)
base$VER<-recode(base$VER,"30=1;else=0")

base$YUC<-(base$ENTIDAD_UM)
base$YUC<-recode(base$YUC,"31=1;else=0")

base$ZAC<-(base$ENTIDAD_UM)
base$ZAC<-recode(base$ZAC,"32=1;else=0")



base$difuntos<- (base$FECHA_DEF)
base$difuntos<-recode(base$difuntos,"'9999-99-99'=0;else=1")

table(base$difuntos)

names(base)

summary(base)



### solo las variables a emplear para descriptivos

names(base)


attach(base)
base3 <- data.frame(difuntos,EDAD,Mujer,Hospitalizado,Embarazo,
 Diabetes,Epoc,Asma,Hipertension,Cardiovascular,Obesidad,
 Tabaquismo,Renal,Neumonia,Inmusupr,uci,Intubado,
 CruzRoja,DIF,Estatal,IMSS,ISSSTE,Privado,Salud,Pemex,Semar,
   BC,BCS,COA,CHIA,CHIH,CDMX,
  GUA,GUE,HID,JAL,MEX,MICH,MOR,NL,
  PUE,QROO,SIN,SON,TAB,TAM,TLA,VER,
  YUC, Centinela)
detach(base)




### casi todas

attach(base)
base3 <- data.frame(difuntos,Centinela,Resultado_Pos,Resultado_Neg,
 Resultado_Pend,EDAD,Mujer,Embarazo,Hospitalizado,
 Diabetes,Epoc,Asma,Hipertension,Cardiovascular,Obesidad,
 Tabaquismo,Renal,Neumonia,Inmusupr,uci,Intubado,
 CruzRoja,DIF,Estatal,IMSS,IMSS_Bienestar,ISSSTE,Municipal,
 Pemex,Privado,Sedena,Semar,Salud,Universitario,Hosp_No_Especificado,
  AGS,BC,BCS,CAM,COA,COL,CHIA,CHIH,CDMX,DUR,
  GUA,GUE,HID,JAL,MEX,MICH,MOR,NAY,NL,OAX,
  PUE,QUE,QROO,SLP,SIN,SON,TAB,TAM,TLA,VER,
  YUC,ZAC)
detach(base)


### descriptive difuntos


attach(difuntos)
base3 <- data.frame(Centinela,Resultado_Pos,Resultado_Neg,
 Resultado_Pend,EDAD,Mujer,Embarazo,Hospitalizado,
 Diabetes,Epoc,Asma,Hipertension,Cardiovascular,Obesidad,
 Tabaquismo,Renal,Neumonia,Inmusupr,uci,Intubado,
 CruzRoja,DIF,Estatal,IMSS,IMSS_Bienestar,ISSSTE,Municipal,
 Pemex,Privado,Sedena,Semar,Salud,Universitario,Hosp_No_Especificado,
  AGS,BC,BCS,CAM,COA,COL,CHIA,CHIH,CDMX,DUR,
  GUA,GUE,HID,JAL,MEX,MICH,MOR,NAY,NL,OAX,
  PUE,QUE,QROO,SLP,SIN,SON,TAB,TAM,TLA,VER,
  YUC,ZAC)
detach(difuntos)



names(base3)
summary(base3)
dim(base3)

### diviendo la base en entrenamiento y prueba

install.packages("caret")
install.packages("lattice")
install.packages("ggplot2")



library(caret)

set.seed(100)  # Para reproducir los mismos resultados

set.seed(1645) ### a ver otra seed 

IndicesEntrenamiento <- createDataPartition(y = base3$difuntos,
                                            p = 0.7,
                                            list = FALSE)
Entrenamiento <- base3[IndicesEntrenamiento,]

Test <- base3[-IndicesEntrenamiento,]



names(Entrenamiento)
summary(Entrenamiento)
dim(Entrenamiento)


names(Test)
summary(Test)
dim(Test)


### Preparando modelo rpart

install.packages("rpart")


library(rpart)
library(rpart.plot)
library(rattle)


### Modelo: siempre usar as.factor(dep_var)


dos<- rpart(formula= as.factor(difuntos)~.,
 data=Entrenamiento, method= "class")


, cp=0


dos<- rpart(formula= as.factor(difuntos)~  Mujer+ 
 EDAD+Diabetes+Epoc+
 Asma+ Hipertension+Cardiovascular+Obesidad+Tabaquismo+Renal+Neumonia+
 Inmusupr+Estatal+IMSS+ISSSTE+Pemex+Privado+
 Salud+
  BC+BCS+CAM+COA+COL+CHIA+CHIH+CDMX+DUR+
  GUA+GUE+HID+JAL+MEX+MICH+MOR+NAY+NL+OAX+
  PUE+QUE+QROO+SLP+SIN+SON+TAB+TAM+TLA+VER+
  YUC+ZAC, data=Entrenamiento, method= "class")



m1 <- glm(depvar ~ x1 + x2 +x3 +x4 +x5, data=Test, familiy=binomial(link='logit'))

tres<- rpart(formula= as.factor(difuntos)~ Centinela+
 Resultado_Pos+ Mujer+ Embarazo+EDAD+Diabetes+Epoc+
 Asma+ Hipertension+Cardiovascular+Obesidad+Tabaquismo+Renal+Neumonia+
 Inmusupr+Estatal+IMSS+ISSSTE+Municipal+Pemex+Privado+
 Sedena+Semar+Salud+Universitario+
  BC+BCS+CAM+COA+COL+CHIA+CHIH+CDMX+DUR+
  GUA+GUE+HID+JAL+MEX+MICH+MOR+NAY+NL+OAX+
  PUE+QUE+QROO+SLP+SIN+SON+TAB+TAM+TLA+VER+
  YUC+ZAC, data=Entrenamiento, method= "class")

### Movilidad: tipo de transporte
               bici
               transporte público
               vehículo propio
               taxi / app - uber-didi etc 
### Camas disponibles
### respiradores 
### Médicos por 10 mil hab
### Enfermeras por 10 mil hab
### Ingreso "corriente"
### Educación / escolaridad (años de)
### Estilo de vida:
    horas de ejercicio
    horas de sueño
    Dieta: grasas (BMI)
    Alcohol (un six? dos? un 24?)
   Hacinamiento: número de personas / número de cuartos / edades
   Cocina: leña
           carbón / etc
   Piso: tierra
         cemento
         acceso a agua
   Socioeconómico (niveles tipo amai)
  Marginación (Conapo 9 variables)
Sector esencial?
 trabaja de informal
 longevidad padre / madre (antes eran de buena madera / genética)
 trasfusiones (es algo malo?)
 tipo de sangre
  número de días desde los primeros síntomas

 



       
 



### resultados del decision tree

summary(dos)  ### resultado completo

print(dos)    ### raiz y ramas relevantes

rpart.rules(dos) ### resumen de probabilidades

fancyRpartPlot(dos, yesno=2, 
   caption=NULL, tweak=1.1, type=4, clip.right.labs = F)

rpart.plot(dos)  ### otra forma de ver el tree


### ahora la predicción del modelo


pred1 <- predict(dos, newdata = Test, type="class")

plotcp(dos)    ## es más para prediction -linear models-

rsq.rpart(dos) ## es más para prediction -linear models-


cm = table(pred1, Test$difuntos) ### preparando la confusion matrix 

library(caret)

confusionMatrix(cm)  ### resultados de la confusion matrix 

### Otros nombres de indicadores de la confusion matrix:
### Positive Predicted value = Precision 
### Sensitivity = True positive rate = Recall

### F1 score formula: 
### 2 * ((Precision * Recall) / (Precision + Recall))
### F1 score indica la relación entre precision & recall

 2 * ((0.9297  * 0.976) / (0.9297 + 0.976))

### End



attach(difuntos)
base4 <- data.frame(Centinela,Resultado_Pos,Resultado_Neg,
 Resultado_Pend,EDAD,Mujer)
detach(difuntos)


attach(difuntos)
base5<-data.frame(Embarazo,Hospitalizado,
 Diabetes,Epoc,Asma,Hipertension,Cardiovascular,Obesidad,
 Tabaquismo,Renal,Neumonia,Inmusupr,uci,Intubado)
detach(difuntos)


attach(difuntos)
base6<-data.frame(CruzRoja,DIF,Estatal,IMSS,IMSS_Bienestar,ISSSTE,Municipal,
 Pemex,Privado,Sedena,Semar,Salud,Universitario,Hosp_No_Especificado)
detach(difuntos)




  AGS,BC,BCS,CAM,COA,COL,CHIA,CHIH,CDMX,DUR,
  GUA,GUE,HID,JAL,MEX,MICH,MOR,NAY,NL,OAX,
  PUE,QUE,QROO,SLP,SIN,SON,TAB,TAM,TLA,VER,
  YUC,ZAC)




library(summarytools)
summarytools::descr(base3)

descr(base, stats = c("mean", "sd","min", "max"), transpose = TRUE, headings = FALSE)


dfSummary(base3, missing.col = F)

va1<-dfSummary(base3)
view(va1, file = "~/covid19/senado.html")

table(difuntos$Neumonia, difuntos$Resultado_Neg)


 pred1 <- predict(uno, newdata = Test, type="class")


write.csv(regla1, "regla1.csv")


################

library(randomForest)

difu.rf <- randomForest (formula= difuntos~.,
       data=Entrenamiento, ntree=500,  
     importance=T)


difu.rf  ### la verdad, explica poquito

plot(difu.rf) ### con más arbolitos, menor será el error 


varImpPlot(difu.rf)  ## importancia de las variables

imp <- importance(difu.rf)

write.csv(imp, file="imp_rf.csv")
 

plot(difu.rf)

hist(treesize(difu.rf))

partialPlot(difu.rf, base3, difuntos)

### predicciones

pred_randomForest <- predict(difu.rf,  )
head(pred_randomForest)

summary(pred_randomForest)



table(Test$difuntos,pred_randomForest)

confusionMatrix(table(Test$difuntos,pred_randomForest))



actual <- Test$difuntos
predicted <- pred_randomForest 


m1<-( createConfusionMatrix(actual, predicted) )

confusionMatrix(actual, predicted)

###### decision tree


rpart.rules(dos)

### 

baseh<-subset(base3, Hospitalizado==1)


library(ggplot2)


ggplot(baseh, aes(x=EDAD))+
  geom_bar()

library(sjPlot)
library(sjlabelled)
library(sjmisc)


sjp_frq(baseh$EDAD)

, type = "hist", show.mean = T)


pred1    0    1
    0 9307  828
    1  160  211


########################

base1$distancia<-base1$FECHA_DEF-base1$FECHA_SINTOMAS

library(lubridate)


summary(base1$FECHA_DEF)

summary(base1$Fech_def)



base1$Fech_def<- (base1$FECHA_DEF)
base1$Fech_def<-recode(base1$Fech_def,"9999-99-99=NA")

summary(base1$FECHA_SINTOMAS)
table(base1$FECHA_SINTOMAS)

library(ggplot2)

table(difuntos$FECHA_DEF)

ggplot(difuntos, aes(x=FECHA_DEF, y=FECHA_SINTOMAS)) + geom_point()

fallecidos<-read.csv("difuntos.csv")

summary(fallecidos)


fallecidos$Diabetes<-(fallecidos$DIABETES)
fallecidos$Diabetes<-recode(fallecidos$Diabetes,"1=1;2=0;98=0")

fallecidos$Epoc<-(fallecidos$EPOC)
fallecidos$Epoc<-recode(fallecidos$Epoc,"1=1;2=0;98=0")

fallecidos$Asma<-(fallecidos$ASMA)
fallecidos$Asma<-recode(fallecidos$Asma,"1=1;2=0;98=0")

fallecidos$Hipertension<-(fallecidos$HIPERTENSION)
fallecidos$Hipertension<-recode(fallecidos$Hipertension,"1=1;2=0;98=0")

fallecidos$Cardiovascular<-(fallecidos$CARDIOVASCULAR)
fallecidos$Cardiovascular<-recode(fallecidos$Cardiovascular,"1=1;2=0;98=0")

fallecidos$Obesidad<-(fallecidos$OBESIDAD)
fallecidos$Obesidad<-recode(fallecidos$Obesidad,"1=1;2=0;98=0")

fallecidos$Tabaquismo<-(fallecidos$TABAQUISMO)
fallecidos$Tabaquismo<-recode(fallecidos$Tabaquismo,"1=1;2=0;98=0")

fallecidos$Renal<-(fallecidos$RENAL_CRONICA)
fallecidos$Renal<-recode(fallecidos$Renal,"1=1;2=0;98=0")


fallecidos$Neumonia<-(fallecidos$NEUMONIA)
fallecidos$Neumonia<-recode(fallecidos$Neumonia,"1=1;2=0;99=0")

fallecidos$Intubado<-(fallecidos$INTUBADO)
fallecidos$Intubado<-recode(fallecidos$Intubado,"1=1;2=0;97=0;99=0")

summary(fallecidos)


write.csv(base3, "base3.csv")






