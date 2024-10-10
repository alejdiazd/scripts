### Not run

### votos 2018 a distritos 2018

### datos en  https://computos2018.ine.mx/#/diputaciones/nacional/1/3/1/1
### archivos zip

### luego de descomprimir, colocar en un folder (directorio de trabajo)

getwd() ## para saber donde estamos en R 


### ¿qué contienen los datos?
### son tres archivos, diputaciones, presidencia y senadurias
### están en csv pero las columnas las separaron con "|"

### estructura general de los tres archivos
### celda a7: nombres de columnas
### celda a8: empiezan los datos
### son 42 columnas

### leer la base 

dip <- read.csv(file="diputaciones.csv", sep="|", 
      skip = 7, header = FALSE)

dim(dip)

head(dip)

summary(dip)


### leer nombres de columnas únicamente

headers <- read.table(file="diputaciones.csv", sep="|",
           skip = 6, header = FALSE, nrows = 1, as.is = T)

### verificar que sean 42 columnas

names(headers)
head(headers)

### algo pasa que pone 44, sabemos que hay NA en 43 y 44
### quitemos esas columnas, no las requerimos

headers <- headers[,-c(43:44)]


### "peguemos" los nombres de las columnas en los datos

colnames(dip)<- headers

### verificamos

head(dip)

dim(dip)

### listo, base con 157,859 renglones (casillas) y 42 columnas

### pero los votos no los lee como números...

summary(dip)


### viene un "-" en unas 200 casillas, movamos eso a NA

dip1<-type.convert(dip, na.strings = "-", dec = ".",
             numerals = c("allow.loss"))

summary(dip1)


### ¿cómo sabemos que ya está bien?
### el PAN tuvo 9962173 votos solo, sin coaliciones
### hagamos la suma (recordar que tenemos 217 casillas...
###  ...donde no había dato y son NA)

sum(dip1[,"PAN"], na.rm = TRUE)

sum(dip1[,"MORENA"], na.rm = TRUE)


### hay que unir bien todos los nombres
### peguemos con un "_" los nombres que están separados
### usemos dplyr, si no se tiene, sólo se instala

install.packages("dplyr")  ### para instalar la librería
library(dplyr)  ### para llamarla

names(dip1) <-
  names(dip1) %>%
  gsub("\\W+", "_", .)


### verificamos

names(dip1)

sum(dip1[,"MOVIMIENTO_CIUDADANO"], na.rm = TRUE)
sum(dip1[,"PAN"], na.rm = TRUE)

### aprovechemos la info urbano / rural ("CASILLA")

dip1$URBANA <- ifelse(dip1$CASILLA=="Urbana",1,0)
dip1$RURAL  <- ifelse(dip1$CASILLA=="Rural" ,1,0)

summary(dip1$CASILLA)
table(dip1$CASILLA)

summary(dip1$URBANA)
summary(dip1$RURAL)

sum(dip1[,"RURAL"], na.rm = TRUE)
sum(dip1[,"URBANA"], na.rm = TRUE)





################

### para distritos

### identificar distritos

dip1$clave_dto <- paste(dip1$ID_DISTRITO, dip1$ID_ESTADO) 

### verificamos

table(dip1$clave_dto)

### primer número: distrito, segundo número: estado

#########################

### identificar coaliciones 

### guía básica de TPM (pri-pvem-panal)

## pri-pvem-panal NO hubo alianza en: 
# Ags 3, BC 8, BCS 2, Chih 9, Cdmx 24, 
# Dur 4, Gto 15, Jal 20, Mich 12, Nay 3, NL 12,
# Pue 15, Que 5, Tab 6, Tam 9, Ver 20

3+8+2+9+24+4+15+20+12+3+12+15+5+6+9+20

### alianza pri-pvem-panal en 133 dtos
### solos en 167 pri y pvem, panal no tuvo cand en Tamps 05

names(dip1)


### veamos cuántos distritos tiene cada estado

table(dip1$NOMBRE_ESTADO, dip1$ID_DISTRITO)

## pri-pvem-panal sin alianza en estados completos 

### veamos los estados

table(dip1$NOMBRE_ESTADO)
table(dip1$ID_ESTADO)

### generemos la dummy para TPM

tpm <- c(1,2,3,8,9,10,11,14,16,18,19,21,22,27,28,30)

### ojo, le ponemos 0,1

dip1$TPM <- ifelse(is.element(dip1$ID_ESTADO, tpm),0,1)

### verificamos

summary(dip1$TPM) ### debe ser como 44-45 por ciento

table(dip1$NOMBRE_ESTADO, dip1$TPM)  ### deben ser los mismos de la guía




### guía básica de PMF (pan-prd-mc)

## pan-prd-mc NO hubo alianza en: 

## NL 12, Mor 5
## también son estados completos

5+12

### alianza pan-prd-mc en 283 dtos
### solos en 17 

### dummy de la alianza pan-prd-mc (NL es 19 y Mor es 17)

pmf <- c(17,19) 

dip1$PMF <- ifelse(is.element(dip1$ID_ESTADO, pmf),0,1)

### verificamos

summary(dip1$PMF) ### debe ser como 94-95 por ciento

table(dip1$NOMBRE_ESTADO, dip1$PMF)  ### deben ser los mismos de la guía



### guía básica de JHH (morena-pt-pes)

## morena-pt-pes NO hubo alianza en: 

## Hid 7, Nay (distrito 03)  
## Hidalgo es completo, Nayarit solo un distrito

7+1

### alianza morena-pt-pes en 292 dtos
### solos en 8 

### dummy de la alianza morena-pt-pes (Hid 13 y Nay es 18)

table(dip1$clave_dto)

### de Nay queremos 3 18

dip1$JHH <- ifelse(dip1$ID_ESTADO==13 |
                   dip1$clave_dto=="3 18",0,1) 


### verificamos

summary(dip1$JHH) ### debe ser como 97 por ciento

table(dip1$NOMBRE_ESTADO, dip1$JHH)  ### deben ser los mismos de la guía


### como va

names(dip1)

str(dip1)

summary(dip1)


### esta es la base a nivel casilla

### si la queremos guardar por alguna razón, haríamos esto:

write.csv(dip1, "dip18_casilla.csv", row.names=FALSE)

## aparecerá en el folder en el cual estamos trabajando 





####### Base en distritos

###### agregando distritos

names(dip1)

### no usaremos algunas columnas

dip0 <- dip1[,-c(1:12, 40:42)]

names(dip0)

### reordenamos columnas

dip0<- dip0[,c(30,1:29,31:33)]

str(dip0)

names(dip0)

############### agregación en distritos

dip_dtos <- aggregate(dip0[,2:33], by=list(dip0[,1]),
   FUN=sum, na.rm=TRUE)

names(dip_dtos)
dim(dip_dtos)

### veamos Morena, eran 20656388

sum(dip_dtos[,"MORENA"], na.rm=TRUE)

## recuperemos la clave por distritos (dto edo)

dip_dtos$clave_dto <- dip_dtos[,1]

names(dip_dtos)

### reordenemos columnas

dip_dtos <- dip_dtos[,c(34,1:33)]

names(dip_dtos)

### usemos Group.1 para tener dto y estado con números

dip_dtos <- dip_dtos %>%
  separate(Group.1, c("dto", "edo"), " ")

### veamos Morena para verificar

names(dip_dtos)

sum(dip_dtos[,"MORENA"], na.rm=TRUE)


summary(dip_dtos)


### ahora las coaliciones

dip_dtos$TPM <- ifelse(dip_dtos$TPM==0,0,1)
dip_dtos$PMF <- ifelse(dip_dtos$PMF==0,0,1)
dip_dtos$JHH <- ifelse(dip_dtos$JHH==0,0,1)

summary(dip_dtos)


### ahora urbano / rural

dip_dtos$URBANO_por <- dip_dtos$URBANA / (dip_dtos$URBANA + dip_dtos$RURAL)

names(dip_dtos)

summary(dip_dtos)


###############################

sum(dip_dtos[,"TOTAL_VOTOS_CALCULADOS"], na.rm=TRUE)

### si queremos total de votos por coalicion donde hubo...

## sumamos todo de PRI and allies

dip_dtos$TOTAL_TPM <- dip_dtos$PRI+dip_dtos$PVEM+dip_dtos$NUEVA_ALIANZA+
   dip_dtos$PRI_PVEM_NA + dip_dtos$PRI_PVEM + dip_dtos$PRI_NA+ 
   dip_dtos$PVEM_NA

names(dip_dtos)

summary(dip_dtos$TOTAL_TPM)

### pongamos ese total donde hubo coalición

dip_dtos$votosTPM <- ifelse(dip_dtos$TPM==1,dip_dtos$TOTAL_TPM,0)


### verificando

summary(dip_dtos$votosTPM)

table(dip_dtos$votosTPM)

plot(dip_dtos$TPM ~ dip_dtos$votosTPM )




### lo mismo para PMF (PAN et al)

names(dip_dtos)

dip_dtos$TOTAL_PMF <- dip_dtos$PAN+dip_dtos$PRD+
   dip_dtos$MOVIMIENTO_CIUDADANO+
   dip_dtos$PAN_PRD_MC + dip_dtos$PAN_PRD + dip_dtos$PAN_MC+ 
   dip_dtos$PRD_MC

names(dip_dtos)

summary(dip_dtos$TOTAL_PMF)

### pongamos ese total donde hubo coalición

dip_dtos$votosPMF <- ifelse(dip_dtos$PMF==1,dip_dtos$TOTAL_PMF,0)


### verificando

summary(dip_dtos$votosPMF)

table(dip_dtos$votosPMF)

plot(dip_dtos$PMF ~ dip_dtos$votosPMF)



#### ahora para Morena et al

names(dip_dtos)

dip_dtos$TOTAL_JHH <- dip_dtos$MORENA+dip_dtos$PT+
   dip_dtos$ENCUENTRO_SOCIAL+
   dip_dtos$PT_MORENA_PES + dip_dtos$PT_MORENA + dip_dtos$PT_PES+ 
   dip_dtos$MORENA_PES


names(dip_dtos)

summary(dip_dtos$TOTAL_JHH)

### pongamos ese total donde hubo coalición

dip_dtos$votosJHH <- ifelse(dip_dtos$JHH==1,dip_dtos$TOTAL_JHH,0)


### verificando

summary(dip_dtos$votosJHH)

table(dip_dtos$votosJHH)

plot(dip_dtos$JHH ~ dip_dtos$votosJHH)

############

names(dip_dtos)

### ya tenemos la base distrital

write.csv(dip_dtos, file="dip_dtos.csv",row.names = FALSE)




### para porcentajes depende qué se quiera
### para sacar sobre todos los votos en urnas... por ej...

dip_dtos$MORENA_por<- dip_dtos$MORENA / dip_dtos$TOTAL_VOTOS_CALCULADOS
dip_dtos$PAN_por<- dip_dtos$PAN / dip_dtos$TOTAL_VOTOS_CALCULADOS
dip_dtos$MC_por<- dip_dtos$MOVIMIENTO_CIUDADANO / dip_dtos$TOTAL_VOTOS_CALCULADOS
dip_dtos$PRD_por<- dip_dtos$PRD / dip_dtos$TOTAL_VOTOS_CALCULADOS
dip_dtos$PRI_por<- dip_dtos$PRI / dip_dtos$TOTAL_VOTOS_CALCULADOS

## solo para ver algo rápido 

plot(dip_dtos$URBANO_por, dip_dtos$MORENA_por, col="red4", pch=1)
 points(dip_dtos$URBANO_por, dip_dtos$PRI_por, col = "green", pch = 19)
 points(dip_dtos$URBANO_por, dip_dtos$PAN_por, col = "blue", pch = 19)


plot(dip_dtos$URBANO_por, dip_dtos$MORENA_por, col="red4", pch=1)
 points(dip_dtos$URBANO_por, dip_dtos$MC_por, col = "orange2", pch = 19)
 points(dip_dtos$URBANO_por, dip_dtos$PRD_por, col = "yellow2", pch = 19)



#############################


dip <- read.csv(file="Diputaciones_2021_1100.csv")

names(dip)

dip1<-type.convert(dip, na.strings = c("-0","Sin dato","Ilegible","Sin acta"), 
          dec = ".", numerals = c("allow.loss"))

summary(dip1)

names(dip1)


####### Base en distritos

###### clave edo - distrito

dip1$clave_dto <- dip1$ID_ESTADO*100 + dip1$ID_DISTRITO 

table(dip1$clave_dto)

names(dip1)


### no usaremos algunas columnas

dip0 <- dip1[,-c(1:19, 46:57)]

names(dip0)

summary(dip0)


############### agregación en distritos

dip_dtos <- aggregate(dip0[,1:26], by=list(dip0[,27]),
   FUN=sum, na.rm=TRUE)

names(dip_dtos)
dim(dip_dtos)

summary(dip_dtos)

head(dip_dtos)

write.csv(dip_dtos, file="dip_dtos_1100.csv",row.names = FALSE)




##############

uno<- read.csv("pluris9721.csv")

names(uno)


library(tidyr)

# The arguments to gather():
# - data: Data object
# - key: Name of new key column (made from names of data columns)
# - value: Name of new value column
# - ...: Names of source columns that contain values
# - factor_key: Treat the new key column as a factor (instead of character vector)


dos <- gather(uno, partido, measurement, PAN:Total, factor_key=TRUE)

head(dos)


write.csv(dos, file="dip9721.csv",row.names = FALSE)

###############


ls()

va <- read.csv("dip9721.csv")

names(va)


library(ggplot2)


ggplot(va, aes(x=Eleccion, y=Porcentaje*100, group=Pista, col=Partido))+
  geom_bar()+
 #geom_line()+
  facet_wrap(Partido~Pista) +theme_bw()+
  ylab("%")+ scale_x_continuous(breaks=seq(1997, 2021, 6))+
   theme(legend.position="none")+ xlab("")+
 theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=8)) 


 facet_wrap(Partido~Pista,scales="free_y") +theme_bw()+


##########################

va <- read.csv("conteo_grafica.csv")

names(va)


library(ggplot2)


library(tidyr)

# The arguments to gather():
# - data: Data object
# - key: Name of new key column (made from names of data columns)
# - value: Name of new value column
# - ...: Names of source columns that contain values
# - factor_key: Treat the new key column as a factor (instead of character vector)


dos <- gather(va, partido, measurement, PAN_PRI_PRD_min:PP_max, factor_key=TRUE)


names(dos)
summary(dos)
head(va)

library(car)

va$Estado <- reorder(va$Estado , va$Morena_mean)

a <- ggplot(va, aes(x=Estado, y= Morena_mean*100))+
  geom_line(col="red4") +
  geom_point(col="red4")+
  geom_errorbar(aes(ymin=Morena_min*100, ymax=Morena_max*100), width=.2,
                 position=position_dodge(0.05), col="red4") +
   theme_bw()+ ylab("% Conteo rápido INE")+xlab("") + ylim(0,60)+
  coord_flip() + ggtitle("Gubernaturas Morena 2021")




va$State<- reorder(va$Estado , va$PAN_PRI_PRD_mean)

b <- ggplot(va, aes(x=State, y= PAN_PRI_PRD_mean*100))+
  geom_line(col="blue") +
  geom_point(col="blue")+
  geom_errorbar(aes(ymin=PAN_PRI_PRD_min*100, ymax=PAN_PRI_PRD_max*100), 
           width=.2, position=position_dodge(0.05), col="blue") +
   theme_bw()+ ylab("% Conteo rápido INE")+xlab("") + ylim(0,60)+
  coord_flip() + ggtitle("Gubernaturas PAN_PRI_PRD 2021")





va$State2<- reorder(va$Estado , va$part_cdna_mean)


ggplot(va, aes(x=State2, y=part_cdna_mean*100))+
  geom_line(col="grey20") +
  geom_point(col="grey20")+
  geom_errorbar(aes(ymin=part_cdna_min*100, ymax=part_cdna_max*100), 
           width=.2, position=position_dodge(0.05), col="grey20") +
   theme_bw()+ ylab("% Conteo rápido INE")+xlab("") + ylim(0,70)+
  coord_flip() + ggtitle("Participación ciudadana 2021")+
 geom_hline(yintercept=52.676, linetype="dashed", col="grey60")




  geom_errorbar(aes(ymin=, ymax=)) +
  geom_line()+
  facet_wrap(Estado~.) +theme_bw()  +
  ylab("%")+ scale_x_continuous(breaks=seq(1997, 2021, 6))+
   theme(legend.position="none")+ xlab("")+
 theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=8)) 


 facet_wrap(Partido~Pista,scales="free_y") +theme_bw()+


#########

library(readxl)

uno<- read_excel("gubernaturas_antes_hoy.xls")

names(uno)
dim(uno)

head(uno)
summary(uno)

uno$State2<- reorder(uno$Estado , uno$dif_conteo_prep)


ggplot(uno, aes(x=State2, y=dif_conteo_prep*100))+
  geom_line(col="grey20") +
  geom_point(col="grey20")+
   theme_bw()+ ylab("Porcentajes, fuente: INE")+xlab("") + ylim(-3,10)+
  coord_flip() +   
 ggtitle("Diferencias Conteos rápidos - PREP \n en participación ciudadana 2021") +
  geom_hline(yintercept=0, linetype="dashed", col="grey60")+
 scale_y_continuous(breaks=seq(-3, 10, 1)) 








  geom_errorbar(aes(ymin=part_cdna_min*100, ymax=part_cdna_max*100), 
           width=.2, position=position_dodge(0.05), col="grey20") +


##############

dip <- read.csv(file="diputaciones_final.csv")

dim(dip)

names(dip)

table(dip$CASILLA)

library(car)

### aprovechemos la info urbano / rural ("CASILLA")

dip$URBANA <- ifelse(dip$CASILLA=="Urbana",1,0)
dip$RURAL  <- ifelse(dip$CASILLA=="No urbana" ,1,0)

summary(dip$CASILLA)
table(dip$CASILLA)

summary(dip$URBANA)
summary(dip$RURAL)

sum(dip[,"RURAL"], na.rm = TRUE)
sum(dip[,"URBANA"], na.rm = TRUE)

summary(dip)

dip$BASICA  <- ifelse(dip$TIPO_CASILLA=="B" ,1,0)
dip$CONTIGUA  <- ifelse(dip$TIPO_CASILLA=="C" ,1,0)
dip$EXTRAORDINARIA<- ifelse(dip$TIPO_CASILLA=="E" ,1,0)
dip$PRISION<- ifelse(dip$TIPO_CASILLA=="P" ,1,0)
dip$ESPECIAL  <- ifelse(dip$TIPO_CASILLA=="S" ,1,0)


table(dip$EXT_CONTIGUA)

sum(dip[,"EXTRAORDINARIA"], na.rm = TRUE)


dip$claves <- paste(dip$ID_DISTRITO, dip$ID_ESTADO) 

### verificamos

table(dip$clave_dto)
table(dip$claves)

table(dip$clave_dto, dip$claves)


names(dip)



## identificar distritos 

dip$clave_dto <- (dip$ID_ESTADO*100) + dip$ID_DISTRITO   

### verificamos

table(dip$clave_dto)

### primer número: estado, segundo número: distrito


### no usaremos algunas columnas

names(dip)

dip0 <- dip[,-c(1:12, 36:38)]

names(dip0)


############### agregación en distritos

dip_dtos <- aggregate(dip0[,1:30], by=list(dip0[,32]),
   FUN=sum, na.rm=TRUE)

names(dip_dtos)
dim(dip_dtos)

head(dip_dtos)

dip_dtos$URBANO <- dip_dtos$URBANA / (dip_dtos$URBANA+ dip_dtos$RURAL)

dip_dtos$PART<- dip_dtos$TOTAL_VOTOS_CALCULADOS/dip_dtos$LISTA_NOMINAL_CASILLA 


dip_dtos$CASILLAS <- dip_dtos$BASICA+dip_dtos$CONTIGUA+
    dip_dtos$EXTRAORDINARIA+dip_dtos$PRISION+dip_dtos$ESPECIAL


dip_dtos$BASICA_por<- dip_dtos$BASICA/dip_dtos$CASILLAS 
dip_dtos$CONTIGUA_por<- dip_dtos$CONTIGUA/dip_dtos$CASILLAS 
dip_dtos$EXTRAORDINARIA_por<- dip_dtos$EXTRAORDINARIA/dip_dtos$CASILLAS 
dip_dtos$PRISION_por<- dip_dtos$PRISION/dip_dtos$CASILLAS 
dip_dtos$ESPECIAL_por<- dip_dtos$ESPECIAL/dip_dtos$CASILLAS 








summary(dip_dtos)

plot(dip_dtos$URBANO, dip_dtos$MORENA)
plot(dip_dtos$URBANO, dip_dtos$PAN)
plot(dip_dtos$URBANO, dip_dtos$PART)


library(ggplot2)





dip_dtos$JHH <- ifelse(dip_dtos$PVEM.PT.MORENA!=0,1,0)
dip_dtos$VXM <- ifelse(dip_dtos$PAN.PRI.PRD!=0,1,0)


dip_dtos$JHH <- ifelse(dip_dtos$JHH==1, 
   dip_dtos$PVEM+dip_dtos$PT+dip_dtos$MORENA+dip_dtos$PVEM.PT.MORENA+
   dip_dtos$PVEM.PT+dip_dtos$PVEM.MORENA+dip_dtos$PT.MORENA,0)
 
dip_dtos$VXM <- ifelse(dip_dtos$VXM==1, 
   dip_dtos$PAN+dip_dtos$PRI+dip_dtos$PRD+dip_dtos$PAN.PRI.PRD+
   dip_dtos$PAN.PRI+dip_dtos$PAN.PRD+dip_dtos$PRI.PRD,0)


dip_dtos$PAN_por<- dip_dtos$PAN/dip_dtos$TOTAL_VOTOS_CALCULADOS
dip_dtos$PRI_por<- dip_dtos$PRI/dip_dtos$TOTAL_VOTOS_CALCULADOS
dip_dtos$PRD_por<- dip_dtos$PRD/dip_dtos$TOTAL_VOTOS_CALCULADOS
dip_dtos$PVEM_por<- dip_dtos$PVEM/dip_dtos$TOTAL_VOTOS_CALCULADOS
dip_dtos$PT_por<- dip_dtos$PT/dip_dtos$TOTAL_VOTOS_CALCULADOS
dip_dtos$MC_por<- dip_dtos$MC/dip_dtos$TOTAL_VOTOS_CALCULADOS
dip_dtos$MORENA_por<- dip_dtos$MORENA/dip_dtos$TOTAL_VOTOS_CALCULADOS
dip_dtos$JHH_por<- dip_dtos$JHH/dip_dtos$TOTAL_VOTOS_CALCULADOS
dip_dtos$VXM_por<- dip_dtos$VXM/dip_dtos$TOTAL_VOTOS_CALCULADOS


dip_dtos$CANDNOREG_por<- dip_dtos$CANDIDATO.A.NO.REGISTRADO.A/dip_dtos$TOTAL_VOTOS_CALCULADOS

dip_dtos$NULOS_por<- dip_dtos$VOTOS.NULOS/dip_dtos$TOTAL_VOTOS_CALCULADOS


dip_dtos$NULOS_TOTAL_por<-dip_dtos$CANDNOREG_por+dip_dtos$NULOS_por



dip_dtos$JHH_dummy <- ifelse(dip_dtos$PVEM.PT.MORENA!=0,1,0)
dip_dtos$VXM_dummy <- ifelse(dip_dtos$PAN.PRI.PRD!=0,1,0)


names(dip_dtos)



t.test(dip_dtos$NULOS_TOTAL_por~ dip_dtos$VXM_dummy)

summary(dip_dtos$NULOS_TOTAL_por)



ggplot(dip_dtos , aes(x=URBANO*100, y=PAN_por*100))+
   geom_point()+xlim(0,100)+ylim(0,100)+facet_wrap(edo~.)


plot(dip_dtos$URBANO, dip_dtos$MORENA_por, col="red4", pch=1)
 points(dip_dtos$URBANO, dip_dtos$PRI_por, col = "green", pch = 19)
 points(dip_dtos$URBANO, dip_dtos$PAN_por, col = "blue", pch = 19)


plot(dip_dtos$URBANO, dip_dtos$MORENA_por, col="red4", pch=1)
 points(dip_dtos$URBANO, dip_dtos$MC_por, col = "orange2", pch = 19)
 points(dip_dtos$URBANO, dip_dtos$PRD_por, col = "yellow2", pch = 19)

plot(dip_dtos$VXM_por, dip_dtos$MC_por, col="orange2", pch=1)
 points(dip_dtos$VXM_por, dip_dtos$JHH_por, col = "red2", pch = 19)



m1 <- lm(MORENA_por ~ PART + I(PART*PART)+ URBANO+I(URBANO*URBANO)+
  factor(JHH_dummy)+factor(VXM_dummy)+MC_por, data=dip_dtos)

m2 <- lm(PAN_por ~ PART +I(PART*PART)+ URBANO+I(URBANO*URBANO)+
  factor(JHH_dummy)+factor(VXM_dummy)+MC_por, data=dip_dtos)

summary(m1)
summary(m2)

library(lmtest)


bptest(m1)

library(sjPlot)

plot_model(m2, type="std2")

plot_model(m2, type="pred", term="URBANO")


names(dip_dtos)

### usemos Group.1 para tener dto y estado con números

library(tidyr)
dip_dtos <- dip_dtos %>%
  separate(Group.1, c("edo", "dto"), " ")

table(dip_dtos$dto)


table(dip$ID_ESTADO, dip$NOMBRE_ESTADO)


###########
library(readxl)

vapormex <- read_excel("C:/Users/usuario/Documents/gastos 2018/gastos 2021/gastos2021.xls", sheet=2)

names(vapormex)


names(vapormex) <-
  names(vapormex) %>%
  gsub("\\W+", "_", .)


table(vapormex$DISTRITO)


va1 <- vapormex %>%
  separate(DISTRITO, c("dto", "cabecera"), "-")

names(va1)
dim(va1)


### verificamos


write.csv(va1, file="jhh21.csv")


va<- read.csv("vapormex.csv") 

names(va)
dim(va)


computos<- read.csv("computos.csv")
names(computos)
dim(computos)


all <- merge(computos, va, by="ID_EDO_DTO"), all.x=TRUE, all.y=TRUE)


dim(all)

names(all)

table(all$VXM_dummy.x, all$VXM_dummy.x)

summary(all$VXM_dummy.y)


write.csv(dip_dtos, file="computos.csv")

va$ID_EDO_DTO <- (va$ID_ESTADO*100) + va$ID_DISTRITO



qplot(URBANO, gto_tope, data=all)


t.test( all$gto_tope~ all$Female)



write.csv(all, file="jhh2021.csv")



###

va<- read.csv("jhh21.csv") 


##############


va<- read.csv("vxm2021.csv") 
mor <- read.csv("jhh2021.csv") 

uno <- read.csv("datos2018.csv")

names(uno)

gobpan <- c(1,3,8,18,22,10,11,23,28,31)

gobpri <- c(4,5,6,12,13,15,24,25,26,29,20,32)

gobmor <- c(2,7,9,17,21,27,30)

gob2021 <- c(29,32,24,25,26,22,19,18,16,6,8,12,2,3,4)


uno$gobpan<- ifelse(is.element(uno$ID_EDO, gobpan),0,1)

uno$gobpri<- ifelse(is.element(uno$ID_EDO, gobpri),0,1)

uno$gobmor<- ifelse(is.element(uno$ID_EDO, gobmor),0,1)

uno$gob2021<- ifelse(is.element(uno$ID_EDO, gob2021),0,1)


dim(uno)
dim(va)
dim(mor)

 

all <- merge(uno, va, by="ID_EDO_DTO")


all <- merge(uno, mor, by="ID_EDO_DTO")

dim(all)

names(all)


), all.x=TRUE, all.y=TRUE)


m0<- lm(gto_tope ~ Female + URBANO + gobpri + gobmor + PAN_por.x + 
    MORENA_por.x + N_2018 , data = all)

summary(m0)

m1<- lm(gto_tope ~ Female + URBANO+gobpri+gobmor+
       PAN_por.x+  PRD_por.x+ gob2021+ 
       N_2018+ MORENA_por.x, data=all)

summary(m1)

library(lmtest)

bptest(m1)

write.csv(all, file="jhh_final.csv")





