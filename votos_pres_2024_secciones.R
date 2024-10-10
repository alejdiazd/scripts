### Not run

### votos 2024 a secciones

### datos en  
### archivos zip

### luego de descomprimir, colocar en un folder (directorio de trabajo)

getwd() ## para saber donde estamos en R 


### que contienen los datos?
### son tres archivos, diputaciones, presidencia y senadurias
### estan en csv pero las columnas las separaron con "|"

### estructura general de los tres archivos
### celda a7: nombres de columnas
### celda a8: empiezan los datos
### son 34 columnas

### leer la base 

pres <- read.csv(file="PRES_2024.csv", sep="|", 
      skip = 7, header = T)

dim(pres)

names(pres)

head(pres)

summary(pres)

### pero los votos no los lee como numeros...


### viene un "-" NA

pres<-type.convert(pres, na.strings = "-", dec = ".",
             numerals = c("allow.loss"))

summary(pres)


pres$SECCION <- as.numeric(gsub("\\D", "", pres$SECCION))

table(pres$SECCION)


pres$XG <- pres$PAN + pres$PRI + pres$PRD + 
 pres$PAN_PRI_PRD + pres$PAN_PRI + pres$PAN_PRD + pres$PRI_PRD

pres$CS <- pres$PVEM + pres$PT + pres$MORENA + 
 pres$PVEM_PT_MORENA+ pres$PVEM_PT+ pres$PVEM_MORENA+ pres$PT_MORENA


pres$Maynez <- pres$MC

### XG obtuvo 16,502,697 votos 
### hagamos la suma 

sum(pres[,"XG"], na.rm = TRUE)


sum(pres[,"CS"], na.rm = TRUE)


sum(pres[,"Maynez"], na.rm = TRUE)


### hay que unir bien todos los nombres
### peguemos con un "_" los nombres que estan separados
### usemos dplyr, si no se tiene, se instala

install.packages("dplyr")  ### para instalar la librer�a
library(dplyr)  ### para llamarla

names(pres) <-
  names(pres) %>%
  gsub("\\W+", "_", .)


### verificamos

names(pres)

sum(pres[,"Maynez"], na.rm = TRUE)
sum(pres[,"XG"], na.rm = TRUE)
sum(pres[,"CS"], na.rm = TRUE)

### aprovechemos la info urbano / rural ("CASILLA")

table(pres$CASILLA)

pres$NO_URBANA <- ifelse(pres$CASILLA=="No Urbana",1,0)
pres$URBANA    <- ifelse(pres$CASILLA=="Urbana",1,0)

summary(pres$URBANA)
summary(pres$NO_URBANA)

sum(pres[,"NO_URBANA"], na.rm = TRUE)
sum(pres[,"URBANA"], na.rm = TRUE)

names(pres)



################


## ahora entidad, distrito, seccion

pres$clave_entidad_dto_seccion <- paste(pres$ID_ENTIDAD,
  pres$ID_DISTRITO_FEDERAL,pres$SECCION,sep="-") 


summary(pres)

head(pres$clave_entidad_dto_seccion)

names(pres)
dim(pres)

#########################



### esta es la base a nivel casilla

### si la queremos guardar:

write.csv(pres, file="pres24_casilla.csv", row.names=FALSE)





####### Base en secciones

###### agregando secciones

names(pres)
dim(pres)

table(pres$TIPO_CASILLA)


### no usaremos algunas columnas

pres0 <- pres[,-c(1:27, 32:34)]

names(pres0)

### reordenamos columnas

pres0<- pres0[,c(10,5:7,1:4,8:9)]

names(pres0)

############### agregar en secciones

pres_sec<- aggregate(pres0[,2:10], by=list(pres0[,1]),
   FUN=sum, na.rm=TRUE)

names(pres_sec)
dim(pres_sec)

### veamos XG, eran  16,502,697 menos exterior 
### XG en territorio nacional 16,416,179

sum(pres_sec[,"XG"], na.rm=TRUE)

## recuperemos la clave 

pres_sec$clave_entidad_dto_seccion <- pres_sec[,1]

names(pres_sec)

### reordenemos columnas

pres_sec <- pres_sec[,c(11,1:10)]

names(pres_sec)

### usemos Group.1 para recuperar
library(tidyr)
library(dplyr)


pres_sec <- pres_sec %>%
  separate(Group.1, c("entidad", "distrito", "seccion"))


sum(pres_sec[,"TOTAL_VOTOS_CALCULADOS"], na.rm=TRUE)
sum(pres_sec[,"XG"], na.rm=TRUE)

dim(pres_sec)

names(pres_sec)

head(pres_sec)


############

names(pres_sec)

summary(pres_sec)

dim(pres_sec)


### que lea en numeros entidad, distrito, seccion


pres_sec$entidad<- as.numeric(gsub("\\D", "", pres_sec$entidad))

pres_sec$distrito<- as.numeric(gsub("\\D", "", pres_sec$distrito))

pres_sec$seccion<- as.numeric(gsub("\\D", "", pres_sec$seccion))

summary(pres_sec)

sum(pres_sec[,"casillas_totales"], na.rm=TRUE)


##############

pres_sec$casillas_totales <- pres_sec$NO_URBANA + pres_sec$URBANA

summary(pres_sec)



### ya tenemos la base por seccion

write.csv(pres_sec, file="pres_sec.csv",row.names = FALSE)


##### End

