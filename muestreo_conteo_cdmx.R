
## muestreo conteo rápido cdmx


ls()

library(readxl)

todo <- read_excel("MarcoDeMuestreo.xlsx")

dim(todo)
names(todo)


addmargins(table(todo$CVE_ALC, todo$CIRCUNS))

addmargins(table(todo$CVE_ALC, todo$ESTRATO))

# Z2 pq / e2

 ( (1.96 * 1.96) * (0.5 * 0.5) ) / (0.02 * 0.02)


3.8416  * 0.25
0.9604 / 0.0004


## Paso 1, ordenemos  

# primero sacamos proporciones
## asumiendo que LN / PE da algo como densidad

names(todo)

# todo$denso <- todo$LN /  todo$PE

# summary(todo$denso)

### Ordenando (renglones)

todo_ordenado <- todo[order(todo$ESTRATO),]

head(todo_ordenado$ESTRATO)


#head(todo_ordenado$denso)


##############

## Paso 2, de 13175 casillas queremos 2401 casos

13175/2401

## esto se llama salto sistemático
# es decir tengo que elegir una casilla mas o menos cada 5 casillas

# el monstruo generador de números aleatorios me dijo: "pos ponle 9"

# entonces elijo el renglón 9 y luego cada 5

9
9 +5 = 14
14+5 = 19
19+5 = 24


generar_secuencia <- function(cantidad) {
  # Iniciar con el número 9
  numero_inicial <- 9
  # Incremento cada 5
  incremento <- 5
  # Generar la secuencia
  secuencia <- seq(numero_inicial, by = incremento, 
        length.out = cantidad)
  
  return(secuencia)
}

# Generar una secuencia de 2401 números
resultado <- generar_secuencia(2401)
print(resultado)

###########

# Paso 3 correr la muestra

muestra_cdmx0 <- todo2[resultado,]


head(muestra_cdmx0$CVE_ALC)
head(muestra_cdmx0$ESTRATO)
head(muestra_cdmx0$CIRCUNS)

dim(muestra_cdmx0)

#write.csv(muestra_cdmx0, file="muestra_cdmx0.csv")

addmargins(table(muestra_cdmx0$CVE_ALC, muestra_cdmx0$CIRCUNS))
addmargins(table(muestra_cdmx0$CVE_ALC, muestra_cdmx0$ESTRATO))

names(muestra_cdmx0)

#########################

fer <- read.csv("C:/Users/Alejandro Diaz/Documents/cotecora/fer_cdmx_final.csv")

dim(fer)

table(fer$CASILLA)

library(car)

fer$CASILLA0 <- recode(fer$CASILLA, "'B1'='B'")

table(fer$CASILLA0)



fer$CASILLA_F <- paste(fer$SECCION, fer$CASILLA0, sep="-")

table(fer$CASILLA_F)
table(muestra1$CASILLA_F)
table(muestra2$CASILLA_F)




todo1$CASILLA_F <- todo1$CASILLA
todo2$CASILLA_F <- todo2$CASILLA

muestra1$CASILLA_F <- muestra1$CASILLA
muestra2$CASILLA_F <- muestra2$CASILLA


uno <- c(muestra1$CASILLA_F)
fer$uno <- ifelse(is.element(prueba$CASILLA_F, uno),1,0)

dos <- c(muestra2$CASILLA_F)
fer$dos <- ifelse(is.element(prueba$CASILLA_F, dos),1,0)





addmargins(table(muestra_cdmx0$CVE_ALC, muestra_cdmx0$CIRCUNS))

names(prueba)

sum(fer$MORENA,na.rm=TRUE)

table(fer$DEMARCACION)

todo$CASILLA_F <- todo$CASILLA

fer <- merge(fer, todo, by="CASILLA_F", all.x=T)

names(fer)
dim(fer)

fer$part_por <- fer$VOTACION_TOTAL_EMITIDA / fer$LN

fer$mor_por <- fer$MORENA / fer$VOTACION_TOTAL_EMITIDA
fer$pt_por <- fer$PT / fer$VOTACION_TOTAL_EMITIDA
fer$pt_mor_por <- fer$PT_MORENA / fer$VOTACION_TOTAL_EMITIDA

fer$pan_por <- fer$PAN / fer$VOTACION_TOTAL_EMITIDA
fer$pri_por <- fer$PRI / fer$VOTACION_TOTAL_EMITIDA
fer$prd_por <- fer$PRD / fer$VOTACION_TOTAL_EMITIDA

summary(fer$pan_por)
summary(fer$mor_por)
summary(fer$part_por)


#############################################

sum(subset(fer,DEMARCACION=='BENITO JUÁREZ')$mor_por)
sum(subset(fer,DEMARCACION=='BENITO JUÁREZ')$pan_por)

sum(subset(fer,DEMARCACION=='BENITO JUÁREZ')$MORENA)
sum(subset(fer,DEMARCACION=='BENITO JUÁREZ')$PAN)
sum(subset(fer,DEMARCACION=='BENITO JUÁREZ')$VOTACION_TOTAL_EMITIDA)
sum(subset(fer,DEMARCACION=='BENITO JUÁREZ')$LN)




names(fer)
primera <- subset(fer, uno==1)
dim(primera)

segunda <- subset(fer, dos==1)
dim(segunda)


sum(subset(segunda,DEMARCACION=='BENITO JUÁREZ')$MORENA)
sum(subset(segunda,DEMARCACION=='BENITO JUÁREZ')$PAN)
sum(subset(segunda,DEMARCACION=='BENITO JUÁREZ')$VOTACION_TOTAL_EMITIDA)
sum(subset(segunda,DEMARCACION=='BENITO JUÁREZ')$LN)


# Oficiales
162049/238447  #pan
# 0.6796018
 45724/238447  #morena
# 0.1917575
 238447/374478
# 0.636745 


# Muestra mía
32185/47398    #pan
9031/47398     #morena
47398/74696    #part



sum(subset(prueba,DEMARCACION=='AZCAPOTZALCO')$MORENA)
sum(subset(prueba,DEMARCACION=='AZCAPOTZALCO')$PT)
sum(subset(prueba,DEMARCACION=='AZCAPOTZALCO')$PT_MORENA)
sum(subset(prueba,DEMARCACION=='AZCAPOTZALCO')$VOTACION_TOTAL_EMITIDA)


sum(subset(prueba,DEMARCACION=='XOCHIMILCO')$mor_por)
sum(subset(prueba,DEMARCACION=='XOCHIMILCO')$pt_por)
sum(subset(prueba,DEMARCACION=='XOCHIMILCO')$pt_mor_por)


####################################################


library(readxl)

todo <- read_excel("MarcoDeMuestreo.xlsx")

dim(todo)
names(todo)


addmargins(table(todo$CVE_ALC, todo$CIRCUNS))

addmargins(table(todo$CVE_ALC, todo$ESTRATO))


deleg1 <- c(4,6,8,9,11)
deleg2 <- c(1,2,3,5,7,10,12,13,14,15,16,17)

todo$deleg1 <- ifelse(is.element(todo$CVE_ALC, deleg1),1,0)
todo$deleg2 <- ifelse(is.element(todo$CVE_ALC, deleg2),1,0)

table(todo$deleg1)
table(todo$deleg2)

 
todo1 <- subset(todo, deleg1==1)
todo2 <- subset(todo, deleg2==1)

dim(todo1)
dim(todo2)

table(todo1$CVE_ALC)
table(todo2$CVE_ALC)


sample1 <- todo1[with(todo1,order(CVE_ALC,ESTRATO)),]
sample2 <- todo2[with(todo2,order(CVE_ALC,ESTRATO)),]


dim(sample1)
dim(sample2)

head(sample1)
head(sample2)


############

generar_secuencia <- function(cantidad) {
  # Iniciar con el número 3
  numero_inicial <- 3
  # Incremento cada 5
  incremento <- 5
  # Generar la secuencia
  secuencia <- seq(numero_inicial, by = incremento, 
        length.out = cantidad)
  return(secuencia)
}

#################

# Generar una secuencia de 378 números
resultado1 <- generar_secuencia(378)
print(resultado1)

# Generar una secuencia de 2023 números
resultado2 <- generar_secuencia(2023)
print(resultado2)

###########

# Paso 3 correr la muestra

muestra1 <- todo1[resultado1,]
muestra2 <- todo2[resultado2,]



### write.csv(muestra1, file="muestra1.csv")

addmargins(table(pruebita$CVE_ALC, pruebita$ESTRATO))
addmargins(table(pruebita$DEMARCACION, pruebita$ESTRATO))

########## notas 

# se conformaron dos grupos, alcaldias con 3 y 6 estratos
# n con fórmula tradicional: Z2 pq / e2, con z=1.96, p=0.5
# n = 2401
# casillas grupo 1 entre gran total 13175
# casillas grupo 2 entre gran total 13175
# proporciones uno y dos 
# entonces 2401*proorcion uno = 378 y 2401*propor dos =2023 
# muestras casilla grupo 1 / n (378)
# muestras casilla grupo 2 / n (2023)
# luego para la primera: salto 1918 entre 378 cada 5
# para la segunda: salto 11257 entre 378 cada 5
# se corre la generación de secuencia
# se hace la secuencia para ambos grupos
# se corren secuencias sobre cada grupo para obtener las muestras 
# se crea una dummy para cada muestras sobre resultados oficiales
# se compara qué tal sale
