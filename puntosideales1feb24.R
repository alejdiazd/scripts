### Not run

### Ideal Points, Leg LXIII, México

### Fuente: Knight 2018
## article & data: https://brazilianpoliticalsciencereview.org/article/strategic-coalitions-and-agenda-setting-in-fragmented-congresses-how-the-pri-sets-the-legislative-agenda-in-mexico/

### archivo en stata ya disponible en canvas
### ver el módulo que se llama datos

### hay que ponerlo en un directorio y saber que ese directorio
### es donde estamos trabajando
### Ahora, para saber dónde estamos

getwd()

### usemos haven para leerlo 

install.packages("haven")
library(haven)

      read_spss                    .sav
                                   .por

uno<- read_stata("mexico20152016allvars_knight.dta")

dim(uno)

names(uno)


table(uno$Deputy)

### parece que hay que limpiarlo
### al final se ve Date, Tema, Treshold, Vote # 
### esos renglones son útiles, 
### pero por no ahora no los requerimos

### guardemos el archivo como csv
### vamos a evitar que los missing values 
###   los lea como NA, mejor 99
### para que en wnominate no tengamos string en vez de valores

write.csv(uno, file="knight63.csv", row.names = FALSE, na='99')


### notar que usamos row.names en False para que no crear
### una columna de consecutivos, porque no la vamos a usar

### el archivo csv aparecerá en nuestro directorio de trabajo

### si se inspecciona en cualquier hoja de c�lculo
### se ver� que en realidad los datos 
### que s� requerimos empiezan en la celda a6
### pero los t�tulos empiezan en a1

### llamenos a nuestro csv

dos <- read.csv("knight63.csv")

dim(dos)
names(dos)

### rutina para que la pantalla se vea Rstudio
## options(max.print = 10000)

summary(dos)

### recodar: necesitamos legislator, party, votes

### Deputy es legislator (d001 a dxyz...)
### name pues es �til para saber qui�n es, pero no lo requerimos
### party si lo requerimos (viene en n�meros)
### Service_record son anotaciones de cambios de partido, etc
### es �til codificarlo cuando queremos ver si votaron
### distinto al cambiarse de bancada. 
### Hay quien regresa a la bancada inicial (cosas veredes...)
### pero por ahora no lo requerimos

### en síntesis, col 2 y 4 no las usaremos

dos <- dos[,-c(2,4)]  ### remueve columnas 2 y 4 únicamente

names(dos)

### ahora tenemos Deputy, party & votes

dim(dos)

### pero falta quitar los renglones que no requerimos

head(dos)

## vamos a remover renglones 1 a 4 (column name no cuenta como renglón)

dos <- dos[-c(1:4),]  

dim(dos)

head(dos)

### ahora estamos listos
### �qu� tenemos? (despu�s de un poco de wrestling with data)

### 305 votaciones
### 535 dips
### Leg LXIII 
### fechas 15oct2015 - 15dic2016

###########################

##### ahora puntos ideales

install.packages("wnominate") 
## por si alguien no la habia bajado
library(wnominate)

#### vamos a decirle a wnominate: legislator, party, votes

dip <- dos[, 1]   ### dips en column 1
legData <- matrix(dos[, 2], length(dos[, 2]), 1) ### partidos en col 2
colnames(legData) <- "party"  ### así se llaman los partidos
dos<- dos[, -c(1, 2)]   ### y los votos

### Robert Knight les puso n�meros a los partidos, no sus nombres

### Veamos que hay en los votos

table(dos$v305)
table(dos$v095)
table(dos$v238)
table(dos$v062)


### vamos al libro de códigos...

### 0    No (Contra)
### 1    Yes (Favor)
### 5    Abstain (Abstención)
### 7    Absent (Ausente)
### 11   Quorum vote (Quórum) (does not count for vote totals)
### 99   nosotros lo pusimos para los NA

summary(dos$v062)  ### lee los NA como 99, como queremos

### entonces ya le podemos decir a roll call que calcule

### yo diría: favor=1, contra=0, missing=5,7,11 y notinLeg=99
### peeeero estamos replicando

rc <- rollcall(dos, yea = c(1), nay = c(0,5),
  missing = c(7,11), notInLegis = 99, 
  legis.names = dip,
  legis.data = legData, 
  desc = "Leg 63 Votes",
  source = "Knight 2018")



### Hay que destacar que Knight estima abstenciones como nay
### aqu� se pusieron como Knight
### hay un debate porqu� de una forma u otra
## yo prefiero abstenciones como missing (pero quiz� es aburrido)

### polaridad

result <- wnominate(rc, polarity = c(464, 1))

### la polaridad no viene por default, 
## habría que buscar
### en raw data a quien vota más a favor
### y quien vota más en contra 

### Prof Keith Poole señala:
## "users will likely wish to orient conservaties 
#  on the right 
## and liberals on the left, 
# so "positive" in this case,
## generally means conservative" 
## (Poole et al 2007: 4)

### se estim� la polaridad en l�nea con Knight
### yo pondr�a (1, 464) pero la nueva tendencia es como ya se estim�
### quiz� porque uno ya no innova y hace lo que tiene sentido
### para los viejitos, como se�ala el prof Poole


summary(result)

plot(result)


leg63_knight <- summary(result, verbose=TRUE)


### por si se quiere guardar de recuerdo...
write.csv(leg63_knight, file="legis63_knight.csv")


plot.coords(result) ### 2 dimensions en grande
plot.angles(result) 
plot.cutlines(result) ### apenas unas pocas votaciones en 2da dimensi�n...
plot.scree(result) ## la 2da dimensi�n explica poquito...



### Coodebook de partidos (Knight original)

# 1    PRI (Partido Revolucionario Institucional)
# 2    PAN (Partido Acci�n Nacional)
# 3    PRD (Partido de la Revolucionario Democr�tica)
# 4    PT (Partido Trabajadores)
# 5    PVEM (Partido Verde Ecologista de M�xico) 
# 6    INDEP (Independientes)
# 7    PSN (Partido de la Sociedad Nacionalista)
# 8    Conv (Convergencia por la Democracia)�
#      Renamed MC (Movimiento Ciudadano) during the LXI Congress 
# 9    PAS (Partido Alianza Social)
#10    PNA (Partido Nueva Alianza)
#11    PA (Partido Alternativa)
#12    MRN �Morena� (Movimiento de Regeneraci�n Nacional)
#13    PES (Partido Encuentro Social)


install.packages("ggplot2")

library(ggplot2)

names(leg63_knight)

### metemos a los partidos

leg63_knight$Partido <- legData

names(leg63_knight)

### ah� est�n
table(leg63_knight$Partido)

### recodifiquemos de n�m a nombres. Usamos library car 

install.packages("car")
library(car)

leg63_knight$Partido <- recode(leg63_knight$Partido, "1='PRI';2='PAN';3='PRD';
    4='PT';5='PVEM';6='Ind';8='MC';10='NvaAl';12='Morena';13='PES'") 


### ah� est�n
table(legData)
table(leg63_knight$Partido)


### hagamos el c�rculo de radio uno

library(ggforce)
circle <- data.frame(
  x0 = 0, y0 = 0, r = 1)


### grafiquemos con los colores partidistas

install.packages("ggplot2") ## por si no se tiene

library(ggplot2)



ggplot(leg63_knight, aes(x=coord1D, y=coord2D, col=Partido))+
  geom_point(aes(col=Partido), size=2,shape=21)+
  scale_color_manual(values=c("grey","orange","red4","aquamarine","blue",
   "violet","yellow","red","green2")) +
  theme_bw()+
  labs(title="Puntos ideales, legislatura LXIII, Cámara Baja") +
  ylab("Dimensión 2")+xlab("Dimensión 1")+
  ggforce::geom_circle (data = circle,
               aes(x0=x0, y0=y0, r=r),
               color = 'light gray',
               inherit.aes=FALSE) +
  coord_fixed() 




##### buscando líneas de corte

### teniamos 305 votaciones
### se emplearon 105 en la estimaci�n 
### se desecharon 200

105 / (105 + 200)


### sabiendo que son 105 votaciones...

plot.coords(result, cutline=2) 

plot.coords(result, cutline=3)  

plot.coords(result, cutline=4)  

plot.coords(result, cutline=15)  # se ve Morena (y algo PRD) al extremo 

plot.coords(result, cutline=33)  

plot.coords(result, cutline=1)  




### una que sabemos que no fue empleada
plot.coords(result, cutline=79) # no fue empleada

### �por qu�? veamos la intuici�n

table(dos$v001)

## favor 419, contra 36, ausente=43, missing=37
## para 'lop' se toma la minor�a como mayor a 0.025 de non-missing
## al remover legisladores etc etc 
## una votaci�n de unos 7 puntos podr�a irse
## lop = lopsided votes ("leaning to one side")


### queda como trabajo en casa ver qu� votaciones
### se quieren estudiar para buscar sus cutlines



#### para profundizar

### Jason Timm tiene una buena visualizaci�n para Nuevo M�xico

https://www.jtimm.net/2018/10/10/nominate-multi-dimensional-scaling-new-mexico-s-53rd-congress/#political-ideology-in-nmsl53


########### extras

### si se quiere ver qu� contiene result

str(result)


### �ngulos

plot.angles(result, main.title="�ngulos de las l�neas de corte",
   x.title="�ngulos en grados", y.title="Conteo",dims=c(1,2))


### comparar

library(gridExtra)
library(ggpubr)

### si cambiamos la polaridad, guardemos las gr�ficas de ggplot
### sale= polaridad como Knight
### creo= polaridad seg�n yo, Morena va la izq

grid.arrange(sale, creo, ncol=2)

### End

##############################################

### senado wnominate

uno <- read.csv("senado_wnom.csv")

dim(uno)

names(uno)

table(uno$nombre) ## no lo requerimos para estimar

table(uno$sen)

table(uno$party)

uno <- uno[,-1]

### remove(uno)


names(uno)

### id representante, party, votaciones....

library(wnominate)


#### vamos a decirle a wnominate
## legislator, party, votes

sen <- uno[, 1]   ### sen en column 1
legData <- matrix(uno[, 2], length(uno[, 2]), 1) ### partidos en col 2
colnames(legData) <- "party"  ### así se llaman los partidos
uno<- uno[, -c(1, 2)]   ### sólo ya quedarán los votos...


### ahora va el rollcall


table(uno$voto_id.2989)

### codebook

# 1 es pro (favor)
# 2 es en contra
# 3 abstención
# 4 ausencia
# 5-8 no hay valores del 5 al 8
# 9 no participa


### definiciones

### 1=pro
### 2=contra
### 3,4,9= missing
 


rc <- rollcall(uno, yea = c(1), nay = c(2),
  missing = c(3,4,9), notInLegis=99, 
  legis.names = sen,
  legis.data = legData, 
  desc = "Leg 63, Senate ",
  source = "Melchor 2018")

#### polaridad


result <- wnominate(rc, polarity = c(29 , 37 ))


summary(result)

plot(result)


plot.coords(result,  cutline=627)

## quiero 2927   cutline 567

plot.coords(result,  cutline=567)




























