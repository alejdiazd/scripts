### Not run

### script para weights, rake, trim

## para abrir un archivo de spss

## https://invesoc.com/encuestasnacionales.php #sept2023

library(haven)

isa <- read_spss("GIMX202309p.sav") #ISA sept 2023

dim(isa)    # 1070 entrevistas


# para ver un poco del codebook

library(sjPlot)

view_df(isa)   # se abre en un explorador de internet

## para trabajar diseños de muestra 
 
library(survey)

## declaramos que no tiene ponderadores

data.svy.sinpesos <- svydesign(ids=~1, data=isa)

## ver mensaje
# "No weights or probabilities supplied, assuming equal probability"

# nótese la estructura

names(data.svy.sinpesos)

# las preguntas de encuesta están donde dice "variables"

names(data.svy.sinpesos$variables)

## ver descriptivos usando diseño de muestra

svytable(~sexo, data.svy.sinpesos)
svymean(~sexo, data.svy.sinpesos)

# para recodificar
library(car)

data.svy.sinpesos$variables$genero <- recode(data.svy.sinpesos$variables$sexo,
   "1=0;2=1")

svytable(~genero, data.svy.sinpesos)
svymean(~genero, data.svy.sinpesos)

# https://www.ine.mx/credencial/estadisticas-lista-nominal-padron-electoral/

gender.dist <- data.frame(genero=c(0,1),
                Freq=nrow(isa)*c(.4806,.5194))


data.svy.rake <- rake(design = data.svy.sinpesos,
              sample.margins <- list(~genero),
              population.margins <- list(gender.dist))


isa$pesos <- weights(data.svy.rake) 

names(isa)


data.svy.conpesos <- svydesign(ids=~1, weights=~pesos, data=isa)


svytable(~sexo, data.svy.sinpesos)

svytable(~sexo, data.svy.conpesos)


## usando ponderadores demográficos de ISA

isa.svy <- svydesign(ids=~1, weights=~pond, data=isa)

svytable(~sexo, isa.svy)


## usando factores expansion de ISA

isa.svy.factores <- svydesign(ids=~1, weights=~fexpm, data=isa)

svytable(~sexo, isa.svy.factores)


## comparando que pasa con los pesos...

svytable(~sexo, data.svy.sinpesos) # autoponderada

svytable(~sexo, data.svy.conpesos) # rake propio

svytable(~sexo, isa.svy)           # pond de isa

svytable(~sexo, isa.svy.factores)  # factores expansion de isa



### proporciones

prop.table(svytable(~sexo, data.svy.sinpesos)) # autoponderada

prop.table(svytable(~sexo, data.svy.conpesos)) # rake una var

prop.table(svytable(~sexo, isa.svy))           # pond de isa

prop.table(svytable(~sexo, isa.svy.factores))  # factores expansion de isa



###############################

# si se quiere hacer un rake con más de una variable...

data.svy.sinpesos$variables$edad_rec <- recode(data.svy.sinpesos$variables$edad,
   "2=1;3=2;4=3;5=4")

# 17 a 24
1689018 + 2110974 + 11261579
# 25 a 39
11047860 + 10534768 + 9649177
# 40 a 54
9051097 + 8648804 + 7917449
# 55 y más
6674196 + 5596007 + 12411614

15061571 + 31231805 + 25617350 + 24681817


# 17 a 24
15061571 / 96592543
# 25 a 39
31231805 / 96592543
# 40 a 54
25617350 / 96592543
# 55 y más
24681817 / 96592543


edad.dist  <- data.frame(edad_rec=c(1,2,3,4), 
                Freq=nrow(isa)*c(.16,.32,.26,.26))


### rake dos variables

data.svy.rake2 <- rake(design = data.svy.sinpesos,
              sample.margins <- list(~genero,~edad_rec),
              population.margins <- list(gender.dist, edad.dist))



### si los pesos se extienden se usa un trim 
# data.svy.rake.trim <- trimWeights(data.svy.rake, 
#             lower=.3, upper=3, strict=TRUE)
#hay que hacer algunos calculos para lower y upper


###

isa$pesos2 <- weights(data.svy.rake2) 


names(isa)


data.svy.conpesos2 <- svydesign(ids=~1, weights=~pesos2, data=isa)


### veamos


prop.table(svytable(~sexo, data.svy.sinpesos)) # autoponderada

prop.table(svytable(~sexo, data.svy.conpesos)) # rake una var

prop.table(svytable(~sexo, data.svy.conpesos2)) # rake dos var

prop.table(svytable(~sexo, isa.svy))           # pond de isa

prop.table(svytable(~sexo, isa.svy.factores))  # factores expansion de isa



prop.table(svytable(~edad, data.svy.sinpesos)) # autoponderada

prop.table(svytable(~edad, data.svy.conpesos)) # rake propio

prop.table(svytable(~edad, data.svy.conpesos2)) # rake dos var

prop.table(svytable(~edad, isa.svy))           # pond de isa

prop.table(svytable(~edad, isa.svy.factores))  # factores expansion de isa


## para poner los pesos con trim
# isa$peso.trim <- weights(data.svy.rake.trim) 
# data.svy.trim <- svydesign(ids=~1, weights=~peso.trim, data=isa)
# prop.table(svytable(~edad, data.svy.trim))
# prop.table(svytable(~sexo, data.svy.trim))

# para verificar

plot_frq(isa$edad, weight.by=isa$fexpm)
plot_frq(isa$sexo, weight.by=isa$fexpm)

### End