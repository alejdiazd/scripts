
### Not run

## strutural equations model (SEM)

install.packages("survey")
library(survey)
install.packages("lavaan")
library(lavaan)
install.packages("lavaan.survey")
library(lavaan.survey)

## diseñar el modelo

modelo <- " 

  medios_trad =~ tv + tvpaga + period + radio + socialmedia
  
  medios_dig =~ face + tw + insta + radio

  plural ~ medios_trad+ medios_dig + mujer + educacion +
           edad + izqder + urbano + bla + bla "


###  =~ indica variable latente
###  ~ indica regresión


### asumiendo que el objeto de la base de datos se llama data1
### aquí lo que se guarda en el objeto fit es la estimación SEM

fit <- lavaan(model, data = data1, auto.var = TRUE, std.lv = TRUE,
  meanstructure = TRUE, int.ov.free = TRUE, estimator = "MLM")



### ahora hay que agregar el diseño de encuesta 
### se asume que el diseño se llama svy_df

svy_df <- svydesign(data = data1, 
  weights = ~Ponderador, 
  ids = ~1 )

### nota: si hay finite population correction (fpc) se pondría fpc=~fpc 
### si hay estratos, strata=~estrato, 
### donde estrato sería la variable que lo así lo indique
### se pone ids=~ 1 porque se asume que es una muestra al azar sin clusters
### si hay clusters, estos van del más grande al más pequeño,
### se pondría fpc = ~fpc1 + fpc2 , 
### donde fpc1 es el cluster 1 (grande) y fpc2 el cluster 2 (pequeño)
### hay que preguntar si donde dice factor poblacion y factor hogar
### están relacionados o no, porque si sí, hay que poner 
### nest = TRUE 
### y poner en ids el primer cluster: ids =~ fpc1
### y ya no se pondría nada en fpc


### ahora se agrega al fit del SEM el diseño de encuesta

fit.surv <- lavaan.survey(lavaan.fit = fit, survey.design = svy_df)


### ahora se mostrará la estimación con las estadísticas de ajuste

summary(fit.surv, standardized = TRUE, fit.measures = TRUE)

### Se debe hallar CFI o TLI mayor a 0.90 y un RMSEA menor a 0.10 

### End