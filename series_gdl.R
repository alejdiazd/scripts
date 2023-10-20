### Not Run

### series de tiempo
### vamos necesitar estas librerías

### usar install.packages("  ") si no la hemos bajado
### nos va a pedir un mirror del CRAN, le damos cloud para más rápido
### luego la llamamos con el comando library(  )

install.packages("tseries")
library(tseries)

install.packages("readxl")
library(readxl)

install.packages("ggplot2")
library(ggplot2)

install.packages("forecast")
library(forecast)

### tseries y forecast sirven para estimar
### readxl para leer archivos en excel
### ggplot2 sirve para graficar

### ahora empezamos a trabajar
### vemos donde estamos trabajando con el comando getwd()

getwd()

### si el file está en el folder que queremos, ya no hacemos nada más
### pero si no, buscamos el folder donde está nuestra base de datos

### como es un archivo en excel se usa:

elec<-read_excel("evmex_2016_2022.xls")

### recodar: elec es el nombre del objeto donde voy a poner mi base de datos

### estadística descriptiva

summary(elec)
head(elec)

### nombres de las columnas

names(elec)

### número de renglones y columnas

dim(elec)

### ahora declaro la serie
### para ello la voy a poner en el objeto va
### le digo que del objeto elec
### use la columna 3
### además le digo empieza en 2016
### y la frecuencia es mensual, entonces uso 12

va<-ts(elec[,3],start=2016,freq=12)

### luego grafico mis datos

autoplot(va)

### Es el número de vehículos eléctrico vendidos en Mex por mes 
### ahora vamos a pronosticar los siguientes doce meses 
### Usemos un promedio: asumimos que esa "tendencia" será 
### el promedio de los datos 

autoplot(meanf(va,12))

## ylim(0,8000)+theme_bw()

### se le puede agregar theme_bw() para fondo blanco 

### ahora supongamos que es estacional conforme al último periodo

autoplot(snaive(va,12))

##ylim(0,2000)+theme_bw()

### ahora comparo promedio con naive sin estaciones y con drift

autoplot(va) +
  autolayer(meanf(va, h=12),
    series="Mean", PI=FALSE) +
  autolayer(rwf(va, h=12),
    series="Naïve", PI=FALSE) +
  autolayer(rwf(va, h=12, drift=T),
    series="Drift", PI=FALSE) +
  ggtitle("Forecasts for EV") +
  xlab("Year") + ylab("EV") + theme_bw()+
  guides(colour=guide_legend(title="Forecast"))

### ahora le pongo el naive estacional

autoplot(va) +
  autolayer(meanf(va, h=12),
    series="Mean", PI=FALSE) +
  autolayer(rwf(va, h=12),
    series="Naïve", PI=FALSE) +
  autolayer(snaive(va, h=12),
    series="S Naïve", PI=FALSE) +
  autolayer(rwf(va, h=12, drift=T),
    series="Drift", PI=FALSE) +
  ggtitle("Forecasts for EV") +
  xlab("Year") + ylab("EV") + theme_bw()+
  guides(colour=guide_legend(title="Forecast"))

### Obvio ninguna convence mucho, 
### pero el punto es que ya podemos pronosticar

### ahora hagamos un arima
### ar (autoregresivo) p
### i (diferencia) d
### ma (promedio móvil) q

### entonces (p,d,q) son los parámetros del arima
### y si es estacional pues necesitamos un segundo grupo p,d,q

### dejemos que lo haga el programa solito

auto.arima(va)

### nos dice que es ARIMA(0,1,1)(1,0,0)[12]
### el AICc=1256.14 
### (entre modelos del mismo número de casos, el AICc más bajito gana)


### ahora un auto arima pero viendo que se consideren estaciones

auto.arima(va, stepwise=T, approximation=T)

### nos propone (0,1,1) y para estaciones: (0,0,1) y con drift
### el AICc=1256.71 (no es tan distinto del anterior)


### vamos a guardar nuestro primer arima en el objeto fit1

fit1<-auto.arima(va)

### y en fit2 guardamos donde pedimos explícitamente lo estacional

fit2<-auto.arima(va, stepwise=T, approximation=T)


### Ahora grafiquitas para ver pronóstico a dic 2023 

fit1 %>% forecast(h=12, level=95) %>% autoplot()

fit2 %>% forecast(h=12, level=95) %>% autoplot()

### ahora vamos a unir las grafiquitas con gridExtra

install.packages("gridExtra")
library(gridExtra)

### primero guardamos cada gráfica en un objeto, les puse uno y dos

uno<-fit1 %>% forecast(h=12, level=95) %>% autoplot()

dos<-fit2 %>% forecast(h=12, level=95) %>% autoplot()


library(gridExtra)

grid.arrange(uno, dos)

### ¿qué sabemos? 
## las ventas ya no bajarán de 3,700 
## (ver el intervalo más bajo)


### Ahora, ¿qué otras opciones nos da autoarima?
### le ponemos trace=T (rastrea lo que va calculando)

auto.arima( va, seasonal = TRUE, trace=T, stepwise=F) 

## tarda un poco...

### nos dice "ahí te van un montón de arimas"
### nos indica sus valores p,d,q
### nos dice si usa drift o no.
### nos pone el AICc
### y nos indica el mejor modelo según el menor AICc


## Ahora nos dice:

## Best model: ARIMA(0,1,3)(1,0,0)[12]

fit3 <- auto.arima(va, seasonal=TRUE, stepwise=F)

summary(fit3)

fit3 %>% forecast(h=12, level=95) %>% autoplot()

tres <- fit3 %>% forecast(h=12, level=95) %>% autoplot()

grid.arrange(uno, dos, tres)




### ¿y cómo veo la estacionalidad? Haga una grafiquita...

ggseasonplot(va, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Autos elec vendidos") +
  ggtitle("Estacionalidad en EV")

### como que hay picos en marzo, verano y dic (¡el aguinaldo para un prius!)

### otra opción: gráficas polares 

ggseasonplot(va, polar=T) +
  ylab("Autos elec vendidos") +
  ggtitle("Estacionalidad")

### medio se ven algunos picos

### Ahora subseries, ver promedios mensuales

ggsubseriesplot(va) +
  ylab("Autos elec vendidos") +
  ggtitle("Estacionalidad")

### pues como que dic, marzo y junio resaltan


### desde luego falta toda la sección de estacionariedad
### y residuales, etc etc etc...

### Pero con esto creo que ya se tiene una buena idea de series de tiempo 


### End