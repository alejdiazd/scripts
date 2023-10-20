
### Not run
install.packages("magick")
library(magick)
library(tesseract)

tesseract_info()


datos_nl <- tesseract::ocr("https://i.redd.it/oomzsc02l6c71.png")

cat(datos_nl)


write.csv(cat(datos_nl, file="uno.csv") )

################

img <- image_read("https://i.redd.it/oomzsc02l6c71.png")
print(img)

cat(image_ocr(img))  

image_ocr(img) 

image_ocr_data(img)


library(rvest)

url <- ("https://www.reddit.com/r/Monterrey/comments/onfd59/municipios_de_nuevo_le%C3%B3n_por_%C3%ADndice_de_desarrollo/")

page <- read_html(url) 

#Parses tables into data frames
table <- html_table(page, fill=TRUE)

table[[1]]
table[[2]]

###########


library(haven)

lapop <- read_stata("MEX_2021_LAPOP_AmericasBarometer_v1.2_w.dta")

names(lapop)


library(sjPlot)

view_df(lapop)



table(lapop$q1tb)

library(car)


lapop$mujer <- ifelse(lapop$q1tb==2,1,0)

table(lapop$genor4, lapop$mujer)


chisq.test(lapop$genor4, lapop$mujer)


plot_xtab(lapop$genor4, lapop$mujer,
  show.summary=TRUE, show.total=FALSE)


#############################
library(remotes)

remotes::install_github("arcruz0/paqueteadp")


library(paqueteadp)
data("eeuu_brasil")
library(plm)

fe <- plm(voto ~ poder_pais, data = eeuu_latam, index = c("codigo", "anio"))

summary(fe)



re <- plm(voto ~ poder_pais, data = eeuu_latam, 
          index = c("codigo", "anio"), model = "random")


summary(re)


phtest(fe, re)





mociba2019 <- read.csv("mociba2019.csv")

names(mociba2019)


data_frame <- read.csv("https://goo.gl/j6lRXD")  #Reading CSV
table(data_frame$treatment, data_frame$improvement)

 <- data.frame(ciberacoso=c("Sí","No"),
                

	      si	no
de12a19hom	28.1	71.9
de12a19muj	32.7	67.3


table(mociba2019$Acoso,mociba2019$Casos)


chisq.test(mociba2019$Acoso,mociba2019$Casos, correct=FALSE)


install.packages("datapasta")
library(datapasta)



library(readxl)

idh_nl_15<- table[[2]] 


names(idh_nl_15)

idh_nl_15$IDH <- idh_nl_15[,3]


summary(idh_nl_15[,8])



agua <- read_excel("agua_municipio_2020_NL.xls")

names(agua)
head(agua)

cor.test(agua$Pobpor, agua$Entubadafueraviviendapor)

cor.test(agua$Pobpor, agua$Entubadaviviendapor)

cor.test(agua$Pobpor, agua$AguaEntubadapor)


chisq.test(agua$Pobpor, agua$AguaEntubadapor)


library(haven)

getwd()

rosas <- read_stata("rosas_jensen_2007.dta")


dim(rosas)


names(rosas)


### replicar "OLS DF included"

# gini2000    GINI 2000
# gini1990    GINI 1990
# lgdppc      LGDPPC
# lfdipop     LFDI


m1 <- lm(gini2000~ gini1990+ lgdppc+ lfdipop,
   data=rosas)
 

summary(m1)


library(car)

vif(m1)

library(lmtest)


bptest(m1)


library(skedastic)

white_lm(m1, interactions = TRUE)


white_lm(m1)

breusch_pagan(m1, koenker=TRUE)

breusch_pagan(m1, koenker=FALSE)

cook_weisberg(m1, auxdesign = "fitted.values", hetfun = "logmult")


cook_weisberg(m1, auxdesign = "fitted.values", hetfun = "mult")

cook_weisberg(m1, auxdesign = "fitted.values", hetfun = "add")


resettest(m1)

library(sandwich)

m1_c <- coeftest(m1, vcov=vcovHC(m1, type="HC1"))



m1_c


waldtest(m1,  vcov=vcovHC(m1, type="HC1"))




### replicar "OLS excluding Mexico City"

rosas2 <- rosas[-9,]


m2 <- lm(gini2000~ gini1990+ lgdppc+ lfdipop,
   data=rosas2)

summary(m2)


bptest(m2)

white_lm(m2, interactions=TRUE)

m2_c <- coeftest(m2, vcov=vcovHC(m2, type="HC1"))


m2_c

cor.test(rosas$gini2000 , rosas$gini1990)
cor.test(rosas2$gini2000 , rosas2$gini1990)


t.test(rosas2$lfdipop ~   rosas2$border_state)



mean(rosas2$lfdipop[rosas2$border_state==1])
mean(rosas2$lfdipop[rosas2$border_state==0])


t.test(rosas$lfdipop ~   rosas$border_state)


mean(rosas$lfdipop[rosas$border_state==1])
mean(rosas$lfdipop[rosas$border_state==0])



library(sjPlot)

plot_xtab(rosas$capitaldum, rosas$border_state)

rosas2$ginien4 <- split(rosas2$gini2000, 4)


table( rosas2$ginien4)

waldtest(m2,  vcov=vcovHC(m2, type="HC1"))

library(modelsummary)


modelsummary(m2_c)


modelsummary(m2)


ver<- get_gof(m2_c)

print(ver)


library(stargazer)

library(sandwich)

cov <- vcovHC(m2, type = "HC1")
robust.se <- sqrt(diag(cov))

stargazer(m2, m2_c, type="text")



stargazer(m2,  type="text", se=list(NULL, robust.se))




library(texreg)


screenreg(list(m1, m1_c), stars=c(0.05, 0.1), 
  digits=3, include.fstatistic=TRUE)


screenreg(list(m2, m2_c), stars=c(0.05, 0.1), 
  digits=3, include.fstatistic=TRUE)

waldtest(m1)
waldtest(m1, m1_c)

####################

library(Hmisc)

rosas$gini3 <-as.numeric(cut2(rosas$gini2000, g=3))


names(rosas)

summary(rosas$gini4)
summary(rosas)

plot_xtab(rosas$gini3, rosas$border_state, 
  show.total=FALSE, show.summary=TRUE)


