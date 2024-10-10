library(readxl)

uno <- read_excel("datos elecciones 2024.xlsx", sheet=4)

summary(uno)

table(uno$Registro)
table(uno$Declara_perdida)


# library(lubridate)
# uno$fecha_registro <- as_datetime(uno$Registro)
# uno$fecha_perdida  <- as_datetime(uno$Declara_perdida)

names(uno)

head(uno)

library(ggplot2)


ggplot(uno, aes(x=reorder(Partido,ordena), y=Registro, 
   group=fam, col=fam))+ 
   geom_linerange(aes(ymin=Registro, ymax=Declara_perdida),
      size = 1.5)+
   xlab("")+ coord_flip()+ ylab("") +
   theme_minimal() +theme(legend.position = "none")
    

summary(uno)


pres$validos <- pres$TOTAL_VOTOS_CALCULADOS - (pres$CANDIDATO_A_NO_REGISTRADO_A + pres$VOTOS_NULOS)


pres$PRD_por <- pres$PRD / pres$TOTAL_VOTOS_CALCULADOS





ggplot(pres_prd, aes(x=PRD_por*100,
   y=reorder(Group.1,PRD_por*100)))+ 
   geom_point()+ xlim(0,5)+
   xlab("% votos válidos")+  ylab("") +
   theme_minimal() + ggtitle("PRD 2024, elección presidencial")  


summary(pres$PRD_por)

names(pres)

pres_prd<- aggregate(pres[,c(15,36)], by=list(pres[,4]),
   FUN=sum, na.rm=TRUE)

dim(pres_prd)

head(pres_prd)

pres_prd$PRD_por <- pres_prd$PRD / pres_prd$validos

pres_prd[which(pres_prd$Group.1 == "YUCAT\xc1N"),]$Group.1 <- "YUCATAN"
pres_prd[which(pres_prd$Group.1 == "SAN LUIS POTOS\xcd"),]$Group.1 <- "SAN LUIS POTOSI"
pres_prd[which(pres_prd$Group.1 == "MICHOAC\xc1N"),]$Group.1 <- "MICHOACAN"
pres_prd[which(pres_prd$Group.1 == "CIUDAD DE M\xc9XICO"),]$Group.1 <- "CIUDAD DE MEXICO"
pres_prd[which(pres_prd$Group.1 == "M\xc9XICO"),]$Group.1 <- "MEXICO"
pres_prd[which(pres_prd$Group.1 == "QUER\xc9TARO"),]$Group.1 <- "QUERETARO"
pres_prd[which(pres_prd$Group.1 == "NUEVO LE\xd3N"),]$Group.1 <- "NUEVO LEON"









dos <- read_excel("datos elecciones 2024.xlsx", sheet=5)

summary(dos)


ggplot(dos, aes(x=validos_por*100,
   y=reorder(Entidad,validos_por*100)))+ 
   geom_point()+ xlim(0,8)+
   xlab("% votos válidos")+  ylab("") +
   theme_minimal() + ggtitle("PRD 2024, gubernaturas")  


##################

dip <- read.csv(file="DIP_FED_2024.csv", sep="|", 
      skip = 7, header = T)

dim(dip)
names(dip)

dip<-type.convert(dip, na.strings = "-", dec = ".",
             numerals = c("allow.loss"))


dip$validos <- dip$TOTAL_VOTOS_CALCULADOS - (dip$CANDIDATO.A.NO.REGISTRADO.A + dip$VOTOS.NULOS)


dip$PRD_final <- dip$PRD +  (dip$PAN_PRI_PRD/3)+
   (dip$PAN_PRD/2) + (dip$PRI_PRD/2)  


sum(dip[,"PRD_final"], na.rm = TRUE)
sum(dip[,"PRD"], na.rm = TRUE)


summary(dip)

names(dip)

dip_prd<- aggregate(dip[,c(36,38)], by=list(dip[,4]),
   FUN=sum, na.rm=TRUE)

dim(dip_prd)


dip_prd$PRD_por <- dip_prd$PRD_final / dip_prd$validos 



dip_prd[which(dip_prd$Group.1 == "YUCAT\xc1N"),]$Group.1 <- "YUCATAN"
dip_prd[which(dip_prd$Group.1 == "SAN LUIS POTOS\xcd"),]$Group.1 <- "SAN LUIS POTOSI"
dip_prd[which(dip_prd$Group.1 == "MICHOAC\xc1N"),]$Group.1 <- "MICHOACAN"
dip_prd[which(dip_prd$Group.1 == "CIUDAD DE M\xc9XICO"),]$Group.1 <- "CIUDAD DE MEXICO"
dip_prd[which(dip_prd$Group.1 == "M\xc9XICO"),]$Group.1 <- "MEXICO"
dip_prd[which(dip_prd$Group.1 == "QUER\xc9TARO"),]$Group.1 <- "QUERETARO"
dip_prd[which(dip_prd$Group.1 == "NUEVO LE\xd3N"),]$Group.1 <- "NUEVO LEON"



ggplot(dip_prd, aes(x=PRD_por*100,
   y=reorder(Group.1,PRD_por*100)))+ 
   geom_point()+ xlim(0,8)+
   xlab("% votos válidos")+  ylab("") +
   theme_minimal() + ggtitle("PRD 2024, diputaciones MR")  



va <- read_excel("datos elecciones 2024.xlsx", sheet=7)


dim(va)

names(va)

summary(va)

ggplot(va, aes(x=Year, y=por*100))+ 
   geom_bar(stat="identity", fill="grey")+ 
   scale_x_continuous(breaks = seq(1979, 2024, by=3))+
   scale_y_continuous(breaks = seq(0, 27, by=3))+
  ylab("% votos válidos")+  
  xlab("1979 PCM, 1982 y 1985 PSUM, 1988 PMS") +
  geom_hline(yintercept=3, linetype="dashed", col="grey20") + 
  ggtitle("PRD, elecciones para diputaciones MR") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
 

###############################################

