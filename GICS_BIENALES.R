################# cargando las bases de datos #############
library(foreign)
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(reshape2)
library(writexl)

########## 2010

setwd(c("C:/Users/Erick/OneDrive/GIC/GITHUB2018/GIC/ENIGH 2010/ENIGH2010"))
Deciles_por_fuente_2010<-read.dbf("Nacional por fuente por DECIL estimaciones 2010.dbf")

names(Deciles_por_fuente_2010)=c("ING COR2010", "TRABAJO2010", "SUBORDINADO2010", "NEGOCIOS2010","OTROS TRAB2010", "RENTAS2010","UTILIDAD2010", "ARRENDA2010", "TRANSFER2010","JUBILACION2010", "BECAS2010", "DONATIVOS2010", "REMESAS2010", "BENEGOBIERNO2010", "TRANS HOG2010", "TRANS INST2010", "ESTIM ALQU2010", "OTROS INGRESOS2010")

Deciles_por_fuente_2010<-Deciles_por_fuente_2010%>%
  mutate(prueba=Deciles_por_fuente_2010$TRABAJO2010+Deciles_por_fuente_2010$RENTAS2010+
           Deciles_por_fuente_2010$JUBILACION2010+Deciles_por_fuente_2010$BECAS2010+
           Deciles_por_fuente_2010$DONATIVOS2010+Deciles_por_fuente_2010$REMESAS2010+
           Deciles_por_fuente_2010$BENEGOBIERNO2010+Deciles_por_fuente_2010$`TRANS HOG2010`+
           Deciles_por_fuente_2010$`TRANS INST2010`+Deciles_por_fuente_2010$`ESTIM ALQU2010`+
           Deciles_por_fuente_2010$`OTROS INGRESOS2010`)

all.equal(Deciles_por_fuente_2010$`ING COR2010`,Deciles_por_fuente_2010$prueba)


Deciles_por_fuente_2010<-Deciles_por_fuente_2010%>%
  mutate("TRANSFERENCES2010"=JUBILACION2010+BECAS2010+DONATIVOS2010+REMESAS2010+`TRANS HOG2010`+`TRANS INST2010`,
         "OTHERS2010"=`ESTIM ALQU2010`+`OTROS INGRESOS2010`)

Deciles_por_fuente_2010<-Deciles_por_fuente_2010%>%
  mutate(prueba2=TRABAJO2010+RENTAS2010+BENEGOBIERNO2010+TRANSFERENCES2010+OTHERS2010)

all.equal(Deciles_por_fuente_2010$`ING COR2010`,Deciles_por_fuente_2010$prueba2)

#### bottom

bottom_por_fuente_2010<-read.dbf("Nacional por fuente por Bottom_40 estimaciones 2010.dbf")

names(bottom_por_fuente_2010)=c("ING COR2010", "TRABAJO2010", "SUBORDINADO2010", "NEGOCIOS2010","OTROS TRAB2010", "RENTAS2010","UTILIDAD2010", "ARRENDA2010", "TRANSFER2010","JUBILACION2010", "BECAS2010", "DONATIVOS2010", "REMESAS2010", "BENEGOBIERNO2010", "TRANS HOG2010", "TRANS INST2010", "ESTIM ALQU2010", "OTROS INGRESOS2010")

bottom_por_fuente_2010<-bottom_por_fuente_2010%>%
  mutate(prueba=bottom_por_fuente_2010$TRABAJO2010+bottom_por_fuente_2010$RENTAS2010+
           bottom_por_fuente_2010$JUBILACION2010+bottom_por_fuente_2010$BECAS2010+
           bottom_por_fuente_2010$DONATIVOS2010+bottom_por_fuente_2010$REMESAS2010+
           bottom_por_fuente_2010$BENEGOBIERNO2010+bottom_por_fuente_2010$`TRANS HOG2010`+
           bottom_por_fuente_2010$`TRANS INST2010`+bottom_por_fuente_2010$`ESTIM ALQU2010`+
           bottom_por_fuente_2010$`OTROS INGRESOS2010`)

all.equal(bottom_por_fuente_2010$`ING COR2010`,bottom_por_fuente_2010$prueba)


bottom_por_fuente_2010<-bottom_por_fuente_2010%>%
  mutate("TRANSFERENCES2010"=JUBILACION2010+BECAS2010+DONATIVOS2010+REMESAS2010+`TRANS HOG2010`+`TRANS INST2010`,
         "OTHERS2010"=`ESTIM ALQU2010`+`OTROS INGRESOS2010`)

bottom_por_fuente_2010<-bottom_por_fuente_2010%>%
  mutate(prueba2=TRABAJO2010+RENTAS2010+BENEGOBIERNO2010+TRANSFERENCES2010+OTHERS2010)

all.equal(bottom_por_fuente_2010$`ING COR2010`,bottom_por_fuente_2010$prueba2)


####consumo

consumo_2010<-read.dbf("Nacional Consumo  por DECIL 2010.dbf")

consumo_2010<-as.data.frame(consumo_2010)

names(consumo_2010)=c("Consumo_2010")




######### 2012

setwd(c("C:/Users/Erick/OneDrive/GIC/GITHUB2018/GIC/ENIGH_2012/ENIGH2012"))
Deciles_por_fuente_2012<-read.dbf("Nacional por fuente por DECIL estimaciones 2012.dbf")

names(Deciles_por_fuente_2012)=c("ING COR2012", "TRABAJO2012", "SUBORDINADO2012", "NEGOCIOS2012","OTROS TRAB2012", "RENTAS2012","UTILIDAD2012", "ARRENDA2012", "TRANSFER2012","JUBILACION2012", "BECAS2012", "DONATIVOS2012", "REMESAS2012", "BENEGOBIERNO2012", "TRANS HOG2012", "TRANS INST2012", "ESTIM ALQU2012", "OTROS INGRESOS2012")

Deciles_por_fuente_2012<-Deciles_por_fuente_2012%>%
  mutate(prueba=Deciles_por_fuente_2012$TRABAJO2012+Deciles_por_fuente_2012$RENTAS2012+
           Deciles_por_fuente_2012$JUBILACION2012+Deciles_por_fuente_2012$BECAS2012+
           Deciles_por_fuente_2012$DONATIVOS2012+Deciles_por_fuente_2012$REMESAS2012+
           Deciles_por_fuente_2012$BENEGOBIERNO2012+Deciles_por_fuente_2012$`TRANS HOG2012`+
           Deciles_por_fuente_2012$`TRANS INST2012`+Deciles_por_fuente_2012$`ESTIM ALQU2012`+
           Deciles_por_fuente_2012$`OTROS INGRESOS2012`)

all.equal(Deciles_por_fuente_2012$`ING COR2012`,Deciles_por_fuente_2012$prueba)

Deciles_por_fuente_2012<-Deciles_por_fuente_2012%>%
  mutate("TRANSFERENCES2012"=JUBILACION2012+BECAS2012+DONATIVOS2012+REMESAS2012+`TRANS HOG2012`+`TRANS INST2012`,
         "OTHERS2012"=`ESTIM ALQU2012`+`OTROS INGRESOS2012`)

Deciles_por_fuente_2012<-Deciles_por_fuente_2012%>%
  mutate(prueba2=TRABAJO2012+RENTAS2012+BENEGOBIERNO2012+TRANSFERENCES2012+OTHERS2012)

all.equal(Deciles_por_fuente_2012$`ING COR2012`,Deciles_por_fuente_2012$prueba2)

#### bottom

bottom_por_fuente_2012<-read.dbf("Nacional por fuente por Bottom_40 estimaciones 2012.dbf")

names(bottom_por_fuente_2012)=c("ING COR2012", "TRABAJO2012", "SUBORDINADO2012", "NEGOCIOS2012","OTROS TRAB2012", "RENTAS2012","UTILIDAD2012", "ARRENDA2012", "TRANSFER2012","JUBILACION2012", "BECAS2012", "DONATIVOS2012", "REMESAS2012", "BENEGOBIERNO2012", "TRANS HOG2012", "TRANS INST2012", "ESTIM ALQU2012", "OTROS INGRESOS2012")

bottom_por_fuente_2012<-bottom_por_fuente_2012%>%
  mutate(prueba=bottom_por_fuente_2012$TRABAJO2012+bottom_por_fuente_2012$RENTAS2012+
           bottom_por_fuente_2012$JUBILACION2012+bottom_por_fuente_2012$BECAS2012+
           bottom_por_fuente_2012$DONATIVOS2012+bottom_por_fuente_2012$REMESAS2012+
           bottom_por_fuente_2012$BENEGOBIERNO2012+bottom_por_fuente_2012$`TRANS HOG2012`+
           bottom_por_fuente_2012$`TRANS INST2012`+bottom_por_fuente_2012$`ESTIM ALQU2012`+
           bottom_por_fuente_2012$`OTROS INGRESOS2012`)

all.equal(bottom_por_fuente_2012$`ING COR2012`,bottom_por_fuente_2012$prueba)


bottom_por_fuente_2012<-bottom_por_fuente_2012%>%
  mutate("TRANSFERENCES2012"=JUBILACION2012+BECAS2012+DONATIVOS2012+REMESAS2012+`TRANS HOG2012`+`TRANS INST2012`,
         "OTHERS2012"=`ESTIM ALQU2012`+`OTROS INGRESOS2012`)

bottom_por_fuente_2012<-bottom_por_fuente_2012%>%
  mutate(prueba2=TRABAJO2012+RENTAS2012+BENEGOBIERNO2012+TRANSFERENCES2012+OTHERS2012)

all.equal(bottom_por_fuente_2012$`ING COR2012`,bottom_por_fuente_2012$prueba2)


### consumo

consumo_2012<-read.dbf("Nacional Consumo  por DECIL 2012.dbf")

consumo_2012<-as.data.frame(consumo_2012)

names(consumo_2012)=c("Consumo_2012")

######### 2014

setwd(c("C:/Users/Erick/OneDrive/GIC/GITHUB2018/GIC/ENIGH_2014/ENIGH_2014"))
Deciles_por_fuente_2014<-read.dbf("Nacional por fuente por DECIL estimaciones 2014.dbf")

consumo_2014<-read.dbf("Nacional Consumo  por DECIL 2014.dbf")

consumo_2014<-as.data.frame(consumo_2014)

names(consumo_2014)=c("Consumo_2014")

names(Deciles_por_fuente_2014)=c("ING COR2014", "TRABAJO2014", "SUBORDINADO2014", "NEGOCIOS2014","OTROS TRAB2014", "RENTAS2014","UTILIDAD2014", "ARRENDA2014", "TRANSFER2014","JUBILACION2014", "BECAS2014", "DONATIVOS2014", "REMESAS2014", "BENEGOBIERNO2014", "TRANS HOG2014", "TRANS INST2014", "ESTIM ALQU2014", "OTROS INGRESOS2014")

Deciles_por_fuente_2014<-Deciles_por_fuente_2014%>%
  mutate(prueba=Deciles_por_fuente_2014$TRABAJO2014+Deciles_por_fuente_2014$RENTAS2014+
           Deciles_por_fuente_2014$JUBILACION2014+Deciles_por_fuente_2014$BECAS2014+
           Deciles_por_fuente_2014$DONATIVOS2014+Deciles_por_fuente_2014$REMESAS2014+
           Deciles_por_fuente_2014$BENEGOBIERNO2014+Deciles_por_fuente_2014$`TRANS HOG2014`+
           Deciles_por_fuente_2014$`TRANS INST2014`+Deciles_por_fuente_2014$`ESTIM ALQU2014`+
           Deciles_por_fuente_2014$`OTROS INGRESOS2014`)

all.equal(Deciles_por_fuente_2014$`ING COR2014`,Deciles_por_fuente_2014$prueba)

Deciles_por_fuente_2014<-Deciles_por_fuente_2014%>%
  mutate("TRANSFERENCES2014"=JUBILACION2014+BECAS2014+DONATIVOS2014+REMESAS2014+`TRANS HOG2014`+`TRANS INST2014`,
         "OTHERS2014"=`ESTIM ALQU2014`+`OTROS INGRESOS2014`)

Deciles_por_fuente_2014<-Deciles_por_fuente_2014%>%
  mutate(prueba2=TRABAJO2014+RENTAS2014+BENEGOBIERNO2014+TRANSFERENCES2014+OTHERS2014)

all.equal(Deciles_por_fuente_2014$`ING COR2014`,Deciles_por_fuente_2014$prueba2)


######## 2016

setwd(c("C:/Users/Erick/OneDrive/GIC/GITHUB2018/GIC/ENIGH_2016/ENIGH_2016"))
Deciles_por_fuente_2016<-read.dbf("Nacional por fuente por DECIL estimaciones 2016.dbf")


consumo_2016<-read.dbf("Nacional Consumo  por DECIL 2016.dbf")

consumo_2016<-as.data.frame(consumo_2016)

names(consumo_2016)=c("Consumo_2016")

names(Deciles_por_fuente_2016)=c("ING COR2016", "TRABAJO2016", "SUBORDINADO2016", "NEGOCIOS2016","OTROS TRAB2016", "RENTAS2016","UTILIDAD2016", "ARRENDA2016", "TRANSFER2016","JUBILACION2016", "BECAS2016", "DONATIVOS2016", "REMESAS2016", "BENEGOBIERNO2016", "TRANS HOG2016", "TRANS INST2016", "ESTIM ALQU2016", "OTROS INGRESOS2016")

Deciles_por_fuente_2016<-Deciles_por_fuente_2016%>%
  mutate(prueba=Deciles_por_fuente_2016$TRABAJO2016+Deciles_por_fuente_2016$RENTAS2016+
           Deciles_por_fuente_2016$JUBILACION2016+Deciles_por_fuente_2016$BECAS2016+
           Deciles_por_fuente_2016$DONATIVOS2016+Deciles_por_fuente_2016$REMESAS2016+
           Deciles_por_fuente_2016$BENEGOBIERNO2016+Deciles_por_fuente_2016$`TRANS HOG2016`+
           Deciles_por_fuente_2016$`TRANS INST2016`+Deciles_por_fuente_2016$`ESTIM ALQU2016`+
           Deciles_por_fuente_2016$`OTROS INGRESOS2016`)

all.equal(Deciles_por_fuente_2016$`ING COR2016`,Deciles_por_fuente_2016$prueba)

Deciles_por_fuente_2016<-Deciles_por_fuente_2016%>%
  mutate("TRANSFERENCES2016"=JUBILACION2016+BECAS2016+DONATIVOS2016+REMESAS2016+`TRANS HOG2016`+`TRANS INST2016`,
         "OTHERS2016"=`ESTIM ALQU2016`+`OTROS INGRESOS2016`)

Deciles_por_fuente_2016<-Deciles_por_fuente_2016%>%
  mutate(prueba2=TRABAJO2016+RENTAS2016+BENEGOBIERNO2016+TRANSFERENCES2016+OTHERS2016)

all.equal(Deciles_por_fuente_2016$`ING COR2016`,Deciles_por_fuente_2016$prueba2)


############ 2018

setwd(c("C:/Users/Erick/OneDrive/GIC/GITHUB2018/GIC/ENIGH 2018/ENIGH2018"))
Deciles_por_fuente_2018<-read.dbf("Nacional por fuente por DECIL estimaciones 2018.dbf")

consumo_2018<-read.dbf("Nacional Consumo  por DECIL 2018.dbf")

consumo_2018<-as.data.frame(consumo_2018)

names(consumo_2018)=c("Consumo_2018")

names(Deciles_por_fuente_2018)=c("ING COR2018", "TRABAJO2018", "SUBORDINADO2018", "NEGOCIOS2018","OTROS TRAB2018", "RENTAS2018","UTILIDAD2018", "ARRENDA2018", "TRANSFER2018","JUBILACION2018", "BECAS2018", "DONATIVOS2018", "REMESAS2018", "BENEGOBIERNO2018", "TRANS HOG2018", "TRANS INST2018", "ESTIM ALQU2018", "OTROS INGRESOS2018")

Deciles_por_fuente_2018<-Deciles_por_fuente_2018%>%
  mutate(prueba=Deciles_por_fuente_2018$TRABAJO2018+Deciles_por_fuente_2018$RENTAS2018+
           Deciles_por_fuente_2018$JUBILACION2018+Deciles_por_fuente_2018$BECAS2018+
           Deciles_por_fuente_2018$DONATIVOS2018+Deciles_por_fuente_2018$REMESAS2018+
           Deciles_por_fuente_2018$BENEGOBIERNO2018+Deciles_por_fuente_2018$`TRANS HOG2018`+
           Deciles_por_fuente_2018$`TRANS INST2018`+Deciles_por_fuente_2018$`ESTIM ALQU2018`+
           Deciles_por_fuente_2018$`OTROS INGRESOS2018`)

all.equal(Deciles_por_fuente_2018$`ING COR2018`, Deciles_por_fuente_2018$prueba)

Deciles_por_fuente_2018<-Deciles_por_fuente_2018%>%
  mutate("TRANSFERENCES2018"=JUBILACION2018+BECAS2018+DONATIVOS2018+REMESAS2018+`TRANS HOG2018`+`TRANS INST2018`,
         "OTHERS2018"=`ESTIM ALQU2018`+`OTROS INGRESOS2018`)

Deciles_por_fuente_2018<-Deciles_por_fuente_2018%>%
  mutate(prueba2=TRABAJO2018+RENTAS2018+BENEGOBIERNO2018+TRANSFERENCES2018+OTHERS2018)

all.equal(Deciles_por_fuente_2018$`ING COR2018`,Deciles_por_fuente_2018$prueba2)



######### 2010-2012 por decil ##############
Tasa_total<-(((Deciles_por_fuente_2012$`ING COR2012`/Deciles_por_fuente_2010$`ING COR2010`)^(1/2))-1)*100

######################## Trabajo 

trabajo<-data.frame(trabajo2010=Deciles_por_fuente_2010$TRABAJO2010,
                    trabajo2012=Deciles_por_fuente_2012$TRABAJO2012,
                    ing_cor2010=Deciles_por_fuente_2010$`ING COR2010`,
                    ing_cor2012=Deciles_por_fuente_2012$`ING COR2012`,
                    Tasa_total)

trabajo<-trabajo%>%
  mutate(trabajo_aporte=((trabajo2012-trabajo2010)/((ing_cor2012-ing_cor2010)))*Tasa_total)

################################### Rentas 
rentas<-data.frame(rentas2010=Deciles_por_fuente_2010$RENTAS2010,
                   rentas2012=Deciles_por_fuente_2012$RENTAS2012,
                   ing_cor2010=Deciles_por_fuente_2010$`ING COR2010`,
                   ing_cor2012=Deciles_por_fuente_2012$`ING COR2012`,
                   Tasa_total)
rentas<-rentas%>%
  mutate(rentas_aporte=((rentas2012-rentas2010)/((ing_cor2012-ing_cor2010)))*Tasa_total)

################################### TRANSFERENCES 

TRANSFERENCES<-data.frame(TRANSFERENCES2010=Deciles_por_fuente_2010$TRANSFERENCES2010,
                          TRANSFERENCES2012=Deciles_por_fuente_2012$TRANSFERENCES2012,
                          ing_cor2010=Deciles_por_fuente_2010$`ING COR2010`,
                          ing_cor2012=Deciles_por_fuente_2012$`ING COR2012`,
                          Tasa_total)
TRANSFERENCES<-TRANSFERENCES%>%
  mutate(TRANSFERENCES_aporte=((TRANSFERENCES2012-TRANSFERENCES2010)/((ing_cor2012-ing_cor2010)))*Tasa_total)


################################### Benegobierno 

benegobierno<-data.frame(benegob2010=Deciles_por_fuente_2010$BENEGOBIERNO2010,
                         benegob2012=Deciles_por_fuente_2012$BENEGOBIERNO2012,
                         ing_cor2010=Deciles_por_fuente_2010$`ING COR2010`,
                         ing_cor2012=Deciles_por_fuente_2012$`ING COR2012`,
                         Tasa_total)

benegobierno<-benegobierno%>%
  mutate(benegob_aporte=((benegob2012-benegob2010)/((ing_cor2012-ing_cor2010)))*Tasa_total)

################################### OTHERS

OTHERS<-data.frame(OTHERS2010=Deciles_por_fuente_2010$OTHERS2010, 
                   OTHERS2012=Deciles_por_fuente_2012$OTHERS2012,
                   ing_cor2010=Deciles_por_fuente_2010$`ING COR2010`,
                   ing_cor2012=Deciles_por_fuente_2012$`ING COR2012`,
                   Tasa_total)

OTHERS<-OTHERS%>%
  mutate(OTHERS_aporte=((OTHERS2012-OTHERS2010)/((ing_cor2012-ing_cor2010)))*Tasa_total)


################################### Cuadro final 

cuadro_final<-data.frame(
  Labor=trabajo$trabajo_aporte,
  Capital=rentas$rentas_aporte,
  "Social programs"=benegobierno$benegob_aporte,
  "Other transfers"=TRANSFERENCES$TRANSFERENCES_aporte,
  "Imputed rent and other income"=OTHERS$OTHERS_aporte)

prueba<-cuadro_final%>%
  mutate(Prueba=Labor+Capital+Social.programs+Other.transfers+Imputed.rent.and.other.income)

all.equal(Tasa_total,prueba$Prueba)



names(cuadro_final)<-c("Labor","Capital","Social programs","Other transfers","Imputed rent 
and other income")

cuadro_final<-cuadro_final%>%
  mutate(Deciles= c("Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"))

row.names(cuadro_final)<-c("Mean","I","II","III","IV","V","VI","VII","VIII","IX","X")

cuadro_final<-cuadro_final%>%
  mutate(Deciles=fct_relevel(Deciles,"Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"))

cuadro_final<-melt(cuadro_final)

labels<-cuadro_final%>%
  mutate(labels=ifelse(value>0,value,0))%>%
  group_by(Deciles)%>%
  summarize(sum(labels)+1.5)

labels<-labels$`sum(labels) + 1.5`

max<-cuadro_final%>%
  group_by(Deciles)%>%
  filter(value>0)%>%
  summarize(sum(value))

max<-round(max(max$`sum(value)`)+2)

min<-cuadro_final%>%
  group_by(Deciles)%>%
  filter(value<0)%>%
  summarize(sum(value))

min<-round(min(min$`sum(value)`)-2)

GIC<-cuadro_final%>%
  mutate(Deciles=fct_relevel(Deciles,"Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"))%>%
  ggplot(aes(x=Deciles, y=value , fill= variable),position= "dodge")+
  geom_col()+
  labs(title = "Figure 3
Mexico
Growth Incidence Curve by income source
2010-2012",
       y="Growth rate (total)",
       x="Decile",
       fill="Source of
  income")+
  geom_hline(yintercept = 0)+
  scale_y_continuous(breaks = seq(min,max,1),labels = rep(c(""),abs(min-max)+1))+
  annotate("text", x= "Mean", y= labels[1], label=round(Tasa_total[1],2))+
  annotate("text", x= "I", y= labels[2], label=round(Tasa_total[2],2))+
  annotate("text", x= "II", y= labels[3], label=round(Tasa_total[3],2))+
  annotate("text", x= "III", y= labels[4], label=round(Tasa_total[4],2))+
  annotate("text", x= "IV", y= labels[5], label=round(Tasa_total[5],2))+
  annotate("text", x= "V", y= labels[6], label=round(Tasa_total[6],2))+
  annotate("text", x= "VI", y= labels[7], label=round(Tasa_total[7],2))+
  annotate("text", x= "VII", y= labels[8], label=round(Tasa_total[8],2))+
  annotate("text", x= "VIII", y= labels[9], label=round(Tasa_total[9],2))+
  annotate("text", x= "IX", y= labels[10], label=round(Tasa_total[10],2))+
  annotate("text", x= "X", y= labels[11], label=round(Tasa_total[11],2))+
  theme_minimal()

GIC

######## Consumo 


GICconsumo<-data.frame(consumo_2010,consumo_2012)



GICconsumo<-GICconsumo%>%
  mutate(Rate=((Consumo_2012-Consumo_2010)/Consumo_2010)*100,
         Deciles=c("Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"),
         orden=1:11)


max<-round((max(GICconsumo$Rate)+1.5),0)
min<-round((min(GICconsumo$Rate)+1.5),0)

labels<-GICconsumo$Rate

GIC_cons<-GICconsumo%>%
  mutate(Deciles=fct_relevel(Deciles,"Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"))%>%
  ggplot(aes(Deciles,Rate))+
  geom_col()+
  labs(title ="FIgure 4
Mexico
Growth Incidence Curve by consumption
2010-2012",
       y="Growth rate (total)",
       x="Decile")+
  scale_y_continuous(breaks=seq(-4,max,1))+
  annotate("text", x= "Mean", y= labels[1]+2, label=round(labels[1],2))+
  annotate("text", x= "I", y= labels[2]+2, label=round(labels[2],2))+
  annotate("text", x= "II", y= labels[3]+2, label=round(labels[3],2))+
  annotate("text", x= "III", y= labels[4]-2, label=round(labels[4],2))+
  annotate("text", x= "IV", y= labels[5]-2, label=round(labels[5],2))+
  annotate("text", x= "V", y= labels[6]+2, label=round(labels[6],2))+
  annotate("text", x= "VI", y= labels[7]-2, label=round(labels[7],2))+
  annotate("text", x= "VII", y= labels[8]+2, label=round(labels[8],2))+
  annotate("text", x= "VIII", y= labels[9]+2, label=round(labels[9],2))+
  annotate("text", x= "IX", y= labels[10]+2, label=round(labels[10],2))+
  annotate("text", x= "X", y= labels[11]+2, label=round(labels[11],2))+
  theme_minimal()


GIC_cons 

######### 2010-2012  por estados media ##############

#### 2010

setwd(c("C:/Users/Erick/OneDrive/GIC/GITHUB2018/GIC/ENIGH 2010/ENIGH2010"))
mean_estados_por_fuente_2010<-read.dbf("ESTADOS MEAN por fuente por estimaciones 2010.dbf")

names(mean_estados_por_fuente_2010)=c("ING COR2010", "TRABAJO2010", "SUBORDINADO2010", "NEGOCIOS2010","OTROS TRAB2010", "RENTAS2010","UTILIDAD2010", "ARRENDA2010", "TRANSFER2010","JUBILACION2010", "BECAS2010", "DONATIVOS2010", "REMESAS2010", "BENEGOBIERNO2010", "TRANS HOG2010", "TRANS INST2010", "ESTIM ALQU2010", "OTROS INGRESOS2010")

mean_estados_por_fuente_2010<-mean_estados_por_fuente_2010%>%
  mutate(prueba=mean_estados_por_fuente_2010$TRABAJO2010+mean_estados_por_fuente_2010$RENTAS2010+
           mean_estados_por_fuente_2010$JUBILACION2010+mean_estados_por_fuente_2010$BECAS2010+
           mean_estados_por_fuente_2010$DONATIVOS2010+mean_estados_por_fuente_2010$REMESAS2010+
           mean_estados_por_fuente_2010$BENEGOBIERNO2010+mean_estados_por_fuente_2010$`TRANS HOG2010`+
           mean_estados_por_fuente_2010$`TRANS INST2010`+mean_estados_por_fuente_2010$`ESTIM ALQU2010`+
           mean_estados_por_fuente_2010$`OTROS INGRESOS2010`)

all.equal(mean_estados_por_fuente_2010$`ING COR2010`,mean_estados_por_fuente_2010$prueba)


mean_estados_por_fuente_2010<-mean_estados_por_fuente_2010%>%
  mutate("TRANSFERENCES2010"=JUBILACION2010+BECAS2010+DONATIVOS2010+REMESAS2010+`TRANS HOG2010`+`TRANS INST2010`,
         "OTHERS2010"=`ESTIM ALQU2010`+`OTROS INGRESOS2010`)

mean_estados_por_fuente_2010<-mean_estados_por_fuente_2010%>%
  mutate(prueba2=TRABAJO2010+RENTAS2010+BENEGOBIERNO2010+TRANSFERENCES2010+OTHERS2010)

all.equal(mean_estados_por_fuente_2010$`ING COR2010`,mean_estados_por_fuente_2010$prueba2)


mean_consumo_por_estado_2010<-read.dbf("ESTADOS MEAN Consumo 2010.dbf")

#### 2012


setwd(c("C:/Users/Erick/OneDrive/GIC/GITHUB2018/GIC/ENIGH_2012/ENIGH2012"))
mean_estados_por_fuente_2012<-read.dbf("ESTADOS MEAN por fuente por estimaciones 2012.dbf")

names(mean_estados_por_fuente_2012)=c("ING COR2012", "TRABAJO2012", "SUBORDINADO2012", "NEGOCIOS2012","OTROS TRAB2012", "RENTAS2012","UTILIDAD2012", "ARRENDA2012", "TRANSFER2012","JUBILACION2012", "BECAS2012", "DONATIVOS2012", "REMESAS2012", "BENEGOBIERNO2012", "TRANS HOG2012", "TRANS INST2012", "ESTIM ALQU2012", "OTROS INGRESOS2012")

mean_estados_por_fuente_2012<-mean_estados_por_fuente_2012%>%
  mutate(prueba=mean_estados_por_fuente_2012$TRABAJO2012+mean_estados_por_fuente_2012$RENTAS2012+
           mean_estados_por_fuente_2012$JUBILACION2012+mean_estados_por_fuente_2012$BECAS2012+
           mean_estados_por_fuente_2012$DONATIVOS2012+mean_estados_por_fuente_2012$REMESAS2012+
           mean_estados_por_fuente_2012$BENEGOBIERNO2012+mean_estados_por_fuente_2012$`TRANS HOG2012`+
           mean_estados_por_fuente_2012$`TRANS INST2012`+mean_estados_por_fuente_2012$`ESTIM ALQU2012`+
           mean_estados_por_fuente_2012$`OTROS INGRESOS2012`)

all.equal(mean_estados_por_fuente_2012$`ING COR2012`,mean_estados_por_fuente_2012$prueba)

mean_estados_por_fuente_2012<-mean_estados_por_fuente_2012%>%
  mutate("TRANSFERENCES2012"=JUBILACION2012+BECAS2012+DONATIVOS2012+REMESAS2012+`TRANS HOG2012`+`TRANS INST2012`,
         "OTHERS2012"=`ESTIM ALQU2012`+`OTROS INGRESOS2012`)

mean_estados_por_fuente_2012<-mean_estados_por_fuente_2012%>%
  mutate(prueba2=TRABAJO2012+RENTAS2012+BENEGOBIERNO2012+TRANSFERENCES2012+OTHERS2012)

all.equal(mean_estados_por_fuente_2012$`ING COR2012`,mean_estados_por_fuente_2012$prueba2)


mean_consumo_por_estado_2012<-read.dbf("ESTADOS MEAN Consumo 2012.dbf")

##### cuadros


cuadro_2010_2012<-data.frame("Total"=(((mean_estados_por_fuente_2012$`ING COR2012`/mean_estados_por_fuente_2010$`ING COR2010`)^(1/2))-1)*100,
                             "Labor"=(((mean_estados_por_fuente_2012$TRABAJO2012/mean_estados_por_fuente_2010$TRABAJO2010)^(1/2))-1)*100,
                             "Capital"=(((mean_estados_por_fuente_2012$RENTAS2012/mean_estados_por_fuente_2010$RENTAS2010)^(1/2))-1)*100,
                             "Social programs"=(((mean_estados_por_fuente_2012$BENEGOBIERNO2012/mean_estados_por_fuente_2010$BENEGOBIERNO2010)^(1/2))-1)*100,
                             "Other transfers"=(((mean_estados_por_fuente_2012$TRANSFERENCES2012/mean_estados_por_fuente_2010$TRANSFERENCES2010)^(1/2))-1)*100,
                             "Imputed rent and other income"=(((mean_estados_por_fuente_2012$OTHERS2012/mean_estados_por_fuente_2010$OTHERS2010)^(1/2))-1)*100)

cuadro_consumo_2010_2012<-data.frame("consumption"=(((mean_consumo_por_estado_2012$c_Consumo_/mean_consumo_por_estado_2010)^(1/2))-1)*100)

cuadro_2010_2012<-cbind(cuadro_2010_2012,cuadro_consumo_2010_2012)

names(cuadro_2010_2012)<-c("Total","Labor","Capital","Social programs","Other transfers","Imputed rent and other income","Consumption")

setwd(c("C:/Users/Erick/OneDrive/GIC/GITHUB2018/GIC/GICS_BIENALES/GICS_BIENALES/"))

write.dbf(cuadro_2010_2012,c("cuadro20102012.dbf"))

write_xlsx(cuadro_2010_2012,"cuadro20102012.xlsx")

rm(list=ls())


######### 2010-2012  por estados bottom ##############

#### 2010

setwd(c("C:/Users/Erick/OneDrive/GIC/GITHUB2018/GIC/ENIGH 2010/ENIGH2010"))
bottom_estados_por_fuente_2010<-read.dbf("ESTADOS bottom 40 por fuente por ENTIDAD estimaciones 2010.dbf")

names(bottom_estados_por_fuente_2010)=c("ING COR2010", "TRABAJO2010", "SUBORDINADO2010", "NEGOCIOS2010","OTROS TRAB2010", "RENTAS2010","UTILIDAD2010", "ARRENDA2010", "TRANSFER2010","JUBILACION2010", "BECAS2010", "DONATIVOS2010", "REMESAS2010", "BENEGOBIERNO2010", "TRANS HOG2010", "TRANS INST2010", "ESTIM ALQU2010", "OTROS INGRESOS2010")

bottom_estados_por_fuente_2010<-bottom_estados_por_fuente_2010%>%
  mutate(prueba=bottom_estados_por_fuente_2010$TRABAJO2010+bottom_estados_por_fuente_2010$RENTAS2010+
           bottom_estados_por_fuente_2010$JUBILACION2010+bottom_estados_por_fuente_2010$BECAS2010+
           bottom_estados_por_fuente_2010$DONATIVOS2010+bottom_estados_por_fuente_2010$REMESAS2010+
           bottom_estados_por_fuente_2010$BENEGOBIERNO2010+bottom_estados_por_fuente_2010$`TRANS HOG2010`+
           bottom_estados_por_fuente_2010$`TRANS INST2010`+bottom_estados_por_fuente_2010$`ESTIM ALQU2010`+
           bottom_estados_por_fuente_2010$`OTROS INGRESOS2010`)

all.equal(bottom_estados_por_fuente_2010$`ING COR2010`,bottom_estados_por_fuente_2010$prueba)


bottom_estados_por_fuente_2010<-bottom_estados_por_fuente_2010%>%
  mutate("TRANSFERENCES2010"=JUBILACION2010+BECAS2010+DONATIVOS2010+REMESAS2010+`TRANS HOG2010`+`TRANS INST2010`,
         "OTHERS2010"=`ESTIM ALQU2010`+`OTROS INGRESOS2010`)

bottom_estados_por_fuente_2010<-bottom_estados_por_fuente_2010%>%
  mutate(prueba2=TRABAJO2010+RENTAS2010+BENEGOBIERNO2010+TRANSFERENCES2010+OTHERS2010)

all.equal(bottom_estados_por_fuente_2010$`ING COR2010`,bottom_estados_por_fuente_2010$prueba2)


bottom_consumo_por_estado_2010<-read.dbf("ESTADOS Consumo bottom 40 2010.dbf")

#### 2012


setwd(c("C:/Users/Erick/OneDrive/GIC/GITHUB2018/GIC/ENIGH_2012/ENIGH2012"))
bottom_estados_por_fuente_2012<-read.dbf("ESTADOS bottom 40 por fuente por ENTIDAD estimaciones 2012.dbf")

names(bottom_estados_por_fuente_2012)=c("ING COR2012", "TRABAJO2012", "SUBORDINADO2012", "NEGOCIOS2012","OTROS TRAB2012", "RENTAS2012","UTILIDAD2012", "ARRENDA2012", "TRANSFER2012","JUBILACION2012", "BECAS2012", "DONATIVOS2012", "REMESAS2012", "BENEGOBIERNO2012", "TRANS HOG2012", "TRANS INST2012", "ESTIM ALQU2012", "OTROS INGRESOS2012")

bottom_estados_por_fuente_2012<-bottom_estados_por_fuente_2012%>%
  mutate(prueba=bottom_estados_por_fuente_2012$TRABAJO2012+bottom_estados_por_fuente_2012$RENTAS2012+
           bottom_estados_por_fuente_2012$JUBILACION2012+bottom_estados_por_fuente_2012$BECAS2012+
           bottom_estados_por_fuente_2012$DONATIVOS2012+bottom_estados_por_fuente_2012$REMESAS2012+
           bottom_estados_por_fuente_2012$BENEGOBIERNO2012+bottom_estados_por_fuente_2012$`TRANS HOG2012`+
           bottom_estados_por_fuente_2012$`TRANS INST2012`+bottom_estados_por_fuente_2012$`ESTIM ALQU2012`+
           bottom_estados_por_fuente_2012$`OTROS INGRESOS2012`)

all.equal(bottom_estados_por_fuente_2012$`ING COR2012`,bottom_estados_por_fuente_2012$prueba)

bottom_estados_por_fuente_2012<-bottom_estados_por_fuente_2012%>%
  mutate("TRANSFERENCES2012"=JUBILACION2012+BECAS2012+DONATIVOS2012+REMESAS2012+`TRANS HOG2012`+`TRANS INST2012`,
         "OTHERS2012"=`ESTIM ALQU2012`+`OTROS INGRESOS2012`)

bottom_estados_por_fuente_2012<-bottom_estados_por_fuente_2012%>%
  mutate(prueba2=TRABAJO2012+RENTAS2012+BENEGOBIERNO2012+TRANSFERENCES2012+OTHERS2012)

all.equal(bottom_estados_por_fuente_2012$`ING COR2012`,bottom_estados_por_fuente_2012$prueba2)


bottom_consumo_por_estado_2012<-read.dbf("ESTADOS Consumo bottom 40 2012.dbf")

##### cuadros


cuadro_2010_2012<-data.frame("Total"=(((bottom_estados_por_fuente_2012$`ING COR2012`/bottom_estados_por_fuente_2010$`ING COR2010`)^(1/2))-1)*100,
                             "Labor"=(((bottom_estados_por_fuente_2012$TRABAJO2012/bottom_estados_por_fuente_2010$TRABAJO2010)^(1/2))-1)*100,
                             "Capital"=(((bottom_estados_por_fuente_2012$RENTAS2012/bottom_estados_por_fuente_2010$RENTAS2010)^(1/2))-1)*100,
                             "Social programs"=(((bottom_estados_por_fuente_2012$BENEGOBIERNO2012/bottom_estados_por_fuente_2010$BENEGOBIERNO2010)^(1/2))-1)*100,
                             "Other transfers"=(((bottom_estados_por_fuente_2012$TRANSFERENCES2012/bottom_estados_por_fuente_2010$TRANSFERENCES2010)^(1/2))-1)*100,
                             "Imputed rent and other income"=(((bottom_estados_por_fuente_2012$OTHERS2012/bottom_estados_por_fuente_2010$OTHERS2010)^(1/2))-1)*100)

cuadro_consumo_2010_2012<-data.frame("consumption"=(((bottom_consumo_por_estado_2012$c_Consumo_/bottom_consumo_por_estado_2010)^(1/2))-1)*100)

cuadro_2010_2012<-cbind(cuadro_2010_2012,cuadro_consumo_2010_2012)

names(cuadro_2010_2012)<-c("Total","Labor","Capital","Social programs","Other transfers","Imputed rent and other income","Consumption")

setwd(c("C:/Users/Erick/OneDrive/GIC/GITHUB2018/GIC/GICS_BIENALES/GICS_BIENALES/"))

write.dbf(cuadro_2010_2012,c("cuadro20102012 bottom.dbf"))

write_xlsx(cuadro_2010_2012,"cuadro20102012 bottom.xlsx")

rm(list=ls())

######### 2010-2012  por estados upper ##############

#### 2010

setwd(c("C:/Users/Erick/OneDrive/GIC/GITHUB2018/GIC/ENIGH 2010/ENIGH2010"))
upper_estados_por_fuente_2010<-read.dbf("ESTADOS upper 60 por fuente por ENTIDAD estimaciones 2010.dbf")

names(upper_estados_por_fuente_2010)=c("ING COR2010", "TRABAJO2010", "SUBORDINADO2010", "NEGOCIOS2010","OTROS TRAB2010", "RENTAS2010","UTILIDAD2010", "ARRENDA2010", "TRANSFER2010","JUBILACION2010", "BECAS2010", "DONATIVOS2010", "REMESAS2010", "BENEGOBIERNO2010", "TRANS HOG2010", "TRANS INST2010", "ESTIM ALQU2010", "OTROS INGRESOS2010")

upper_estados_por_fuente_2010<-upper_estados_por_fuente_2010%>%
  mutate(prueba=upper_estados_por_fuente_2010$TRABAJO2010+upper_estados_por_fuente_2010$RENTAS2010+
           upper_estados_por_fuente_2010$JUBILACION2010+upper_estados_por_fuente_2010$BECAS2010+
           upper_estados_por_fuente_2010$DONATIVOS2010+upper_estados_por_fuente_2010$REMESAS2010+
           upper_estados_por_fuente_2010$BENEGOBIERNO2010+upper_estados_por_fuente_2010$`TRANS HOG2010`+
           upper_estados_por_fuente_2010$`TRANS INST2010`+upper_estados_por_fuente_2010$`ESTIM ALQU2010`+
           upper_estados_por_fuente_2010$`OTROS INGRESOS2010`)

all.equal(upper_estados_por_fuente_2010$`ING COR2010`,upper_estados_por_fuente_2010$prueba)


upper_estados_por_fuente_2010<-upper_estados_por_fuente_2010%>%
  mutate("TRANSFERENCES2010"=JUBILACION2010+BECAS2010+DONATIVOS2010+REMESAS2010+`TRANS HOG2010`+`TRANS INST2010`,
         "OTHERS2010"=`ESTIM ALQU2010`+`OTROS INGRESOS2010`)

upper_estados_por_fuente_2010<-upper_estados_por_fuente_2010%>%
  mutate(prueba2=TRABAJO2010+RENTAS2010+BENEGOBIERNO2010+TRANSFERENCES2010+OTHERS2010)

all.equal(upper_estados_por_fuente_2010$`ING COR2010`,upper_estados_por_fuente_2010$prueba2)


upper_consumo_por_estado_2010<-read.dbf("ESTADOS Consumo upper 60 2010.dbf")

#### 2012


setwd(c("C:/Users/Erick/OneDrive/GIC/GITHUB2018/GIC/ENIGH_2012/ENIGH2012"))
upper_estados_por_fuente_2012<-read.dbf("ESTADOS upper 60 por fuente por ENTIDAD estimaciones 2012.dbf")

names(upper_estados_por_fuente_2012)=c("ING COR2012", "TRABAJO2012", "SUBORDINADO2012", "NEGOCIOS2012","OTROS TRAB2012", "RENTAS2012","UTILIDAD2012", "ARRENDA2012", "TRANSFER2012","JUBILACION2012", "BECAS2012", "DONATIVOS2012", "REMESAS2012", "BENEGOBIERNO2012", "TRANS HOG2012", "TRANS INST2012", "ESTIM ALQU2012", "OTROS INGRESOS2012")

upper_estados_por_fuente_2012<-upper_estados_por_fuente_2012%>%
  mutate(prueba=upper_estados_por_fuente_2012$TRABAJO2012+upper_estados_por_fuente_2012$RENTAS2012+
           upper_estados_por_fuente_2012$JUBILACION2012+upper_estados_por_fuente_2012$BECAS2012+
           upper_estados_por_fuente_2012$DONATIVOS2012+upper_estados_por_fuente_2012$REMESAS2012+
           upper_estados_por_fuente_2012$BENEGOBIERNO2012+upper_estados_por_fuente_2012$`TRANS HOG2012`+
           upper_estados_por_fuente_2012$`TRANS INST2012`+upper_estados_por_fuente_2012$`ESTIM ALQU2012`+
           upper_estados_por_fuente_2012$`OTROS INGRESOS2012`)

all.equal(upper_estados_por_fuente_2012$`ING COR2012`,upper_estados_por_fuente_2012$prueba)

upper_estados_por_fuente_2012<-upper_estados_por_fuente_2012%>%
  mutate("TRANSFERENCES2012"=JUBILACION2012+BECAS2012+DONATIVOS2012+REMESAS2012+`TRANS HOG2012`+`TRANS INST2012`,
         "OTHERS2012"=`ESTIM ALQU2012`+`OTROS INGRESOS2012`)

upper_estados_por_fuente_2012<-upper_estados_por_fuente_2012%>%
  mutate(prueba2=TRABAJO2012+RENTAS2012+BENEGOBIERNO2012+TRANSFERENCES2012+OTHERS2012)

all.equal(upper_estados_por_fuente_2012$`ING COR2012`,upper_estados_por_fuente_2012$prueba2)


upper_consumo_por_estado_2012<-read.dbf("ESTADOS Consumo upper 60 2012.dbf")

##### cuadros


cuadro_2010_2012<-data.frame("Total"=(((upper_estados_por_fuente_2012$`ING COR2012`/upper_estados_por_fuente_2010$`ING COR2010`)^(1/2))-1)*100,
                             "Labor"=(((upper_estados_por_fuente_2012$TRABAJO2012/upper_estados_por_fuente_2010$TRABAJO2010)^(1/2))-1)*100,
                             "Capital"=(((upper_estados_por_fuente_2012$RENTAS2012/upper_estados_por_fuente_2010$RENTAS2010)^(1/2))-1)*100,
                             "Social programs"=(((upper_estados_por_fuente_2012$BENEGOBIERNO2012/upper_estados_por_fuente_2010$BENEGOBIERNO2010)^(1/2))-1)*100,
                             "Other transfers"=(((upper_estados_por_fuente_2012$TRANSFERENCES2012/upper_estados_por_fuente_2010$TRANSFERENCES2010)^(1/2))-1)*100,
                             "Imputed rent and other income"=(((upper_estados_por_fuente_2012$OTHERS2012/upper_estados_por_fuente_2010$OTHERS2010)^(1/2))-1)*100)

cuadro_consumo_2010_2012<-data.frame("consumption"=(((upper_consumo_por_estado_2012$c_Consumo_/upper_consumo_por_estado_2010)^(1/2))-1)*100)

cuadro_2010_2012<-cbind(cuadro_2010_2012,cuadro_consumo_2010_2012)

names(cuadro_2010_2012)<-c("Total","Labor","Capital","Social programs","Other transfers","Imputed rent and other income","Consumption")

setwd(c("C:/Users/Erick/OneDrive/GIC/GITHUB2018/GIC/GICS_BIENALES/GICS_BIENALES/"))

write.dbf(cuadro_2010_2012,c("cuadro20102012 upper.dbf"))

write_xlsx(cuadro_2010_2012,"cuadro20102012 upper.xlsx")

rm(list=ls())




######### mapas 2010-2012 ##########
library(gpclib)
library(sp)
library(ggplot2)
library(plyr)
library(dplyr)
library(scales)
library(foreign)
library(maptools)
library(rgdal)
library(ggmap)
library(gridExtra)
library(rgeos)
library(mapdata)
library(Hmisc)
library(grid)
library(mapproj)

gpclibPermit()

setwd(c("C:/Users/Erick/OneDrive/GIC/GITHUB2018/GIC/GICS_BIENALES/GICS_BIENALES/"))

mapa<-readOGR("mexstates.shp")

mapa_frame<-fortify(mapa,region ="FIPS_ADMIN")

####### ingreso bottom 40

data<-read.csv("datos para mapas 2010-2012.csv")

datos_para_mapa<-inner_join(mapa_frame,data, by = "id")

Bottom_income<-ggplot(data =  datos_para_mapa, aes(long, lat, group=group,fill=`Bottom.40.income`)) + 
  geom_polygon(colour="grey") +
  coord_map(projection="mercator") +
  labs(title = "Map 1
Annual growth rate of total income per capita 
for the bottom 40% households of the distribution
2010 - 2012",fill="",caption= "Own elaboration based on NISG, 2021")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_gradient2(low = "#A80000",mid = "white", high = "#176700", midpoint = 0)+
  theme_nothing(legend = TRUE)
Bottom_income  

ggsave(Bottom_income,width = 15, height = 10, units = c("cm"), device = "png",filename = "bottom_income_2010_2012.png")

PPI_income<-ggplot(data =  datos_para_mapa, aes(long, lat, group=group,fill=`PPGI.income`)) + 
  geom_polygon(colour="grey") +
  coord_map(projection="mercator") +
  labs(title = "Map 2
Pro-Poor Index for income by state
2010 - 2012",fill="",caption= "Own elaboration based on NISG, 2021")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_gradient2(low = "#A80000",mid = "white", high = "#176700", midpoint = 0)+
  theme_nothing(legend = TRUE)
PPI_income  

ggsave(PPI_income,width = 15, height = 10, units = c("cm"), device = "png",filename = "PPI_income_2010_2012.png")

Bottom_consumption<-ggplot(data =  datos_para_mapa, aes(long, lat, group=group,fill=`Bottom.40.consumption`)) + 
  geom_polygon(colour="grey") +
  coord_map(projection="mercator") +
  labs(title = "Map 3
Annual growth rate of consumption per capita 
for the bottom 40% households of the distribution
2010 - 2012",fill="",caption= "Own elaboration based on NISG, 2021")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_gradient2(low = "#A80000",mid = "white", high = "#176700", midpoint = 0)+
  theme_nothing(legend = TRUE)
Bottom_consumption  

ggsave(Bottom_consumption,width = 15, height = 10, units = c("cm"), device = "png",filename = "bottom_consumption_2010_2012.png")

PPI_consumption<-ggplot(data =  datos_para_mapa, aes(long, lat, group=group,fill=`PPGI.consumption`)) + 
  geom_polygon(colour="grey") +
  coord_map(projection="mercator") +
  labs(title = "Map 4
Pro-Poor Index for consumption by state
2010 - 2012",fill="",caption= "Own elaboration based on NISG, 2021")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_gradient2(low = "#A80000",mid = "white", high = "#176700", midpoint = 0)+
  theme_nothing(legend = TRUE)
PPI_consumption 

ggsave(PPI_consumption,width = 15, height = 10, units = c("cm"), device = "png",filename = "PPI_consumption_2010_2012.png")

rm(list = ls())


######### 2012-2014 ##############
Tasa_total<-((Deciles_por_fuente_2014$`ING COR2014`- Deciles_por_fuente_2012$`ING COR2012`)/Deciles_por_fuente_2012$`ING COR2012`)*100

######################## Trabajo 

trabajo<-data.frame(trabajo2012=Deciles_por_fuente_2012$TRABAJO2012,
                    trabajo2014=Deciles_por_fuente_2014$TRABAJO2014,
                    ing_cor2012=Deciles_por_fuente_2012$`ING COR2012`,
                    ing_cor2014=Deciles_por_fuente_2014$`ING COR2014`,
                    Tasa_total)

trabajo<-trabajo%>%
  mutate(trabajo_aporte=((trabajo2014-trabajo2012)/((ing_cor2014-ing_cor2012)))*Tasa_total)

################################### Rentas 
rentas<-data.frame(rentas2012=Deciles_por_fuente_2012$RENTAS2012,
                   rentas2014=Deciles_por_fuente_2014$RENTAS2014,
                   ing_cor2012=Deciles_por_fuente_2012$`ING COR2012`,
                   ing_cor2014=Deciles_por_fuente_2014$`ING COR2014`,
                   Tasa_total)
rentas<-rentas%>%
  mutate(rentas_aporte=((rentas2014-rentas2012)/((ing_cor2014-ing_cor2012)))*Tasa_total)

################################### TRANSFERENCES 

TRANSFERENCES<-data.frame(TRANSFERENCES2012=Deciles_por_fuente_2012$TRANSFERENCES2012,
                          TRANSFERENCES2014=Deciles_por_fuente_2014$TRANSFERENCES2014,
                          ing_cor2012=Deciles_por_fuente_2012$`ING COR2012`,
                          ing_cor2014=Deciles_por_fuente_2014$`ING COR2014`,
                          Tasa_total)
TRANSFERENCES<-TRANSFERENCES%>%
  mutate(TRANSFERENCES_aporte=((TRANSFERENCES2014-TRANSFERENCES2012)/((ing_cor2014-ing_cor2012)))*Tasa_total)


################################### Benegobierno 

benegobierno<-data.frame(benegob2012=Deciles_por_fuente_2012$BENEGOBIERNO2012,
                         benegob2014=Deciles_por_fuente_2014$BENEGOBIERNO2014,
                         ing_cor2012=Deciles_por_fuente_2012$`ING COR2012`,
                         ing_cor2014=Deciles_por_fuente_2014$`ING COR2014`,
                         Tasa_total)

benegobierno<-benegobierno%>%
  mutate(benegob_aporte=((benegob2014-benegob2012)/((ing_cor2014-ing_cor2012)))*Tasa_total)

################################### OTHERS

OTHERS<-data.frame(OTHERS2012=Deciles_por_fuente_2012$OTHERS2012, 
                   OTHERS2014=Deciles_por_fuente_2014$OTHERS2014,
                   ing_cor2012=Deciles_por_fuente_2012$`ING COR2012`,
                   ing_cor2014=Deciles_por_fuente_2014$`ING COR2014`,
                   Tasa_total)

OTHERS<-OTHERS%>%
  mutate(OTHERS_aporte=((OTHERS2014-OTHERS2012)/((ing_cor2014-ing_cor2012)))*Tasa_total)


################################### Cuadro final 

cuadro_final<-data.frame(
  Labor=trabajo$trabajo_aporte,
  Capital=rentas$rentas_aporte,
  "Social programs"=benegobierno$benegob_aporte,
  "Other transfers"=TRANSFERENCES$TRANSFERENCES_aporte,
  "Imputed rent and other income"=OTHERS$OTHERS_aporte)

prueba<-cuadro_final%>%
  mutate(Prueba=Labor+Capital+Social.programs+Other.transfers+Imputed.rent.and.other.income)

all.equal(Tasa_total,prueba$Prueba)



names(cuadro_final)<-c("Labor","Capital","Social programs","Other transfers","Imputed rent 
and other income")

cuadro_final<-cuadro_final%>%
  mutate(Deciles= c("Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"))

row.names(cuadro_final)<-c("Mean","I","II","III","IV","V","VI","VII","VIII","IX","X")

cuadro_final<-cuadro_final%>%
  mutate(Deciles=fct_relevel(Deciles,"Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"))

cuadro_final<-melt(cuadro_final)

labels<-cuadro_final%>%
  mutate(labels=ifelse(value>0,value,0))%>%
  group_by(Deciles)%>%
  summarize(sum(labels)+1.5)

labels<-labels$`sum(labels) + 1.5`

max<-cuadro_final%>%
  group_by(Deciles)%>%
  filter(value>0)%>%
  summarize(sum(value))

max<-round(max(max$`sum(value)`)+2)

min<-cuadro_final%>%
  group_by(Deciles)%>%
  filter(value<0)%>%
  summarize(sum(value))

min<-round(min(min$`sum(value)`)-2)

GIC<-cuadro_final%>%
  mutate(Deciles=fct_relevel(Deciles,"Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"))%>%
  ggplot(aes(x=Deciles, y=value , fill= variable),position= "dodge")+
  geom_col()+
  labs(title = "Figure 3
Mexico
Growth Incidence Curve by income
2012-2014",
       y="Growth rate (total)",
       x="Decile",
       fill="Source of
  income")+
  geom_hline(yintercept = 0)+
  scale_y_continuous(breaks = seq(min,max,1),labels = rep(c(""),abs(min-max)+1))+
  annotate("text", x= "Mean", y= labels[1], label=round(Tasa_total[1],2))+
  annotate("text", x= "I", y= labels[2], label=round(Tasa_total[2],2))+
  annotate("text", x= "II", y= labels[3], label=round(Tasa_total[3],2))+
  annotate("text", x= "III", y= labels[4], label=round(Tasa_total[4],2))+
  annotate("text", x= "IV", y= labels[5], label=round(Tasa_total[5],2))+
  annotate("text", x= "V", y= labels[6], label=round(Tasa_total[6],2))+
  annotate("text", x= "VI", y= labels[7], label=round(Tasa_total[7],2))+
  annotate("text", x= "VII", y= labels[8], label=round(Tasa_total[8],2))+
  annotate("text", x= "VIII", y= labels[9], label=round(Tasa_total[9],2))+
  annotate("text", x= "IX", y= labels[10], label=round(Tasa_total[10],2))+
  annotate("text", x= "X", y= labels[11], label=round(Tasa_total[11],2))+
  theme_minimal()

GIC

######## Consumo 


GICconsumo<-data.frame(consumo_2012,consumo_2014)



GICconsumo<-GICconsumo%>%
  mutate(Rate=((Consumo_2014-Consumo_2012)/Consumo_2012)*100,
         Deciles=c("Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"),
         orden=1:11)


max<-round((max(GICconsumo$Rate)+1.5),0)
min<-round((min(GICconsumo$Rate)+1.5),0)

GIC_cons<-GICconsumo%>%
  mutate(Deciles=fct_relevel(Deciles,"Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"))%>%
  ggplot(aes(Deciles,Rate))+
  geom_col()+
  labs(title ="FIgure 4
Mexico
Growth Incidence Curve by consumption
2012-2014",
       y="Growth rate (total)",
       x="Decile")+
  scale_y_continuous(breaks=seq(-15,max,1))+
  geom_text(aes(label = round(Rate,2)),
            nudge_y =-2)+
  theme_minimal()


GIC_cons


######### 2014-2016 ##############
Tasa_total<-((Deciles_por_fuente_2016$`ING COR2016`- Deciles_por_fuente_2014$`ING COR2014`)/Deciles_por_fuente_2014$`ING COR2014`)*100

######################## Trabajo 

trabajo<-data.frame(trabajo2014=Deciles_por_fuente_2014$TRABAJO2014,
                    trabajo2016=Deciles_por_fuente_2016$TRABAJO2016,
                    ing_cor2014=Deciles_por_fuente_2014$`ING COR2014`,
                    ing_cor2016=Deciles_por_fuente_2016$`ING COR2016`,
                    Tasa_total)

trabajo<-trabajo%>%
  mutate(trabajo_aporte=((trabajo2016-trabajo2014)/((ing_cor2016-ing_cor2014)))*Tasa_total)

################################### Rentas 
rentas<-data.frame(rentas2014=Deciles_por_fuente_2014$RENTAS2014,
                   rentas2016=Deciles_por_fuente_2016$RENTAS2016,
                   ing_cor2014=Deciles_por_fuente_2014$`ING COR2014`,
                   ing_cor2016=Deciles_por_fuente_2016$`ING COR2016`,
                   Tasa_total)
rentas<-rentas%>%
  mutate(rentas_aporte=((rentas2016-rentas2014)/((ing_cor2016-ing_cor2014)))*Tasa_total)

################################### TRANSFERENCES 

TRANSFERENCES<-data.frame(TRANSFERENCES2014=Deciles_por_fuente_2014$TRANSFERENCES2014,
                          TRANSFERENCES2016=Deciles_por_fuente_2016$TRANSFERENCES2016,
                          ing_cor2014=Deciles_por_fuente_2014$`ING COR2014`,
                          ing_cor2016=Deciles_por_fuente_2016$`ING COR2016`,
                          Tasa_total)
TRANSFERENCES<-TRANSFERENCES%>%
  mutate(TRANSFERENCES_aporte=((TRANSFERENCES2016-TRANSFERENCES2014)/((ing_cor2016-ing_cor2014)))*Tasa_total)


################################### Benegobierno 

benegobierno<-data.frame(benegob2014=Deciles_por_fuente_2014$BENEGOBIERNO2014,
                         benegob2016=Deciles_por_fuente_2016$BENEGOBIERNO2016,
                         ing_cor2014=Deciles_por_fuente_2014$`ING COR2014`,
                         ing_cor2016=Deciles_por_fuente_2016$`ING COR2016`,
                         Tasa_total)

benegobierno<-benegobierno%>%
  mutate(benegob_aporte=((benegob2016-benegob2014)/((ing_cor2016-ing_cor2014)))*Tasa_total)

################################### OTHERS

OTHERS<-data.frame(OTHERS2014=Deciles_por_fuente_2014$OTHERS2014, 
                   OTHERS2016=Deciles_por_fuente_2016$OTHERS2016,
                   ing_cor2014=Deciles_por_fuente_2014$`ING COR2014`,
                   ing_cor2016=Deciles_por_fuente_2016$`ING COR2016`,
                   Tasa_total)

OTHERS<-OTHERS%>%
  mutate(OTHERS_aporte=((OTHERS2016-OTHERS2014)/((ing_cor2016-ing_cor2014)))*Tasa_total)


################################### Cuadro final 

cuadro_final<-data.frame(
  Labor=trabajo$trabajo_aporte,
  Capital=rentas$rentas_aporte,
  "Social programs"=benegobierno$benegob_aporte,
  "Other transfers"=TRANSFERENCES$TRANSFERENCES_aporte,
  "Imputed rent and other income"=OTHERS$OTHERS_aporte)

prueba<-cuadro_final%>%
  mutate(Prueba=Labor+Capital+Social.programs+Other.transfers+Imputed.rent.and.other.income)

all.equal(Tasa_total,prueba$Prueba)



names(cuadro_final)<-c("Labor","Capital","Social programs","Other transfers","Imputed rent 
and other income")

cuadro_final<-cuadro_final%>%
  mutate(Deciles= c("Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"))

row.names(cuadro_final)<-c("Mean","I","II","III","IV","V","VI","VII","VIII","IX","X")

cuadro_final<-cuadro_final%>%
  mutate(Deciles=fct_relevel(Deciles,"Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"))

cuadro_final<-melt(cuadro_final)

labels<-cuadro_final%>%
  mutate(labels=ifelse(value>0,value,0))%>%
  group_by(Deciles)%>%
  summarize(sum(labels)+1.5)

labels<-labels$`sum(labels) + 1.5`

max<-cuadro_final%>%
  group_by(Deciles)%>%
  filter(value>0)%>%
  summarize(sum(value))

max<-round(max(max$`sum(value)`)+2)

min<-cuadro_final%>%
  group_by(Deciles)%>%
  filter(value<0)%>%
  summarize(sum(value))

min<-round(min(min$`sum(value)`)-2)

GIC<-cuadro_final%>%
  mutate(Deciles=fct_relevel(Deciles,"Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"))%>%
  ggplot(aes(x=Deciles, y=value , fill= variable),position= "dodge")+
  geom_col()+
  labs(title = "Figure 5
Mexico
Growth Incidence Curve by income
2014-2016",
       y="Growth rate (total)",
       x="Decile",
       fill="Source of
  income")+
  geom_hline(yintercept = 0)+
  scale_y_continuous(breaks = seq(min,max,1),labels = rep(c(""),abs(min-max)+1))+
  annotate("text", x= "Mean", y= labels[1], label=round(Tasa_total[1],2))+
  annotate("text", x= "I", y= labels[2], label=round(Tasa_total[2],2))+
  annotate("text", x= "II", y= labels[3], label=round(Tasa_total[3],2))+
  annotate("text", x= "III", y= labels[4], label=round(Tasa_total[4],2))+
  annotate("text", x= "IV", y= labels[5], label=round(Tasa_total[5],2))+
  annotate("text", x= "V", y= labels[6], label=round(Tasa_total[6],2))+
  annotate("text", x= "VI", y= labels[7], label=round(Tasa_total[7],2))+
  annotate("text", x= "VII", y= labels[8], label=round(Tasa_total[8],2))+
  annotate("text", x= "VIII", y= labels[9], label=round(Tasa_total[9],2))+
  annotate("text", x= "IX", y= labels[10], label=round(Tasa_total[10],2))+
  annotate("text", x= "X", y= labels[11], label=round(Tasa_total[11],2))+
  theme_minimal()

GIC



######## Consumo 


GICconsumo<-data.frame(consumo_2014,consumo_2016)



GICconsumo<-GICconsumo%>%
  mutate(Rate=((Consumo_2016-Consumo_2014)/Consumo_2014)*100,
         Deciles=c("Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"),
         orden=1:11)


max<-round((max(GICconsumo$Rate)+1.5),0)
min<-round((min(GICconsumo$Rate)+1.5),0)

labels<-GICconsumo$Rate

GIC_cons<-GICconsumo%>%
  mutate(Deciles=fct_relevel(Deciles,"Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"))%>%
  ggplot(aes(Deciles,Rate))+
  geom_col()+
  labs(title ="FIgure 6
Mexico
Growth Incidence Curve by consumption
2014-2016",
       y="Growth rate (total)",
       x="Decile")+
  scale_y_continuous(breaks=seq(min,max,1))+
  annotate("text", x= "Mean", y= labels[1]+2, label=round(labels[1],2))+
  annotate("text", x= "I", y= labels[2]+2, label=round(labels[2],2))+
  annotate("text", x= "II", y= labels[3]+2, label=round(labels[3],2))+
  annotate("text", x= "III", y= labels[4]+2, label=round(labels[4],2))+
  annotate("text", x= "IV", y= labels[5]+2, label=round(labels[5],2))+
  annotate("text", x= "V", y= labels[6]+2, label=round(labels[6],2))+
  annotate("text", x= "VI", y= labels[7]+2, label=round(labels[7],2))+
  annotate("text", x= "VII", y= labels[8]+2, label=round(labels[8],2))+
  annotate("text", x= "VIII", y= labels[9]+2, label=round(labels[9],2))+
  annotate("text", x= "IX", y= labels[10]+2, label=round(labels[10],2))+
  annotate("text", x= "X", y= labels[11]-2, label=round(labels[11],2))+
  theme_minimal()


GIC_cons

######### 2016-2018 ##############
Tasa_total<-((Deciles_por_fuente_2018$`ING COR2018`- Deciles_por_fuente_2016$`ING COR2016`)/Deciles_por_fuente_2016$`ING COR2016`)*100

######################## Trabajo 

trabajo<-data.frame(trabajo2016=Deciles_por_fuente_2016$TRABAJO2016,
                    trabajo2018=Deciles_por_fuente_2018$TRABAJO2018,
                    ing_cor2016=Deciles_por_fuente_2016$`ING COR2016`,
                    ing_cor2018=Deciles_por_fuente_2018$`ING COR2018`,
                    Tasa_total)

trabajo<-trabajo%>%
  mutate(trabajo_aporte=((trabajo2018-trabajo2016)/((ing_cor2018-ing_cor2016)))*Tasa_total)

################################### Rentas 
rentas<-data.frame(rentas2016=Deciles_por_fuente_2016$RENTAS2016,
                   rentas2018=Deciles_por_fuente_2018$RENTAS2018,
                   ing_cor2016=Deciles_por_fuente_2016$`ING COR2016`,
                   ing_cor2018=Deciles_por_fuente_2018$`ING COR2018`,
                   Tasa_total)
rentas<-rentas%>%
  mutate(rentas_aporte=((rentas2018-rentas2016)/((ing_cor2018-ing_cor2016)))*Tasa_total)

################################### TRANSFERENCES 

TRANSFERENCES<-data.frame(TRANSFERENCES2016=Deciles_por_fuente_2016$TRANSFERENCES2016,
                          TRANSFERENCES2018=Deciles_por_fuente_2018$TRANSFERENCES2018,
                          ing_cor2016=Deciles_por_fuente_2016$`ING COR2016`,
                          ing_cor2018=Deciles_por_fuente_2018$`ING COR2018`,
                          Tasa_total)
TRANSFERENCES<-TRANSFERENCES%>%
  mutate(TRANSFERENCES_aporte=((TRANSFERENCES2018-TRANSFERENCES2016)/((ing_cor2018-ing_cor2016)))*Tasa_total)


################################### Benegobierno 

benegobierno<-data.frame(benegob2016=Deciles_por_fuente_2016$BENEGOBIERNO2016,
                         benegob2018=Deciles_por_fuente_2018$BENEGOBIERNO2018,
                         ing_cor2016=Deciles_por_fuente_2016$`ING COR2016`,
                         ing_cor2018=Deciles_por_fuente_2018$`ING COR2018`,
                         Tasa_total)

benegobierno<-benegobierno%>%
  mutate(benegob_aporte=((benegob2018-benegob2016)/((ing_cor2018-ing_cor2016)))*Tasa_total)

################################### OTHERS

OTHERS<-data.frame(OTHERS2016=Deciles_por_fuente_2016$OTHERS2016, 
                   OTHERS2018=Deciles_por_fuente_2018$OTHERS2018,
                   ing_cor2016=Deciles_por_fuente_2016$`ING COR2016`,
                   ing_cor2018=Deciles_por_fuente_2018$`ING COR2018`,
                   Tasa_total)

OTHERS<-OTHERS%>%
  mutate(OTHERS_aporte=((OTHERS2018-OTHERS2016)/((ing_cor2018-ing_cor2016)))*Tasa_total)


################################### Cuadro final 

cuadro_final<-data.frame(
  Labor=trabajo$trabajo_aporte,
  Capital=rentas$rentas_aporte,
  "Social programs"=benegobierno$benegob_aporte,
  "Other transfers"=TRANSFERENCES$TRANSFERENCES_aporte,
  "Imputed rent and other income"=OTHERS$OTHERS_aporte)

prueba<-cuadro_final%>%
  mutate(Prueba=Labor+Capital+Social.programs+Other.transfers+Imputed.rent.and.other.income)

all.equal(Tasa_total,prueba$Prueba)



names(cuadro_final)<-c("Labor","Capital","Social programs","Other transfers","Imputed rent 
and other income")

cuadro_final<-cuadro_final%>%
  mutate(Deciles= c("Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"))

row.names(cuadro_final)<-c("Mean","I","II","III","IV","V","VI","VII","VIII","IX","X")

cuadro_final<-cuadro_final%>%
  mutate(Deciles=fct_relevel(Deciles,"Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"))

cuadro_final<-melt(cuadro_final)

labels<-cuadro_final%>%
  mutate(labels=ifelse(value>0,value,0))%>%
  group_by(Deciles)%>%
  summarize(sum(labels)+1.5)

labels<-labels$`sum(labels) + 1.5`

max<-cuadro_final%>%
  group_by(Deciles)%>%
  filter(value>0)%>%
  summarize(sum(value))

max<-round(max(max$`sum(value)`)+2)

min<-cuadro_final%>%
  group_by(Deciles)%>%
  filter(value<0)%>%
  summarize(sum(value))

min<-round(min(min$`sum(value)`)-2)

GIC<-cuadro_final%>%
  mutate(Deciles=fct_relevel(Deciles,"Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"))%>%
  ggplot(aes(x=Deciles, y=value , fill= variable),position= "dodge")+
  geom_col()+
  labs(title = "Figure 7
Mexico
Growth Incidence Curve by income
2016-2018",
       y="Growth rate (total)",
       x="Decile",
       fill="Source of
  income")+
  geom_hline(yintercept = 0)+
  scale_y_continuous(breaks = seq(min,max,1),labels = rep(c(""),abs(min-max)+1))+
  annotate("text", x= "Mean", y= labels[1], label=round(Tasa_total[1],2))+
  annotate("text", x= "I", y= labels[2], label=round(Tasa_total[2],2))+
  annotate("text", x= "II", y= labels[3], label=round(Tasa_total[3],2))+
  annotate("text", x= "III", y= labels[4], label=round(Tasa_total[4],2))+
  annotate("text", x= "IV", y= labels[5], label=round(Tasa_total[5],2))+
  annotate("text", x= "V", y= labels[6], label=round(Tasa_total[6],2))+
  annotate("text", x= "VI", y= labels[7], label=round(Tasa_total[7],2))+
  annotate("text", x= "VII", y= labels[8], label=round(Tasa_total[8],2))+
  annotate("text", x= "VIII", y= labels[9], label=round(Tasa_total[9],2))+
  annotate("text", x= "IX", y= labels[10], label=round(Tasa_total[10],2))+
  annotate("text", x= "X", y= labels[11], label=round(Tasa_total[11],2))+
  theme_minimal()

GIC


######## Consumo 


GICconsumo<-data.frame(consumo_2016,consumo_2018)



GICconsumo<-GICconsumo%>%
  mutate(Rate=((Consumo_2018-Consumo_2016)/Consumo_2016)*100,
         Deciles=c("Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"),
         orden=1:11)


max<-round((max(GICconsumo$Rate)+1.5),0)
min<-round((min(GICconsumo$Rate)+1.5),0)

labels<-GICconsumo$Rate

GIC_cons<-GICconsumo%>%
  mutate(Deciles=fct_relevel(Deciles,"Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"))%>%
  ggplot(aes(Deciles,Rate))+
  geom_col()+
  labs(title ="FIgure 8
Mexico
Growth Incidence Curve by consumption
2016-2018",
       y="Growth rate (total)",
       x="Decile")+
  scale_y_continuous(breaks=seq(min,max,1))+
  annotate("text", x= "Mean", y= labels[1]+2, label=round(labels[1],2))+
  annotate("text", x= "I", y= labels[2]+2, label=round(labels[2],2))+
  annotate("text", x= "II", y= labels[3]+2, label=round(labels[3],2))+
  annotate("text", x= "III", y= labels[4]+2, label=round(labels[4],2))+
  annotate("text", x= "IV", y= labels[5]+2, label=round(labels[5],2))+
  annotate("text", x= "V", y= labels[6]+2, label=round(labels[6],2))+
  annotate("text", x= "VI", y= labels[7]+2, label=round(labels[7],2))+
  annotate("text", x= "VII", y= labels[8]+2, label=round(labels[8],2))+
  annotate("text", x= "VIII", y= labels[9]+2, label=round(labels[9],2))+
  annotate("text", x= "IX", y= labels[10]+2, label=round(labels[10],2))+
  annotate("text", x= "X", y= labels[11]+2, label=round(labels[11],2))+
  theme_minimal()


GIC_cons


######### 2010-2014 ##############
Tasa_total<-((Deciles_por_fuente_2014$`ING COR2014`- Deciles_por_fuente_2010$`ING COR2010`)/Deciles_por_fuente_2010$`ING COR2010`)*100

######################## Trabajo 

trabajo<-data.frame(trabajo2010=Deciles_por_fuente_2010$TRABAJO2010,
                    trabajo2014=Deciles_por_fuente_2014$TRABAJO2014,
                    ing_cor2010=Deciles_por_fuente_2010$`ING COR2010`,
                    ing_cor2014=Deciles_por_fuente_2014$`ING COR2014`,
                    Tasa_total)

trabajo<-trabajo%>%
  mutate(trabajo_aporte=((trabajo2014-trabajo2010)/((ing_cor2014-ing_cor2010)))*Tasa_total)

################################### Rentas 
rentas<-data.frame(rentas2010=Deciles_por_fuente_2010$RENTAS2010,
                   rentas2014=Deciles_por_fuente_2014$RENTAS2014,
                   ing_cor2010=Deciles_por_fuente_2010$`ING COR2010`,
                   ing_cor2014=Deciles_por_fuente_2014$`ING COR2014`,
                   Tasa_total)
rentas<-rentas%>%
  mutate(rentas_aporte=((rentas2014-rentas2010)/((ing_cor2014-ing_cor2010)))*Tasa_total)

################################### TRANSFERENCES 

TRANSFERENCES<-data.frame(TRANSFERENCES2010=Deciles_por_fuente_2010$TRANSFERENCES2010,
                          TRANSFERENCES2014=Deciles_por_fuente_2014$TRANSFERENCES2014,
                          ing_cor2010=Deciles_por_fuente_2010$`ING COR2010`,
                          ing_cor2014=Deciles_por_fuente_2014$`ING COR2014`,
                          Tasa_total)
TRANSFERENCES<-TRANSFERENCES%>%
  mutate(TRANSFERENCES_aporte=((TRANSFERENCES2014-TRANSFERENCES2010)/((ing_cor2014-ing_cor2010)))*Tasa_total)


################################### Benegobierno 

benegobierno<-data.frame(benegob2010=Deciles_por_fuente_2010$BENEGOBIERNO2010,
                         benegob2014=Deciles_por_fuente_2014$BENEGOBIERNO2014,
                         ing_cor2010=Deciles_por_fuente_2010$`ING COR2010`,
                         ing_cor2014=Deciles_por_fuente_2014$`ING COR2014`,
                         Tasa_total)

benegobierno<-benegobierno%>%
  mutate(benegob_aporte=((benegob2014-benegob2010)/((ing_cor2014-ing_cor2010)))*Tasa_total)

################################### OTHERS

OTHERS<-data.frame(OTHERS2010=Deciles_por_fuente_2010$OTHERS2010, 
                   OTHERS2014=Deciles_por_fuente_2014$OTHERS2014,
                   ing_cor2010=Deciles_por_fuente_2010$`ING COR2010`,
                   ing_cor2014=Deciles_por_fuente_2014$`ING COR2014`,
                   Tasa_total)

OTHERS<-OTHERS%>%
  mutate(OTHERS_aporte=((OTHERS2014-OTHERS2010)/((ing_cor2014-ing_cor2010)))*Tasa_total)


################################### Cuadro final 

cuadro_final<-data.frame(
  Labor=trabajo$trabajo_aporte,
  Capital=rentas$rentas_aporte,
  "Social programs"=benegobierno$benegob_aporte,
  "Other transfers"=TRANSFERENCES$TRANSFERENCES_aporte,
  "Imputed rent and other income"=OTHERS$OTHERS_aporte)

prueba<-cuadro_final%>%
  mutate(Prueba=Labor+Capital+Social.programs+Other.transfers+Imputed.rent.and.other.income)

all.equal(Tasa_total,prueba$Prueba)



names(cuadro_final)<-c("Labor","Capital","Social programs","Other transfers","Imputed rent 
and other income")

cuadro_final<-cuadro_final%>%
  mutate(Deciles= c("Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"))

row.names(cuadro_final)<-c("Mean","I","II","III","IV","V","VI","VII","VIII","IX","X")

cuadro_final<-cuadro_final%>%
  mutate(Deciles=fct_relevel(Deciles,"Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"))

cuadro_final<-melt(cuadro_final)

labels<-cuadro_final%>%
  mutate(labels=ifelse(value>0,value,0))%>%
  group_by(Deciles)%>%
  summarize(sum(labels)+1.5)

labels<-labels$`sum(labels) + 1.5`

max<-cuadro_final%>%
  group_by(Deciles)%>%
  filter(value>0)%>%
  summarize(sum(value))

max<-round(max(max$`sum(value)`)+2)

min<-cuadro_final%>%
  group_by(Deciles)%>%
  filter(value<0)%>%
  summarize(sum(value))

min<-round(min(min$`sum(value)`)-2)

GIC<-cuadro_final%>%
  mutate(Deciles=fct_relevel(Deciles,"Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"))%>%
  ggplot(aes(x=Deciles, y=value , fill= variable),position= "dodge")+
  geom_col()+
  labs(title = "Figure 9
Mexico
Growth Incidence Curve by income
2010-2014",
       y="Growth rate (total)",
       x="Decile",
       fill="Source of
  income")+
  geom_hline(yintercept = 0)+
  scale_y_continuous(breaks = seq(min,max,1),labels = rep(c(""),abs(min-max)+1))+
  annotate("text", x= "Mean", y= labels[1], label=round(Tasa_total[1],2))+
  annotate("text", x= "I", y= labels[2], label=round(Tasa_total[2],2))+
  annotate("text", x= "II", y= labels[3], label=round(Tasa_total[3],2))+
  annotate("text", x= "III", y= labels[4], label=round(Tasa_total[4],2))+
  annotate("text", x= "IV", y= labels[5], label=round(Tasa_total[5],2))+
  annotate("text", x= "V", y= labels[6], label=round(Tasa_total[6],2))+
  annotate("text", x= "VI", y= labels[7], label=round(Tasa_total[7],2))+
  annotate("text", x= "VII", y= labels[8], label=round(Tasa_total[8],2))+
  annotate("text", x= "VIII", y= labels[9], label=round(Tasa_total[9],2))+
  annotate("text", x= "IX", y= labels[10], label=round(Tasa_total[10],2))+
  annotate("text", x= "X", y= labels[11], label=round(Tasa_total[11],2))+
  theme_minimal()

GIC

######## Consumo 


GICconsumo<-data.frame(consumo_2010,consumo_2014)



GICconsumo<-GICconsumo%>%
  mutate(Rate=((Consumo_2014-Consumo_2010)/Consumo_2010)*100,
         Deciles=c("Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"),
         orden=1:11)


max<-round((max(GICconsumo$Rate)+1.5),0)
min<-round((min(GICconsumo$Rate)+1.5),0)

labels<-GICconsumo$Rate

GIC_cons<-GICconsumo%>%
  mutate(Deciles=fct_relevel(Deciles,"Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"))%>%
  ggplot(aes(Deciles,Rate))+
  geom_col()+
  labs(title ="FIgure 10
Mexico
Growth Incidence Curve by consumption
2010-2014",
       y="Growth rate (total)",
       x="Decile")+
  scale_y_continuous(breaks=seq(0,max,10))+
  annotate("text", x= "Mean", y= labels[1]+2, label=round(labels[1],2))+
  annotate("text", x= "I", y= labels[2]+2, label=round(labels[2],2))+
  annotate("text", x= "II", y= labels[3]+2, label=round(labels[3],2))+
  annotate("text", x= "III", y= labels[4]+2, label=round(labels[4],2))+
  annotate("text", x= "IV", y= labels[5]+2, label=round(labels[5],2))+
  annotate("text", x= "V", y= labels[6]+2, label=round(labels[6],2))+
  annotate("text", x= "VI", y= labels[7]+2, label=round(labels[7],2))+
  annotate("text", x= "VII", y= labels[8]+2, label=round(labels[8],2))+
  annotate("text", x= "VIII", y= labels[9]+2, label=round(labels[9],2))+
  annotate("text", x= "IX", y= labels[10]+2, label=round(labels[10],2))+
  annotate("text", x= "X", y= labels[11]+2, label=round(labels[11],2))+
  theme_minimal()


GIC_cons



#proporciones de ingres
library(tidyverse)
library(reshape2)
base_2018<-data.frame(Decil=c("Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"),
                 Labor=c(0.67,0.39,0.54,0.62,0.66,0.68,0.71,0.73,0.72,0.72,0.64),
                 "Capital gains"=c(0.06,0.01,0.01,0.01,0.02,0.02,0.02,0.02,0.03,0.04,0.13),
                 "Social programs"=c(0.01,0.14,0.07,0.04,0.03,0.02,0.02,0.01,0.01,0.00,0.00),
                 "Others transfers"=c(0.14,0.23,0.20,0.17,0.15,0.15,0.13,0.12,0.13,0.13,0.14),
                 "Imputed,rent,and,other,income"=c(0.11,0.23,0.18,0.15,0.14,0.13,0.12,0.12,0.11,0.10,0.09))

names(base_2018)<-c("Decil","Labor","Capital gains","Social programs","Others transfers","Imputed rent and other income")


base_2018<-melt(base_2018)

grafica_2018<-base_2018%>%
  mutate(Decil=fct_relevel(Decil,"Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"))%>%
  mutate(variable=fct_relevel(variable,"Social programs","Capital gains","Others transfers","Imputed rent and other income","Labor"))%>%
  ggplot(aes(Decil,value, fill= variable),position= "dodge")+
  geom_col()+
  theme_minimal()

grafica_2018<-grafica_2018+theme(legend.position="bottom")

grafica_2018<-grafica_2018+ggtitle("2018")+
  theme(plot.title = element_text(hjust = 0.5))

grafica_2018<-grafica_2018+
  theme(legend.title = element_blank())+
  xlab("") + ylab("")

grafica_2018

library(tidyverse)
base_2010<-data.frame(Decil=c("Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"),
                      Labor=c(0.68,0.38,0.52,0.60,0.63,0.66,0.69,0.70,0.71,0.71,0.69),
                      "Capital gains"=c(0.04,0.01,0.01,0.01,0.01,0.01,0.01,0.02,0.02,0.03,0.07),
                      "Social programs"=c(0.02,0.18,0.09,0.05,0.04,0.03,0.02,0.01,0.01,0.01,0.00),
                      "Others transfers"=c(0.14,0.19,0.19,0.18,0.17,0.15,0.14,0.13,0.13,0.13,0.12),
                      "Imputed,rent,and,other,income"=c(0.13,0.24,0.19,0.16,0.15,0.15,0.14,0.14,0.13,0.13,0.11))

names(base_2010)<-c("Decil","Labor","Capital gains","Social programs","Others transfers","Imputed rent and other income")

base_2010<-melt(base_2010)

grafica_2010<-base_2010%>%
  mutate(Decil=fct_relevel(Decil,"Mean","I","II","III","IV","V","VI","VII","VIII","IX","X"))%>%
  mutate(variable=fct_relevel(variable,"Social programs","Capital gains","Others transfers","Imputed rent and other income","Labor"))%>%
  ggplot(aes(Decil,value, fill= variable),position= "dodge")+
  geom_col()+
  theme_minimal()

grafica_2010<-grafica_2010+theme(legend.position="bottom")

grafica_2010<-grafica_2010+ggtitle("2010")+
  theme(plot.title = element_text(hjust = 0.5))

grafica_2010<-grafica_2010+
  theme(legend.title = element_blank())+
  xlab("") + ylab("")

grafica_2010

library(ggpubr)

final<-ggarrange(grafica_2010,grafica_2018,ncol = 1, nrow = 2)

annotate_figure(final,top = text_grob("Figure 2
Mexico
Relative contribution by source of income to total household income",
                                      color = "black", face = "bold", size = 14))


