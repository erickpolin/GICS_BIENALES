################# cargando las bases de datos #############
library(foreign)
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(reshape2)

########## 2010

setwd(c("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/ENIGH 2010/ENIGH2010"))
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
         "OTHERS"=`ESTIM ALQU2010`+`OTROS INGRESOS2010`)

Deciles_por_fuente_2010<-Deciles_por_fuente_2010%>%
  mutate(prueba2=TRABAJO2010+RENTAS2010+BENEGOBIERNO2010+TRANSFERENCES2010+OTHERS)

all.equal(Deciles_por_fuente_2010$`ING COR2010`,Deciles_por_fuente_2010$prueba2)




######### 2012

setwd(c("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/ENIGH_2012/ENIGH2012"))
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
         "OTHERS"=`ESTIM ALQU2012`+`OTROS INGRESOS2012`)

Deciles_por_fuente_2012<-Deciles_por_fuente_2012%>%
  mutate(prueba2=TRABAJO2012+RENTAS2012+BENEGOBIERNO2012+TRANSFERENCES2012+OTHERS)

all.equal(Deciles_por_fuente_2012$`ING COR2012`,Deciles_por_fuente_2012$prueba2)




######### 2014

setwd(c("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/ENIGH_2014/ENIGH_2014"))
Deciles_por_fuente_2014<-read.dbf("Nacional por fuente por DECIL estimaciones 2014.dbf")

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
         "OTHERS"=`ESTIM ALQU2014`+`OTROS INGRESOS2014`)

Deciles_por_fuente_2014<-Deciles_por_fuente_2014%>%
  mutate(prueba2=TRABAJO2014+RENTAS2014+BENEGOBIERNO2014+TRANSFERENCES2014+OTHERS)

all.equal(Deciles_por_fuente_2014$`ING COR2014`,Deciles_por_fuente_2014$prueba2)


######## 2016

setwd(c("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/ENIGH_2016/ENIGH_2016"))
Deciles_por_fuente_2016<-read.dbf("Nacional por fuente por DECIL estimaciones 2016.dbf")

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
         "OTHERS"=`ESTIM ALQU2016`+`OTROS INGRESOS2016`)

Deciles_por_fuente_2016<-Deciles_por_fuente_2016%>%
  mutate(prueba2=TRABAJO2016+RENTAS2016+BENEGOBIERNO2016+TRANSFERENCES2016+OTHERS)

all.equal(Deciles_por_fuente_2016$`ING COR2016`,Deciles_por_fuente_2016$prueba2)


############ 2018

setwd(c("C:/Users/Erick/Dropbox/GIC/GITHUB2018/GIC/ENIGH 2018/ENIGH2018"))
Deciles_por_fuente_2018<-read.dbf("Nacional por fuente por DECIL estimaciones 2018.dbf")

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
         "OTHERS"=`ESTIM ALQU2018`+`OTROS INGRESOS2018`)

Deciles_por_fuente_2018<-Deciles_por_fuente_2018%>%
  mutate(prueba2=TRABAJO2018+RENTAS2018+BENEGOBIERNO2018+TRANSFERENCES2018+OTHERS)

all.equal(Deciles_por_fuente_2018$`ING COR2018`,Deciles_por_fuente_2018$prueba2)



######### 2010-2012 
Tasa_total<-((Deciles_por_fuente_2012$`ING COR2012`- Deciles_por_fuente_2010$`ING COR2010`)/Deciles_por_fuente_2010$`ING COR2010`)*100





















########################## Trabajo 

trabajo<-data.frame(trabajo2010=Deciles_por_fuente_2010$TRABAJO2010,
                    trabajo2012=Deciles_por_fuente_2012$TRABAJO2012,
                    ing_cor2010=Deciles_por_fuente_2010$`ING COR2010`,
                    ing_cor2012=Deciles_por_fuente_2012$`ING COR2012`,
                    Tasa_total)

trabajo<-trabajo%>%
  mutate(trabajo_aporte=((trabajo2012-trabajo2010)/((ing_cor2012-ing_cor2010)))*Tasa_total)

################################### Rentas 
rentas<-data.frame(rentas2010=Deciles_por_fuente_2010$RENTAS2010,rentas2012=Deciles_por_fuente_2012$RENTAS2012,
                   ing_cor2010=Deciles_por_fuente_2010$`ING COR2010`,
                   ing_cor2012=Deciles_por_fuente_2012$`ING COR2012`,
                   Tasa_total)
rentas<-rentas%>%
  mutate(rentas_aporte=((rentas2012-rentas2010)/((ing_cor2012-ing_cor2010)))*Tasa_total)

################################### TRANSFERENCES 

TRANSFERENCES<-data.frame(TRANSFERENCES2010=Deciles_por_fuente_2010$JUBILACION2010,TRANSFERENCES2012=Deciles_por_fuente_2012$JUBILACION2012,
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

OTHERS<-data.frame(OTHERS2010=Deciles_por_fuente_2010$`TRANS HOG2010`, 
                           OTHERS2012=Deciles_por_fuente_2012$`TRANS HOG2012`,
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



names(cuadro_final)<-c("Labor","Capital","Pensions","Scholarships","Donations","Remittances","Government transfers",
                       "Household transfers","Instituion transfers","Imputed rent","Others")

























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
  labs(title = "Growth Incidence Curve Nacional 2010-2012",
       y="Growth rate (total)",
       x="Decile",
       fill="Source of
  income")+
  geom_hline(yintercept = 0)+
  scale_y_continuous(breaks = seq(-10,30,10),labels = c("","","","",""))+
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

GIC<-ggplotly(GIC)

GIC

saveWidget(GIC,fil="GIC_Mexico_by_source_of_income.html")

rm(list=ls())