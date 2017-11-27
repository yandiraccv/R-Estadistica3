---
title: "SEGUROS"
output: 
  flexdashboard::flex_dashboard:
    orientation: rows
    vertical_layout: fill
    theme: journal
    #logo: logo_svs.png
runtime: shiny
---

```{r setup, include=FALSE}

library(flexdashboard)
library(dplyr)
library(ggplot2)
library(scales)
library(plotly)
library(data.table)
library(DT)

#meses<-c("enero","febrero","marzo","abril","mayo","junio","julio","agosto","septiembre","octubre","noviembre","diciembre")
meses<-c("January","February","March","April","May","June","July","August","September","October","November","December")
n_dias<-c(31,28,31,30,31,30,31,31,30,31,30,31)
MESES<-data.frame(meses,n_dias)


VENTAS<-read.csv("VENTAS.csv",sep=",",dec=".",header=T)
CANCEL<-read.csv("CANCEL.csv",sep=",",dec=".",header=T)
dias_ventas<-VENTAS%>%group_by(mes)%>%summarise(dia=max(dia))
dias_ventas<-dias_ventas[order(match(dias_ventas$mes,meses)),]

dias_cancel<-CANCEL%>%group_by(mes)%>%summarise(dia=max(dia))

####VENTAS####
monto_mes<-VENTAS%>%select(mes,monto)%>%group_by(mes)%>%summarise(Monto_Primas=sum(monto))
MONTO_M<-merge(monto_mes,as.data.frame(meses),by.x = "mes",by.y="meses",all.y=T)
MONTO_M<-MONTO_M[order(match(MONTO_M$mes,meses)),]

prim_mes<-VENTAS%>%group_by(mes)%>%summarise(Primas=sum(numeroPrimas))
PRIM_M<-merge(prim_mes,as.data.frame(meses),by.x = "mes",by.y="meses",all.y=T)
PRIM_M<-PRIM_M[order(match(PRIM_M$mes,meses)),]
PRIM_M$Acumulado<-cumsum(PRIM_M$Primas)
PRIM_M$grupo<-rep("Primas",12)
PRIM_M$mes<-abbreviate(PRIM_M$mes,3)
PRIM_M$mes<-factor(PRIM_M$mes,levels=abbreviate(meses,3))

MONTO_PRIM_M<-cbind(PRIM_M%>%select(mes,Primas),Monto_Primas=MONTO_M$Monto_Primas)
MONTO_PRIM_M$grupo<-rep("Monto",12)
montos_mensuales<-MONTO_PRIM_M[order(match(MONTO_PRIM_M$mes,abbreviate(meses,3))),]
montos_mensuales$Acumulado<-cumsum(montos_mensuales$Monto_Primas)

presup<-c(2212.32,7918.49,13241,20403,42347.25,76201.80,76471.45,109721.41,217409.67,
          406379.48,645842.30,868502.18)
#Presupuestos<-METAS%>%group_by(mes)%>%summarise(presup=sum(meta))
Presupuestos<-data.frame(mes=abbreviate(meses,3),presupuesto=presup,Acumulado=cumsum(presup))
Presupuestos$grupo<-rep("Meta_anual",12)

data_plot<-rbind(montos_mensuales%>%select(mes,Acumulado,grupo),Presupuestos%>%select(mes,Acumulado,grupo))
#data_plot$mes<-factor(data_plot$mes,levels = abbreviate(meses,3))

####ANULACIONES#####
monto_anul_mes<-CANCEL%>%select(mes,monto)%>%group_by(mes)%>%summarise(Monto_Primas=sum(monto))
MONTO_ANUL_M<-merge(monto_anul_mes,as.data.frame(meses),by.x = "mes",by.y="meses",all.y=T)
MONTO_ANUL_M<-MONTO_ANUL_M[order(match(MONTO_ANUL_M$mes,meses)),]

prim_anul_mes<-CANCEL%>%group_by(mes)%>%summarise(Primas=sum(numeroPrimas))
PRIM_ANUL_M<-merge(prim_anul_mes,as.data.frame(meses),by.x = "mes",by.y="meses",all.y=T)
PRIM_ANUL_M<-PRIM_ANUL_M[order(match(PRIM_ANUL_M$mes,meses)),]
PRIM_ANUL_M$Acumulado<-cumsum(PRIM_ANUL_M$Primas)
PRIM_ANUL_M$grupo<-rep("Primas",12)
PRIM_ANUL_M$mes<-abbreviate(PRIM_ANUL_M$mes,3)
PRIM_ANUL_M$mes<-factor(PRIM_ANUL_M$mes,levels=abbreviate(meses,3))
monto_primas_anul_mes<-cbind(PRIM_ANUL_M,Monto_Primas_AN=MONTO_ANUL_M$Monto_Primas)

tabla_ventas_anul<-cbind(montos_mensuales%>%select(mes,Primas,Monto_Primas,Acumulado,grupo),monto_primas_anul_mes%>%select(Primas_AN=Primas,Monto_Primas_AN))
tabla_ventas_anul$Primas_AN<-ifelse(is.na(tabla_ventas_anul$Primas_AN),0,tabla_ventas_anul$Primas_AN)
tabla_ventas_anul$Monto_Primas_AN<-ifelse(is.na(tabla_ventas_anul$Monto_Primas_AN),0,tabla_ventas_anul$Monto_Primas_AN)

tabla_ventas_anul$Primas_NE<-tabla_ventas_anul$Primas+tabla_ventas_anul$Primas_AN
tabla_ventas_anul$Monto_Primas_NE<-tabla_ventas_anul$Monto_Primas+tabla_ventas_anul$Monto_Primas_AN
tabla_ventas_anul$Acumulado_NE<-cumsum(tabla_ventas_anul$Monto_Primas_NE)
tabla_final<-tabla_ventas_anul%>%select(-grupo)
#tabla_final$MES<-meses

TABLA_GEN_DASH<-data.frame(MES=meses,PRIMAS=format(tabla_final$Primas, decimal.mark=",", big.mark=".",small.mark="."),
                           
                           MONTO_PRIMAS=format(tabla_final$Monto_Primas, decimal.mark=",",big.mark=".",small.mark="."),
                           
                           ACUMULADO=format(tabla_final$Acumulado, decimal.mark=",", big.mark=".",small.mark="."),
                           
                           PRIMAS_AN=format(tabla_final$Primas_AN, decimal.mark=",", big.mark=".",small.mark="."),
                           
                           MONTO_PRIMAS_AN=format(tabla_final$Monto_Primas_AN, decimal.mark=",", big.mark=".",small.mark="."),
                           
                           PRIMAS_NE=format(tabla_final$Primas_NE, decimal.mark=",", big.mark=".",small.mark="."),
                           
                           MONTO_PRIMAS_NE=format(tabla_final$Monto_Primas_AN, decimal.mark=",", big.mark=".",small.mark="."),
                           
                           ACUMULADO_NE=format(tabla_final$Acumulado_NE, decimal.mark=",", big.mark=".",small.mark="."))

data_plot_ne<-rbind(tabla_ventas_anul%>%select(mes,Acumulado=Acumulado_NE,grupo), Presupuestos%>%select(mes,Acumulado,grupo))
#data_plot_ne$mes<-abbreviate(data_plot_ne$mes,3)
#data_plot_ne$mes<-factor(data_plot_ne$mes,levels = abbreviate(meses,3))

primas_ne_mes<-data.frame(tabla_final%>%select(mes,Primas_NE))
primas_ne_mes$Acumulado<-cumsum(primas_ne_mes$Primas_NE)
primas_ne_mes$grupo<-rep("Primas",12)
primas_ne_mes<-as.data.frame(primas_ne_mes)
#primas_ne_mes$mes<-abbreviate(primas_ne_mes$mes,3)
#primas_ne_mes$mes<-factor(primas_ne_mes$mes,levels = abbreviate(meses,3))

######VENTAS DIARIAS####
v_dia_3mes<-function(VENTAS){
  #Tabla ventas en mes actual diarias
  T_DIARIO_MACTUAL<-VENTAS%>%select(numeroVentas,monto,dia,mes)%>%filter(mes==dias_ventas$mes[nrow(dias_ventas)])%>%
    group_by(dia)%>%summarise(No_Ventas=sum(numeroVentas,na.rm=T),Monto=sum(monto,na.rm=T))
  T_DIARIO_MACTUAL$Acum_No_Ventas<-cumsum(T_DIARIO_MACTUAL$No_Ventas)
  T_DIARIO_MACTUAL$Acum_Monto<-cumsum(T_DIARIO_MACTUAL$Monto)
  
  T_DIARIO_3M<-VENTAS%>%select(numeroVentas,monto,dia,mes)%>%filter(mes==dias_ventas$mes[nrow(dias_ventas)]|mes==dias_ventas$mes[nrow(dias_ventas)-1]|mes==dias_ventas$mes[nrow(dias_ventas)-2])%>%
    group_by(mes,dia)%>%summarise(No_Ventas=sum(numeroVentas,na.rm=T),Monto=sum(monto,na.rm=T))
  
  #mm<-METAS%>%filter(mes==dias_ventas$mes[nrow(dias_ventas)])%>%summarise(sum(meta))
  mm<-Presupuestos$presupuesto[nrow(dias_ventas)]
  mmd<-mm/MESES[nrow(dias_ventas),2]
  meta_mes<-data.frame(mes=rep("meta_mes",MESES[nrow(dias_ventas),2]),dia=c(1:MESES[nrow(dias_ventas),2]),No_ventas=rep(NA,MESES[nrow(dias_ventas),2]),Monto=rep(NA,MESES[nrow(dias_ventas),2]),Acum_No_Ventas=rep(NA,MESES[nrow(dias_ventas),2]),Acum_Monto=as.numeric(mmd)*c(1:MESES[nrow(dias_ventas),2]))
  PLOT_DIARIO_3M<-as.data.frame(T_DIARIO_3M%>%group_by(mes)%>%mutate(Acum_No_Ventas=cumsum(No_Ventas),Acum_Monto=cumsum(Monto)))
  colnames(meta_mes)<-colnames(PLOT_DIARIO_3M)
  PLOT_DIARIO_3M<-as.data.frame(rbind(PLOT_DIARIO_3M,meta_mes))
  lvs<-unique(PLOT_DIARIO_3M$mes)
  lvs<-lvs[order(match(lvs,meses))]
  PLOT_DIARIO_3M$mes<-factor(PLOT_DIARIO_3M$mes,levels=lvs)
  
  M<-data.frame(mes=unique(PLOT_DIARIO_3M$mes))
  M<-data.frame(mes=M[order(match(M$mes,meses)),],aux=c(1,2,3,4))
  M1<-PLOT_DIARIO_3M%>%filter(mes==M[nrow(M)-1,1])
  M2<-PLOT_DIARIO_3M%>%filter(mes==M[nrow(M)-2,1])
  M3<-PLOT_DIARIO_3M%>%filter(mes==M[nrow(M)-3,1])
  DIAS<-data.frame(dia=c(1:31))
  
  m_M1<-merge(DIAS,M1%>%select(-mes),all.x = T)
  colnames(m_M1)<-c("dia",paste0("No_V_",M[nrow(M),1]),paste0("Monto_",M[nrow(M),1]),
                    paste0("Acum_No_V_",M[nrow(M),1]),paste0("Acum_Monto_",M[nrow(M),1]))
  
  m_M2<-merge(DIAS,M2%>%select(-mes),all.x = T)
  colnames(m_M2)<-c("dia",paste0("No_V_",M[nrow(M)-2,1]),paste0("Monto_",M[nrow(M)-2,1]),
                    paste0("Acum_No_V_",M[nrow(M)-2,1]),paste0("Acum_Monto_",M[nrow(M)-2,1]))
  
  m_M3<-merge(DIAS,M3%>%select(-mes),all.x = T)
  colnames(m_M3)<-c("dia",paste0("No_V_",M[nrow(M)-3,1]),paste0("Monto_",M[nrow(M)-3,1]),
                    paste0("Acum_No_V_",M[nrow(M)-3,1]),paste0("Acum_Monto_",M[nrow(M)-3,1]))
  
  DIARIO_3M_FINAL<-cbind(m_M1,m_M2%>%select(-dia),m_M3%>%select(-dia))
  list(plot_diario_3M=PLOT_DIARIO_3M,diario_3M_final=DIARIO_3M_FINAL)
}
D3M_list<-v_dia_3mes(VENTAS)
data_plot_dia<-D3M_list$plot_diario_3M
#DIARIO_3M_FINAL<-D3M_list$diario_3M_final

#####CUMPLIMIENTO PRODUCTO####
ventas_producto<-VENTAS%>%group_by(producto)%>%summarise(Real=sum(monto))
#Noviembre
ventas_producto$Presupuesto<-c(81753.3528,103893.75,128683.49,3792.25,149786.76,171340.8,128683.49)

ventas_producto$Cumplimiento<-round(100*ventas_producto$Real/ventas_producto$Presupuesto,2)

ventas_producto<-as.data.frame(ventas_producto)
```

```{r}
print(paste("AL",dias_ventas$dia[nrow(dias_ventas)],dias_ventas$mes[nrow(dias_ventas)]))
```


VENTAS 
=======================================================================

Row
-----------------------------------------------------------------------

### MONTO vs PRESUPUESTO ACUMULADOS

```{r}
p1<-ggplot(data_plot,aes(x=mes, y=Acumulado,fill=grupo)) + 
  geom_bar(stat = "identity", position = 'identity',colour="black") + 
  xlab("") + ylab("") +guides(fill=FALSE)+ scale_y_continuous(labels = dollar) + 
  scale_fill_brewer(palette="Pastel1") 
  #ggplotly(p1,tooltip = c("Acumulado","grupo"))
pp1<-plotly_build(p1)
style(pp1)%>%layout(yaxis=list(tickfont = list(size=9)))

#geom_text(aes(label=format(data_plot$Acumulado,decimal.mark = ",",big.mark = ".",small.mark = ".")), vjust=-2, colour="black",size=2.5)
```


### ACUMULADO PRIMAS VENDIDAS

```{r}
p2<-ggplot(data = PRIM_M,aes(x=mes,y=Acumulado,fill=grupo)) + 
      geom_bar(stat="identity", width=0.5,colour="black") + 
      xlab("") + ylab("")+ scale_fill_brewer(palette="Set3")+guides(fill=FALSE)
      #ggplotly(p2,tooltip = c("Acumulado","grupo"))
pp2<-plotly_build(p2)
style(pp2)%>%layout(yaxis=list(tickfont = list(size=9)))
```

Row
-----------------------------------------------------------------------
### MONTO vs PRESUPUESTO ACUMULADOS NETOS

```{r}
p1_ne<-ggplot(data_plot_ne,aes(x=mes, y=Acumulado,fill=grupo)) + 
  geom_bar(stat = "identity", position = 'identity',colour="black") + 
  xlab("") + ylab("")+guides(fill=FALSE) + scale_y_continuous(labels = dollar) + 
  scale_fill_brewer(palette="Pastel2") 
  #ggplotly(p1_ne,tooltip = c("Acumulado","grupo"))
pp1_ne<-plotly_build(p1_ne)
style(pp1_ne)%>%layout(yaxis=list(tickfont = list(size=9)))
```

### ACUMULADO PRIMAS NETAS

```{r}
p2_ne<-ggplot(data = primas_ne_mes,aes(x=mes,y=Acumulado,fill=grupo)) + 
  geom_bar(stat="identity", width=0.5,colour="black")+ 
  xlab("") + ylab("")+ scale_fill_brewer(palette="Set3")+guides(fill=FALSE)
  #ggplotly(p2_ne,tooltip = c("Acumulado","grupo"))
pp2_ne<-plotly_build(p2_ne)
style(pp2_ne)%>%layout(yaxis=list(tickfont = list(size=9)))
```

VENTAS DIARIAS
=======================================================================

### ACUMULADO PRIMAS VENDIDAS DIARIAS
```{r}
p_dia<-ggplot(data_plot_dia, aes(x=dia, y=Acum_Monto, colour=mes,group=mes)) +
  geom_line() + scale_colour_brewer(palette="Set1") +
  labs(colour="Mes")+ xlab("")+ylab("")+ scale_y_continuous(labels = dollar)+ geom_point(shape=20, size=2, fill="white") 
#ggplotly(p_dia,tooltip = c("dia","Acumulado"))
pp_dia<-plotly_build(p_dia)
style(pp_dia)%>%layout(yaxis=list(tickfont = list(size=9)))
```

CUMPLIMIENTO PRODUCTOS
=======================================================================
Row
-----------------------------------------------------------------------
### TOTAL
```{r}
gauge(round(100*sum(ventas_producto$Real)/sum(ventas_producto$Presupuesto),2), min = 0, max = 100, symbol = '%', gaugeSectors(
  success = c(80, 100), warning = c(40, 79), danger = c(0, 39),colors = c("green", "yellow", "red")
))
```

### TOTAL
```{r}
vt<-round(100*sum(ventas_producto$Real)/sum(ventas_producto$Presupuesto),2)
valueBox(format(sum(ventas_producto$Real), decimal.mark=",",
big.mark=".",small.mark="."), icon = "fa-dollar",color = ifelse(vt>= 80, "lightgreen", ifelse(vt>=40 & vt<80,"khaki","mistyrose")))
```

### AKI SEGURO
```{r}
 gauge(ventas_producto[1,4], min = 0, max = 100, symbol = '%', gaugeSectors(
  success = c(80, 100), warning = c(40, 79), danger = c(0, 39),colors = c("green", "yellow", "red")
))

```

### AKI SEGURO
```{r}
valueBox(format(ventas_producto[1,2],decimal.mark = ",",big.mark = ".",small.mark = "."), icon = "fa-dollar",color = ifelse(ventas_producto[1,4]>= 80, "lightgreen", ifelse(ventas_producto[1,4]>=40 & ventas_producto[1,4]<80,"khaki","mistyrose")))
```

Row
-----------------------------------------------------------------------
### GARANTÍA TOTAL
```{r}
gauge(ventas_producto[2,4], min = 0, max = 100, symbol = '%', gaugeSectors(
  success = c(80, 100), warning = c(40, 79), danger = c(0, 39),colors = c("green", "yellow", "red")
))
```

### GARANTÍA TOTAL
```{r}
valueBox(format(ventas_producto[2,2],decimal.mark = ",",big.mark = ".",small.mark = "."), icon = "fa-dollar",color=ifelse(ventas_producto[2,4]>= 80,"lightgreen", ifelse(ventas_producto[2,4]>=40 & ventas_producto[2,4]<80,"khaki","mistyrose")))
```

### RUEDA PROTEGIDO
```{r}
gauge(ventas_producto[3,4], min = 0, max = 100, symbol = '%', gaugeSectors(
  success = c(80, 100), warning = c(40, 79), danger = c(0, 39),colors = c("green", "yellow", "red")
))
```

### RUEDA PROTEGIDO
```{r}
valueBox(format(ventas_producto[3,2],decimal.mark = ",",big.mark = ".",small.mark = "."), icon = "fa-dollar",color=ifelse(ventas_producto[3,4]>= 80,"lightgreen", ifelse(ventas_producto[3,4]>=40 & ventas_producto[3,4]<80,"khaki","mistyrose")))
```

Row
-----------------------------------------------------------------------
### RUEDA SEGURO
```{r}
gauge(ventas_producto[4,4], min = 0, max = 100, symbol = '%', gaugeSectors(
 success = c(80, 100), warning = c(40, 79), danger = c(0, 39),colors = c("green", "yellow", "red")
))
```

### RUEDA SEGURO
```{r}
valueBox(format(ventas_producto[4,2],decimal.mark = ",",big.mark = ".",small.mark = "."), icon = "fa-dollar",color=ifelse(ventas_producto[4,4]>= 80, "lightgreen", ifelse(ventas_producto[4,4]>=40 & ventas_producto[4,4]<80,"khaki","mistyrose")))
```

### SALUD SEGURA
```{r}
gauge(ventas_producto[5,4], min = 0, max = 100, symbol = '%', gaugeSectors(
 success = c(80, 100), warning = c(40, 79), danger = c(0, 39),colors = c("green", "yellow", "red")
))
```

### SALUD SEGURA
```{r}
valueBox(format(ventas_producto[5,2],decimal.mark = ",",big.mark = ".",small.mark = "."), icon = "fa-dollar",color=ifelse(ventas_producto[5,4]>= 80, "lightgreen", ifelse(ventas_producto[5,4]>=40 & ventas_producto[5,4]<80,"khaki","mistyrose")))
```

Row
-----------------------------------------------------------------------
### SONRISA PROTEGIDA
```{r}
gauge(ventas_producto[6,4], min = 0, max = 100, symbol = '%', gaugeSectors(
  success = c(80, 100), warning = c(40, 79), danger = c(0, 39),colors = c("green", "yellow", "red")
))
```

### SONRISA PROTEGIDA
```{r}
valueBox(format(ventas_producto[6,2],decimal.mark = ",",big.mark = ".",small.mark = "."), icon = "fa-dollar",color=ifelse(ventas_producto[6,4]>= 80, "lightgreen", ifelse(ventas_producto[6,4]>=40 & ventas_producto[6,4]<80,"khaki","mistyrose")))
```

### SUPER PROTEGIDO
```{r}
gauge(ventas_producto[7,4], min = 0, max = 100, symbol = '%', gaugeSectors(
  success = c(80, 100), warning = c(40, 79), danger = c(0, 39),colors = c("green", "yellow", "red")
))
```

### SUPER PROTEGIDO
```{r}
valueBox(format(ventas_producto[7,2],decimal.mark = ",",big.mark = ".",small.mark = "."), icon = "fa-dollar",color=ifelse(ventas_producto[7,4]>= 80, "lightgreen", ifelse(ventas_producto[7,4]>=40 & ventas_producto[7,4]<80,"khaki","mistyrose")))
```

DETALLE VENTAS
=======================================================================
```{r}
DT::datatable(TABLA_GEN_DASH, options = list(
  bPaginate = FALSE
))
#knitr::kable(TABLA_GEN_DASH)
```
