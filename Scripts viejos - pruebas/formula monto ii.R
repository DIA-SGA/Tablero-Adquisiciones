

tablafinalEP<-loadObject("resultados/base_tabladinamicaEP.Rbin")
#importo base TRATAMIENTOS
tabla.tramitaciones.doc<-loadObject("resultados/tabla.tramitaciones.doc.Rbin")
library(dplyr)
tabla.para.indicadores1<-tablafinalEP %>% 
  select(`Estado Pliego`, reparticion.actual.agrup,
         `Días desde autorización`, dias_ultimo_pase,MontoSolic)
tabla.tramitaciones.doc$MontoSolic<-0
tabla.para.indicadores2<-tabla.tramitaciones.doc %>% 
  select(`Etapa del proceso`, `Repartición actual`,
         `Dias desde el inicio`, `Días desde el último pase`,MontoSolic)

nombres<-c("Etapa del proceso", "Repartición actual", 
           "Dias desde el inicio", "Días desde el último pase","Monto solicitado")
names(tabla.para.indicadores1)<-nombres
names(tabla.para.indicadores2)<-nombres
tabla.para.indicadores<-rbind(tabla.para.indicadores1, tabla.para.indicadores2)

View(tabla.para.indicadores)

paste("M ",
round(sum(as.numeric(tabla.para.indicadores$`Monto solicitado`), na.rm=T)/1000000,0),sep="")
         