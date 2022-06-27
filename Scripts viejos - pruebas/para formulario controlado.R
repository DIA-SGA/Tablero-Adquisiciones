
library(tidyverse)
library(stringr)

library(R.utils)
#importo base SCO
tablafinalEP<-loadObject("resultados/base_tabladinamicaEP.Rbin")
#importo base TRATAMIENTOS
tabla.tramitaciones.doc<-loadObject("resultados/tabla.tramitaciones.doc.Rbin")

library(dplyr)
tabla.para.indicadores1<-tablafinalEP %>% 
  select(`Estado Pliego`, `reparticion.actual.agrup`,
         `Días desde autorización`, dias_ultimo_pase,MontoSolic)
tabla.tramitaciones.doc$MontoSolic<-0
tabla.para.indicadores2<-tabla.tramitaciones.doc %>% 
  select(`Etapa del proceso`, `Repartición actual`,
         `Dias desde el inicio`, `Días desde el último pase`,MontoSolic)

nombres<-c("Etapa del proceso", "reparticion.actual.agrup", 
           "Dias desde el inicio", "Días desde el último pase","Monto solicitado")
names(tabla.para.indicadores1)<-nombres
names(tabla.para.indicadores2)<-nombres
tabla.para.indicadores<-rbind(tabla.para.indicadores1, tabla.para.indicadores2)


# para unir tabla programas y hacer todo en una tabla ##
tablafinalEP$Acrónimo <-"Sin dato"
tabla.tramitaciones.doc$Secretaría<-"Sin dato"
tabla.tramitaciones.doc$`Programa Descripción`<-"Sin dato"
tabla.tramitaciones.doc$`Nro. Pliego`<-"Sin dato"
tabla.tramitaciones.doc$MontoSolic<-"Sin dato"

tabla.tramitaciones.doc <- tabla.tramitaciones.doc %>% 
  select(1,2,3,4,7,6,8,9,11,12,13,10)

tabla_final_2<-tablafinalEP [,c("Estado Pliego","Acrónimo","Nombre pliego","NumExpediente","repartición actual","Usuario actual", "Días desde la creación del proceso","dias_ultimo_pase", "Secretaría","Descripción Detallada","Nro. Pliego","MontoSolic")] 
names(tabla_final_2)<-names(tabla.tramitaciones.doc)

## une comprar con financiemiento internacional 
tabla_completa<-rbind(tabla.tramitaciones.doc,tabla_final_2)

#cambio el nombre a las variables por uno mas entendible
# names(tabla_completa)<-c("Estado Pliego",
#                          "Acrónimo", 
#                          "Nombre del proceso",
#                          "Expediente",
#                          "Repartición donde se encuentra",
#                          "Usuario actual",
#                          "Días desde el inicio",
#                          "Días desde el último pase",
#                          "Secretaría",
#                          "Descripción del Programa",
#                          "Nro. Pliego",
#                          "Monto solicitado")

names(tabla_completa)<-c("Estado del proceso",
                         "Acrónimo", 
                         "Nombre del proceso",
                         "Expediente",
                         "Repartición donde se encuentra",
                         "Usuario actual",
                         "Días desde el inicio",
                         "Días desde el último pase",
                         "Secretaría",
                         "Descripción del programa",
                         "Nro. Pliego",
                         "Monto solicitado")

tabla_completa$`Monto solicitado` <- as.numeric(tabla_completa$`Monto solicitado`)
tabla_completa$`Monto solicitado` <- ifelse(gsub(" ","",tabla_completa$`Monto solicitado`)=="NA","",tabla_completa$`Monto solicitado`)

fowmc<- read_excel("bases/BASES20211114/Formulario_FOWMC.xlsx")

# agregar los importes a los montos vaciós
# los importes están en dolares y pesos traer todo a pesos a tc banco nacion del día 
# traer secretaría y dirección, pero arrancar desde dirección, si es nulo dirección el primero que este
# 
# (fowmc)
# 
# fowmc %>% 
#   anti_join(tabla_completa, by=c("Expediente")) %>%
#   filter(Expediente!="NA") %>%
#   select(Expediente,`Importe Total`)
# 
# dolar<-105.6

fowmc$Dirección<-ifelse(fowmc$Dirección=="N/A",
                                     NA,fowmc$Dirección)
fowmc$Dirección<-ifelse(fowmc$Dirección=="S/D",
                                     NA,fowmc$Dirección)

fowmc$`Dirección General`<-ifelse(fowmc$`Dirección General`=="N/A",
                                               NA,fowmc$`Dirección General`)
fowmc$`Dirección General`<-ifelse(fowmc$`Dirección General`=="S/D",
                                               NA,fowmc$`Dirección General`)

fowmc$Secretaria<-ifelse(fowmc$Secretaria=="N/A",
                                      NA,fowmc$Secretaria)
fowmc$Secretaria<-ifelse(fowmc$Secretaria=="S/D",
                                      NA,fowmc$Secretaria)

fowmc$Subsecretaria<-ifelse(fowmc$Subsecretaria=="N/A",
                                         NA,fowmc$Subsecretaria)
fowmc$Subsecretaria<-ifelse(fowmc$Subsecretaria=="S/D",
                                         NA,fowmc$Subsecretaria)

fowmc<-fowmc %>%
  rename(Expediente=`Número de expediente`)%>%
  select(4,5,6,7,10,13,14) %>%
  mutate(Expediente=str_replace(Expediente," ","")) %>% 
  mutate(`Importe Total`=round(`Importe Total`)) %>%
  mutate(importe_final=ifelse(`Importe total moneda`=="USD",`Importe Total`*dolar,`Importe Total`)) %>%
  mutate(importe_final=round(importe_final)) %>%
  #filter(!is.na(c(Expediente))) %>% ## necesito filtrar si es nulo en todas las variables o nulo a lo largo de todas las filas
  mutate(Dirección_Final=coalesce(Dirección,`Dirección General`,Subsecretaria, Secretaria))

tabla_completa_bis<- tabla_completa %>% 
  left_join(fowmc, by=c("Expediente")) %>%
  mutate(`Monto solicitado`=ifelse
         (is.na(`Monto solicitado`),
           importe_final,`Monto solicitado`)) 

view(tabla_completa_bis)
names(tabla_completa_bis)<-c("Estado del proceso",
                             "Acrónimo",
                             "Nombre del proceso",
                             "Expediente",
                             "Repartición donde se encuentra",
                             "Usuario actual",
                             "Días desde el inicio",
                             "Días desde el último pase",
                             "Secretaría",
                             "Descripción del programa",
                             "Nro. Pliego",
                             "Monto solicitado")

tabla_completa_bis$Secretaría<-ifelse(tabla_completa$Secretaría==
                                        "Subsecretaría de Gestión Administrativa","Secretaría de Gestión Administrativa",
                                      tabla_completa_bis$Secretaría)

names(tabla_completa_bis)
library(DT)
tabla_completa_bis [,c(1,3,11,12,4,5,6,7,8)] %>%
  arrange(desc(`Días desde el último pase`))%>%
  datatable(extensions = 'Buttons', 
            options = list(initComplete = 
                             JS("function(settings, json){",                     "$(this.api().table().header()).css({'background-color': '#303F9F', 'color': '#fff'});",
                                "}"),
                           scrollY =F,
                           dom = 'Blfrtip',
                           buttons = c('csv', 'excel', 'pdf'),
                           pageLength = 200, 
                           lengthMenu = list(c(100, 200, 500, -1),
                                             c(100, 200, 500,"Todo")),
                           autoWidth = TRUE),
            caption = "Se puede exportar a formatos csv, pdf y xls, conservando los filtros aplicados",
            filter = 'top',
            #order("Días desde el último pase"),
            class = 'cell-border stripe',
            callback = JS("return table;"), 
            rownames=F,
            escape = TRUE,
            style = "default", width = NULL, height = NULL, elementId = NULL,
            fillContainer = getOption("DT.fillContainer", NULL),
            autoHideNavigation = getOption("DT.autoHideNavigation", NULL),
            selection = c("multiple", "single", "none"), 
            plugins = NULL, editable  = FALSE) %>%
  formatStyle("Monto solicitado",'text-align'="right") 


