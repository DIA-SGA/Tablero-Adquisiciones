rm(list = ls())
names(tablafinalEP)

view(tabla.para.indicadores)

#E)Traigo la tabla final
library(R.utils)
#importo base SCO
tablafinalEP<-loadObject("resultados/base_tabladinamicaEP.Rbin")
#importo base TRATAMIENTOS
tabla.tramitaciones.doc<-loadObject("resultados/tabla.tramitaciones.doc.Rbin")

View(tablafinalEP)

## para unir tabla programas y hacer todo en una tabla ##
tablafinalEP$Acrónimo <-""
tabla.tramitaciones.doc$Secretaría<-""
tabla.tramitaciones.doc$`Programa Descripción`<-""
tabla.tramitaciones.doc$`Nro. Pliego`<-""
tabla.tramitaciones.doc$MontoSolic<-""

tabla_final_2<-tablafinalEP [,c("Estado Pliego","Acrónimo","Nombre pliego","NumExpediente","Repartición actual","Usuario actual", "Días desde la creación del proceso","dias_ultimo_pase", "Secretaría","Descripción Detallada","Nro. Pliego","MontoSolic")] 
View(tabla_final_2)

names(tabla_final_2)<-names(tabla.tramitaciones.doc)

## une comprar con financiemiento internacional 
tabla_completa<-rbind(tabla.tramitaciones.doc,tabla_final_2)
names(tabla_completa)
View(tabla_completa)

#cambio el nombre a las variables por uno mas entendible
names(tabla_completa)<-c("Estado Pliego",
                         "Acrónimo", 
                         "Nombre del proceso",
                         "Expediente",
                         "Repartición donde se encuentra",
                         "Usuario actual",
                         "Días desde el inicio",
                         "Días desde el último pase",
                         "Secretaría",
                         "Descripción del Programa",
                         "Nro. Pliego",
                         "Monto solicitado")
names(tabla_completa)

library(DT)
tabla_completa [,c(1:2,10,3,11,12,4:9)]%>% 
  datatable(extensions = 'Buttons', 
            options = list(initComplete = 
                             JS("function(settings, json){",
                                "$(this.api().table().header()).css({'background-color': '#303F9F', 'color': '#fff'});",
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
            class = 'cell-border stripe',
            callback = JS("return table;"), 
            rownames=F,
            escape = TRUE,
            style = "default", width = NULL, height = NULL, elementId = NULL,
            fillContainer = getOption("DT.fillContainer", NULL),
            autoHideNavigation = getOption("DT.autoHideNavigation", NULL),
            selection = c("multiple", "single", "none"), 
            plugins = NULL, editable  = FALSE)

class(tabla_completa$`Monto solicitado`)
tabla_completa2<-tabla_completa

summarise(tabla_completa)

View(tabla_completa2)

tabla_completa$`Monto solicitado` <- as.numeric(tabla_completa$`Monto solicitado`)
tabla_completa$`Monto solicitado` <- format(tabla_completa$`Monto solicitado`, big.mark = ".")
tabla_completa$`Monto solicitado` <- ifelse(gsub(" ","",tabla_completa$`Monto solicitado`)=="NA","",tabla_completa$`Monto solicitado`)

