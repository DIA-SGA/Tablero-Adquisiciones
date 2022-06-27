#E)Traigo la tabla final
library(R.utils)
#importo base SCO
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

View(tabla.para.indicadores1)
nombres<-c("Etapa del proceso", "reparticion.actual.agrup", 
           "Dias desde el inicio", "Días desde el último pase","Monto solicitado")
names(tabla.para.indicadores1)<-nombres
names(tabla.para.indicadores2)<-nombres
tabla.para.indicadores<-rbind(tabla.para.indicadores1, tabla.para.indicadores2)

View(tabla.para.indicadores)

# para unir tabla programas y hacer todo en una tabla ##
tablafinalEP$Acrónimo <-"Sin dato"
tabla.tramitaciones.doc$Secretaría<-"Sin dato"
tabla.tramitaciones.doc$`Programa Descripción`<-"Sin dato"
tabla.tramitaciones.doc$`Nro. Pliego`<-"Sin dato"
tabla.tramitaciones.doc$MontoSolic<-"Sin dato"

tabla_final_2<-tablafinalEP [,c("Estado Pliego","Acrónimo","Nombre pliego","NumExpediente","reparticion.actual.agrup","Usuario actual", "Días desde la creación del proceso","dias_ultimo_pase", "Secretaría","Descripción Detallada","Nro. Pliego","MontoSolic")] 
names(tabla_final_2)<-names(tabla.tramitaciones.doc)
names(tabla_final_2)
names(tabla.tramitaciones.doc)

## une comprar con financiemiento internacional 
tabla_completa<-rbind(tabla.tramitaciones.doc,tabla_final_2)
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

library(DT)
tabla_completa [,c(1,3,11,12,4,5,6,7,8)] %>%
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


library(plotly)
library(dplyr)
#grafico por etapas
names(tabla.para.indicadores)

tabla.etapaproc<-tabla.para.indicadores %>% 
filter(`Etapa del proceso`=="5-Preadjudicación" | `Etapa del proceso`=="1-Inicial") %>%
    group_by(`Etapa del proceso`,reparticion.actual.agrup) %>% 
  summarise(`Cantidad de expedientes`=n())

tabla.etapaproc %>%
plot_ly(x = ~`Etapa del proceso`, y =~`Cantidad de expedientes`, color = ~reparticion.actual.agrup) %>%
add_bars() %>%
  layout(barmode = "stack") %>%
  layout(title = "N procesos en etapa inicial y preadjudicación según ubicación",
         font=list(family = "sans serif", size = 13),
         xaxis = list(title = ""),
         yaxis = list(title = ""))

tabla.etapaproc %>%
  plot_ly(x = ~`Etapa del proceso`, y =~`Cantidad de expedientes`, color = ~reparticion.actual.agrup) %>%
  add_bars() %>%
  layout(barmode = "stack") %>%
  layout(title = "N procesos en etapa inicial y preadjudicación según ubicación",
         font=list(family = "sans serif", size = 13),
         xaxis = list(title = ""),
         yaxis = list(title = ""),
       legend = list(font = list(size = 12))) %>% 
  layout(autosize = F, width = 400, height = 550)



# plot_ly(as.data.frame(tabla.etapaproc), 
#         y = ~`Cantidad de expedientes`, 
#         x = ~`Etapa del proceso`, 
#         type = 'bar', orientation = 'v',
#         marker = list(color = '#b52a0c',
#                       line = list(color = 'rgb(8,48,107)',
#                                   width = 1.5)))%>% 
#   layout(title = "Cantidad de Procesos según etapa",
#          font=list(family = "sans serif", size = 13, color = '#000000'),
#          xaxis = list(title = ""),
#          yaxis = list(title = ""))


# plot_ly(tabla.etapaproc, 
#         labels = ~`reparticion.actual.agrup`, 
#         values = ~`Cantidad de expedientes`, type = 'pie',
#         textposition = 'inside',
#         textinfo = 'percent') %>% 
#   layout(title = 'Cantidad de Procesos según ubicación actual', 
#          font=list(family = "sans serif", size = 13, color = '#000000'),
#          xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
#          yaxis = list(showgrid = F, zeroline = F, showticklabels = F),
#          legend = list(font = list(size = 12))) %>% 
#   layout(autosize = F, width = 550, height = 520)


