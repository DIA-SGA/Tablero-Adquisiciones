#8-SALIDA EN HTML
library(R.utils)
base.tram.limpia<-loadObject("resultados/base_tram_limpia.Rbin")
nrow(base.tram.limpia)

#creo la variable Etapa en la cual se encuentra el tramite
base.tram.limpia$etapa.tramite<-"3-Unidad requirente"
base.tram.limpia$etapa.tramite<-ifelse(
  substr(base.tram.limpia$`Repartición actual`,1,
         regexpr("#",base.tram.limpia$`Repartición actual`)-1)=="DGPYCP",
  "1-Dictamen de presupuesto",base.tram.limpia$etapa.tramite)
base.tram.limpia$etapa.tramite<-ifelse(
  substr(base.tram.limpia$`Repartición actual`,1,
         regexpr("#",base.tram.limpia$`Repartición actual`)-1)=="DCYC",
  "2-Unidad de adquisiciones",base.tram.limpia$etapa.tramite)
base.tram.limpia$etapa.tramite<-ifelse(
  substr(base.tram.limpia$`Repartición actual`,1,
         regexpr("#",base.tram.limpia$`Repartición actual`)-1)=="DGPFE",
  "2-Unidad de adquisiciones",base.tram.limpia$etapa.tramite)
base.tram.limpia$etapa.tramite<-ifelse(
  substr(base.tram.limpia$`Repartición actual`,1,
         regexpr("#",base.tram.limpia$`Repartición actual`)-1)=="DAL",
  "4-Dictamen de aprobación",base.tram.limpia$etapa.tramite)

saveObject(base.tram.limpia,"resultados/base_tram_limpia.Rbin")
#########################
#    TABLA DINAMICA     #
#########################
library(rpivotTable)
library(dplyr)
##A)Seleccion de variables 
names(base.tram.limpia)

tabla.tramitaciones<-base.tram.limpia %>% 
  select(`Descripción`, NumExpediente, `Estado expediente`,
         `Tipo de trámite`, `Cant. de doc`, dias_ultimo_pase,
         `Usuario actual`,`Repartición actual`, etapa.tramite) %>% 
  mutate(Tipo.tramite=paste(substr(`Tipo de trámite`,1,4),
                            substr(`Tipo de trámite`,7,9),
                            sep="")
  )

names(tabla.tramitaciones)<-c("Descripción", "Expediente", 
                              "Estado del expediente", 
                              "Tipo de trámite", "Cantidad de documentos", 
                              "Días desde el último pase",  
                              "Usuario actual", "Repartición actual", 
                              "Etapa de tramitación","Código de trámite")  

library(DT)
datatable(tabla.tramitaciones, 
          options = list(initComplete = 
                           JS("function(settings, json){",
                              "$(this.api().table().header()).css({'background-color': '#303F9F', 'color': '#fff'});",
                              "}"),
                         pageLength = 200, 
                         lengthMenu = c(100, 200, 500, 1000),
                         autoWidth = TRUE),
          caption = "La tabla permite ordenar los datos, hacer filtros para cada variable y buscar un texto particular en toda la tabla (usando el casillero 'search')",
          class = 'cell-border stripe',
          callback = JS("return table;"), 
          rownames=F,
          escape = TRUE,
          style = "default", width = NULL, height = NULL, elementId = NULL,
          fillContainer = getOption("DT.fillContainer", NULL),
          autoHideNavigation = getOption("DT.autoHideNavigation", NULL),
          selection = c("multiple", "single", "none"), extensions = list(),
          plugins = NULL, editable = FALSE)


