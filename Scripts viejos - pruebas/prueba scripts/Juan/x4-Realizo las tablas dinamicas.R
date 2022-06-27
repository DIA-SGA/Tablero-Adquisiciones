#11-genero la tabla final para el tablero
#A)Traigo la tabla final
library(R.utils)
tablafinalEP<-loadObject("resultados/base_tabladinamicaEP.Rbin")
nrow(tablafinalEP)

#B)controlo q no se repitan los numeros de pliego
table(tablafinalEP$`Nro. Pliego`)

#C)aclaro cuales son los q no tienen SCO en las variables de anio, nombre y nro de SCO
tablafinalEP$anioSCO<-ifelse(
  is.na(tablafinalEP$anioSCO),"sin SCO",tablafinalEP$anioSCO)
tablafinalEP$`Número solicitud` <-ifelse(
  is.na(tablafinalEP$`Número solicitud`),"sin SCO",tablafinalEP$`Número solicitud`)
tablafinalEP$`Nombre solicitud`<-ifelse(
  is.na(tablafinalEP$`Nombre solicitud`),"sin SCO",tablafinalEP$`Nombre solicitud`)

#D)hago homogeneo las secretarias
summary(as.factor(tablafinalEP$`Unidad Ejecutora Propuesta a nivel Secretaría`))

tablafinalEP$Secretaría<-"xx"
#SAS
tablafinalEP$Secretaría<-ifelse(
  tablafinalEP$`Unidad Ejecutora Propuesta a nivel Secretaría`=="Sec. De  Acceso a la Salud" |
    tablafinalEP$`Unidad Ejecutora Propuesta a nivel Secretaría`=="Sec. De Acceso a la Salud",
  "Secretaría de Acceso a la Salud", tablafinalEP$Secretaría)
#SCS
tablafinalEP$Secretaría<-ifelse(
  tablafinalEP$`Unidad Ejecutora Propuesta a nivel Secretaría`=="Sec. De Calidad en Salud" |
    tablafinalEP$`Unidad Ejecutora Propuesta a nivel Secretaría`=="Secretaría de Calidad en Salud",
  "Secretaría de Calidad en Salud", tablafinalEP$Secretaría)
#SES
tablafinalEP$Secretaría<-ifelse(
  tablafinalEP$`Unidad Ejecutora Propuesta a nivel Secretaría`=="Sec. De Equidad en Salud",
  "Secretaría de Equidad de Salud", tablafinalEP$Secretaría)
#SGA
tablafinalEP$Secretaría<-ifelse(
  tablafinalEP$`Unidad Ejecutora Propuesta a nivel Secretaría`=="Sub. de Gestión Administrativa",
  "Subsecretaría de Gestión Administrativa", tablafinalEP$Secretaría)
#UGA
tablafinalEP$Secretaría<-ifelse(
  tablafinalEP$`Unidad Ejecutora Propuesta a nivel Secretaría`=="Unidad Gabinete de Asesores",
  "Unidad Gabinete de Asesores", tablafinalEP$Secretaría)
#SPA, sin programa asociado
tablafinalEP$Secretaría<-ifelse(
  is.na(tablafinalEP$`Unidad Ejecutora Propuesta a nivel Secretaría`) &
    tablafinalEP$`Nombre solicitud`!="sin SCO", 
  "Sin programa asociado",tablafinalEP$Secretaría)
#SSV, sin Solicitud (SCO) vinculada
tablafinalEP$Secretaría<-ifelse(
  is.na(tablafinalEP$`Unidad Ejecutora Propuesta a nivel Secretaría`) &
    tablafinalEP$`Nombre solicitud`=="sin SCO", 
  "Sin SCO vinculada",tablafinalEP$Secretaría)

table(tablafinalEP$Secretaría)
table(tablafinalEP$Secretaría, 
      tablafinalEP$`Unidad Ejecutora Propuesta a nivel Secretaría`)

#E)cambio el nombre de la variable
tablafinalEP$`Programa Descripción`<-tablafinalEP$`Descripción Detallada`

#F)cambio las categorias de la variable etapas para unificar con la otra base
table(tablafinalEP$`Estado Pliego`)
tablafinalEP$`Estado Pliego`<-ifelse(tablafinalEP$`Estado Pliego`==names(table(tablafinalEP$`Estado Pliego`))[2], 
                                     "1-Inicial",tablafinalEP$`Estado Pliego`)
tablafinalEP$`Estado Pliego`<-ifelse(tablafinalEP$`Estado Pliego`==names(table(tablafinalEP$`Estado Pliego`))[3], 
                                     "2-Publicación o invitación",tablafinalEP$`Estado Pliego`)
tablafinalEP$`Estado Pliego`<-ifelse(tablafinalEP$`Estado Pliego`==names(table(tablafinalEP$`Estado Pliego`))[6], 
                                     "5-Preadjudicación",tablafinalEP$`Estado Pliego`)

#G)Edito los nombres de REPARTICION ACTUAL
tablafinalEP<-tablafinalEP %>% 
  mutate(`repartición actual`=gsub("#MS","",
                                   unlist(strsplit(`Repartición actual`," ")) [
                                     grepl("#",unlist(strsplit(`Repartición actual`," ")))])) %>% 
  mutate(reparticion.actual.agrup = case_when(
    `repartición actual`=="DGPFE" ~ "DGPFI",
    `repartición actual`=="DAFYP" ~ "DGPFI",
    `repartición actual`=="DGAJ" ~ "DAL",
    `repartición actual`=="DAL" ~ "DAL",
    `repartición actual`=="DS" ~ "DAL",
    `repartición actual`=="DGA" ~ "DGA",
    `repartición actual`=="DCYC" ~ "Compras",
    `repartición actual`=="SSGA" ~ "SGA",
    `repartición actual`=="SGA" ~ "SGA",
    `repartición actual`=="DTYC" ~ "Contabilidad",
    `repartición actual`=="DCYT" ~ "Contabilidad",
    `repartición actual`=="RCTD" ~ "Ejercito Argentino",
    `repartición actual`=="DD" ~ "Despacho",
    TRUE                      ~ "Programas")) 
table(tablafinalEP$reparticion.actual.agrup)

#G)corrijo las SCO q no tienen programatica
##CAMBIO DE SECRETARIA
tablafinalEP$Secretaría <-ifelse(
  tablafinalEP$Secretaría=="Sin programa asociado" & 
    tablafinalEP$`Nro. Pliego`=="80-0002-LPU21",
  "Secretaría de Acceso a la Salud",tablafinalEP$Secretaría)
tablafinalEP$Secretaría <-ifelse(
  tablafinalEP$Secretaría=="Sin programa asociado" & 
    tablafinalEP$`Nro. Pliego`=="80-0011-LPU21",
  "Secretaría de Acceso a la Salud",tablafinalEP$Secretaría)
tablafinalEP$Secretaría <-ifelse(
  tablafinalEP$Secretaría=="Sin programa asociado" & 
    tablafinalEP$`Nro. Pliego`=="80-0029-CDI21",
  "Secretaría de Acceso a la Salud",tablafinalEP$Secretaría)
tablafinalEP$Secretaría <-ifelse(
  tablafinalEP$Secretaría=="Sin programa asociado" & 
    tablafinalEP$`Nro. Pliego`=="80-0035-CDI21", 
  "Secretaría de Acceso a la Salud",tablafinalEP$Secretaría)
tablafinalEP$Secretaría <-ifelse(
  tablafinalEP$Secretaría=="Sin programa asociado" & 
    tablafinalEP$`Nro. Pliego`=="80-0036-CDI21", 
  "Secretaría de Acceso a la Salud",tablafinalEP$Secretaría)
tablafinalEP$Secretaría <-ifelse(
  tablafinalEP$Secretaría=="Sin programa asociado" & 
    tablafinalEP$`Nro. Pliego`=="80-0038-CDI21", 
  "Secretaría de Acceso a la Salud",tablafinalEP$Secretaría)
tablafinalEP$Secretaría <-ifelse(
  tablafinalEP$Secretaría=="Sin programa asociado" & 
    tablafinalEP$`Nro. Pliego`=="80-0008-LPR21",
  "Secretaría de Acceso a la Salud",tablafinalEP$Secretaría)
tablafinalEP$Secretaría <-ifelse(
  tablafinalEP$Secretaría=="Sin prorama asociado" & 
    tablafinalEP$`Nro. Pliego`=="80-0003-LPR21",
  "Secretaría de Calidad en Salud",tablafinalEP$Secretaría)
tablafinalEP$Secretaría <-ifelse(
  tablafinalEP$Secretaría=="Sin programa asociado" & 
    tablafinalEP$`Nro. Pliego`=="80-0064-CDI20",
  "Secretaría de Calidad en Salud",tablafinalEP$Secretaría)
tablafinalEP$Secretaría <-ifelse(
  tablafinalEP$Secretaría=="Sin programa asociado" & 
    tablafinalEP$`Nro. Pliego`=="80-0062-CDI20",
  "Subsecretaría de Gestión Administrativa",tablafinalEP$Secretaría)
tablafinalEP$Secretaría <-ifelse(
  tablafinalEP$Secretaría=="Sin programa asociado" & 
    tablafinalEP$`Nro. Pliego`=="80-0008-CDI21",
  "Unidad Gabinete de Asesores",tablafinalEP$Secretaría)
tablafinalEP$Secretaría <-ifelse(
  tablafinalEP$Secretaría=="Sin programa asociado" & 
    tablafinalEP$`Nro. Pliego`=="80-0019-CDI21",
  "Unidad Gabinete de Asesores",tablafinalEP$Secretaría)
table(tablafinalEP$Secretaría)

##CAMBIO DE LA DESCRIPCION,  a los q no la tenian
tablafinalEP$`Programa Descripción` <-ifelse(
  tablafinalEP$Secretaría=="Sin programa asociado" & 
    tablafinalEP$`Nro. Pliego`=="80-0002-LPU21",
  "Asistencia, Prevención, Vigilancia e Investigación en VIH e Infecciones de Transmisión Sexual",
  tablafinalEP$`Programa Descripción`)
tablafinalEP$`Programa Descripción` <-ifelse(
  tablafinalEP$Secretaría=="Sin programa asociado" & 
    tablafinalEP$`Nro. Pliego`=="80-0011-LPU21",
  "Asistencia al Paciente Trasplantado", tablafinalEP$`Programa Descripción`)
tablafinalEP$`Programa Descripción` <-ifelse(
  tablafinalEP$Secretaría=="Sin programa asociado" & 
    tablafinalEP$`Nro. Pliego`=="80-0029-CDI21",
  "Asistencia al Paciente Trasplantado", tablafinalEP$`Programa Descripción`)
tablafinalEP$`Programa Descripción` <-ifelse(
  tablafinalEP$Secretaría=="Sin programa asociado" & 
    tablafinalEP$`Nro. Pliego`=="80-0035-CDI21", 
  "Asistencia al Paciente Trasplantado", tablafinalEP$`Programa Descripción`)
tablafinalEP$`Programa Descripción` <-ifelse(
  tablafinalEP$Secretaría=="Sin programa asociado" & 
    tablafinalEP$`Nro. Pliego`=="80-0036-CDI21", 
  "Asistencia al Paciente Trasplantado", tablafinalEP$`Programa Descripción`)
tablafinalEP$`Programa Descripción` <-ifelse(
  tablafinalEP$Secretaría=="Sin programa asociado" & 
    tablafinalEP$`Nro. Pliego`=="80-0038-CDI21", 
  "Vigilancia, Prevención y Control de Enfermedades Zoonóticas", tablafinalEP$`Programa Descripción`)
tablafinalEP$`Programa Descripción` <-ifelse(
  tablafinalEP$Secretaría=="Sin programa asociado" & 
    tablafinalEP$`Nro. Pliego`=="80-0008-LPR21",
  "Promoción y Atención Primaria en Salud Bucodental", tablafinalEP$`Programa Descripción`)
tablafinalEP$`Programa Descripción` <-ifelse(
  tablafinalEP$Secretaría=="Sin programa asociado" & 
    tablafinalEP$`Nro. Pliego`=="80-0003-LPR21",
  "Cobertura de Emergencias Sanitaria", tablafinalEP$`Programa Descripción`)
tablafinalEP$`Programa Descripción` <-ifelse(
  tablafinalEP$Secretaría=="Sin programa asociado" & 
    tablafinalEP$`Nro. Pliego`=="80-0064-CDI20",
  "Cobertura de Emergencias Sanitaria", tablafinalEP$`Programa Descripción`)
tablafinalEP$`Programa Descripción` <-ifelse(
  tablafinalEP$Secretaría=="Sin programa asociado" & 
    tablafinalEP$`Nro. Pliego`=="80-0062-CDI20",
  "Actividades de Apoyo Administrativo", tablafinalEP$`Programa Descripción`)
tablafinalEP$`Programa Descripción` <-ifelse(
  tablafinalEP$Secretaría=="Sin programa asociado" & 
    tablafinalEP$`Nro. Pliego`=="80-0008-CDI21",
  "Conducción y Monitoreo", tablafinalEP$`Programa Descripción`)
tablafinalEP$`Programa Descripción` <-ifelse(
  tablafinalEP$Secretaría=="Sin programa asociado" & 
    tablafinalEP$`Nro. Pliego`=="80-0019-CDI21",
  "Coordinación Relaciones Institucionales", tablafinalEP$`Programa Descripción`)

#F)eliminamos archivos COVID o errores de tableau
tablafinalEP <- tablafinalEP %>% 
  filter(`Nombre solicitud`!=
           "ADQUISICIÓN DE EQUIPAMIENTO PARA LOS MÓVILES DE CLÍNICA MÉDICA.") %>% 
  filter(`Nombre solicitud`!=
           "INSUMOS PARA MÓVIL DE DIAGNÓSTICO POR IMÁGENES IBW 273.") %>% 
  filter(`Nombre solicitud`!=
           "ADQUISICIÓN DE EQUIPOS PARA MÓVIL DE LABORATORIO DE ANÁLISIS CLÍNICOS IAA821") %>% 
  filter(`Nombre solicitud`!="Adquisición de nuevo servicio de internet") %>% 
  filter(`Nro. Pliego` !="80-0069-CDI20") %>% 
  filter(`Nro. Pliego` !="80-0072-CDI20") %>% 
  filter(NumExpediente!="EX-2020-64448862-APN-DCYC#MS") %>% 
  filter(NumExpediente!="EX-2020-28491911-APN-DCYCMS#MSYDS") %>% 
  filter(NumExpediente!="EX-2020-34821230-APN-DCYC#MS") %>%
  filter(NumExpediente!="EX-2020-71918015-APN-DCYC#MS") %>%
  filter(NumExpediente!="EX-2020-89162011-APN-DCYC#MS") %>%
  filter(NumExpediente!="EX-2021-04953618-APN-DCYC#MS") %>% 
  filter(NumExpediente!="EX-2021-03379655-APN-DGPFE#MS")
  

nrow(tablafinalEP)
#81 84 86 85 93 96 91 93

### GUARDO LA BASE
write.csv2(tablafinalEP, "resultados/tablafinal.csv")
saveObject(tablafinalEP,"resultados/base_tabladinamicaEP.Rbin")

#11-SALIDA EN HTML

##A)Seleccion de variables y filtro de la secretaria
#(i)Tabla de Secretaria SAS
names(tablafinalEP)
seleccion.vs<-  c("Estado Pliego",
                  "Programa Descripción", 
                  "Nombre pliego",
                  "Nro. Pliego", "NumExpediente", "anioEXP", "anioSCO", 
                     #         `Nombre solicitud`,
                  "MontoSolic",
                  "Repartición actual", "Usuario actual",
                  "Días desde autorización",
                  "dias_ultimo_pase"
                  #,MAXdiasdesdeAutorizacion
) 
seleccion.vs

library(dplyr)
tablaSASep<-tablafinalEP %>% 
  filter(Secretaría=="Secretaría de Acceso a la Salud") %>% 
  select(seleccion.vs) %>% 
  arrange(`Estado Pliego`, `Programa Descripción`, `Nombre pliego`, dias_ultimo_pase)

nrow(tablafinalEP)
round(mean(as.numeric(tablafinalEP$`Días desde última modificación`), na.rm=T),0)
round(mean(as.numeric(tablafinalEP$`Días desde autorización`), na.rm=T),0)
table(tablafinalEP$`Repartición actual`)

tabla.reparti.actual<-tablafinalEP %>% 
  group_by(`Repartición actual`) %>% 
  summarise(`Cantidad de expedientes`=n())

tabla.programa<-tablafinalEP %>% 
  group_by(`Programa Descripción`) %>% 
  summarise(`Cantidad de expedientes`=n())

#cambio el nombre a las variables por uno mas entendible
names(tablaSASep)<-c("Estado del proceso", 
                     "Descripción del programa","Nombre del proceso",
                     "Nro. Pliego", "Expediente",
                     "Año de inicio del expediente", 
                     "Año de autorizacion del proceso", 
                     "Monto solicitado",
                     "Repartición donde se encuentra",
                     "Usuario actual",
                     "Días desde la autorización",
                     "Días desde el último pase")

library(DT)
datatable(tablaSASep, 
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

#(ii)Tabla de Secretaria SCS
tablaSCSep<-tablafinalEP %>% 
  filter(Secretaría=="Secretaría de Calidad en Salud") %>% 
  select(seleccion.vs) %>% 
  arrange(`Estado Pliego`, `Programa Descripción`, `Nombre pliego`, dias_ultimo_pase)
nrow(tablaSCSep)

#cambio el nombre a las variables por uno mas entendible
names(tablaSCSep)<-names(tablaSASep)

datatable(tablaSCSep, 
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

#(iii)Tabla de Secretaria Equidad
tablaSESep<-tablafinalEP %>% 
  filter(Secretaría=="Secretaría de Equidad de Salud") %>% 
  select(seleccion.vs) %>% 
arrange(`Estado Pliego`, `Programa Descripción`, `Nombre pliego`, dias_ultimo_pase)
nrow(tablaSESep)

#cambio el nombre a las variables por uno mas entendible
names(tablaSESep)<-names(tablaSASep)

datatable(tablaSESep, 
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

#(iv)Tabla de Secretaria SSGA
tablaSSGAep<-tablafinalEP %>% 
  filter(Secretaría=="Subsecretaría de Gestión Administrativa") %>% 
  select(seleccion.vs) %>% 
arrange(`Estado Pliego`, `Programa Descripción`, `Nombre pliego`, dias_ultimo_pase)
nrow(tablaSSGAep)

#cambio el nombre a las variables por uno mas entendible
names(tablaSSGAep)<-names(tablaSASep)

datatable(tablaSSGAep, 
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

#(v)Tabla de Sin programa asignado
tablaSPAep<-tablafinalEP %>% 
  filter(Secretaría=="Sin programa asociado") %>% 
  select(seleccion.vs) %>% 
arrange(`Estado Pliego`, `Programa Descripción`, `Nombre pliego`, dias_ultimo_pase)
nrow(tablaSPAep)

#cambio el nombre a las variables por uno mas entendible
names(tablaSPAep)<-names(tablaSASep)

datatable(tablaSPAep, 
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

#(vi)Tabla de Sin SCO vinculada
tablaSSVep<-tablafinalEP %>% 
  filter(Secretaría=="Sin SCO vinculada") %>% 
  select(seleccion.vs) %>% 
arrange(`Estado Pliego`, `Programa Descripción`, `Nombre pliego`, dias_ultimo_pase)
nrow(tablaSSVep)

#cambio el nombre a las variables por uno mas entendible
names(tablaSSVep)<-names(tablaSASep)

datatable(tablaSSVep, 
          options = list(initComplete = 
                           JS("function(settings, json){",
                              "$(this.api().table().header()).css({'background-color': '#303F9F', 'color': '#fff'});",
                              "}"),
                         pageLength = 200, 
                         lengthMenu = c(10, 20, 50, 100),
                         autoWidth = TRUE),
          caption = "La tabla permite ordenar los datos, hacer filtros para cada variable y buscar un texto particular en toda la tabla (usando el casillero 'search')",
          filter = 'top',
          class = 'cell-border stripe',
          callback = JS("return table;"), 
          rownames=F,
          escape = TRUE,
          style = "default", width = NULL, height = NULL, elementId = NULL,
          fillContainer = getOption("DT.fillContainer", NULL),
          autoHideNavigation = getOption("DT.autoHideNavigation", NULL),
          selection = c("multiple", "single", "none"), extensions = list(),
          plugins = NULL, editable = FALSE)

