#A) importo la base de documentos vinculados a los expedientes
library(readxl)
doc.acronimos2020<-read_xlsx("bases/Documentos_vinculados_a_expedientes_crosstab2020.xlsx")
doc.acronimos2021<-read_xlsx("bases/Documentos_vinculados_a_expedientes_crosstab2021.xlsx")
nrow(doc.acronimos2020)
nrow(doc.acronimos2021)
names(doc.acronimos2020)
names(doc.acronimos2021)
doc.acronimos<-rbind(doc.acronimos2020, doc.acronimos2021[,1:22])
names(doc.acronimos)
nrow(doc.acronimos)


#B) hago una tabla con los ultimos documentos de c/expediente
##exceptuando PV, IF e IFGRA
library(dplyr)
ultimo.sin.pv.if.ifgra<-doc.acronimos %>%
  select(Expediente, `Orden`, `Número`,
         `Usuario asociador del documento`, `Usuario creador del documento`,
         `Fecha de creación del documento`, `Tipo de documento`, `Acrónimo`, 
         `Motivo del documento`) %>% 
  filter(Acrónimo=="CAREX" | Acrónimo=="PLIEG" | 
         #Acrónimo=="NOBAN" | 
         Acrónimo=="BOFIC" | Acrónimo=="PUCON" | 
         Acrónimo=="ACFO" | 
         #Acrónimo=="IFGRA" | Acrónimo=="NOBAN" | 
         Acrónimo=="NOTIF" | Acrónimo=="INFFC" | Acrónimo=="PDISP" |
         Acrónimo=="DICJU" | Acrónimo=="DI" |
         Acrónimo=="ORCOM" | Acrónimo=="CONTR") %>% 
  mutate(Etapa.del.proceso=case_when(
    Acrónimo=="CAREX" ~ "1-Inicial", Acrónimo=="PLIEG" ~ "1-Inicial",
    Acrónimo=="BOFIC" ~ "2-Publicación o invitación", 
    Acrónimo=="PUCON" ~ "2-Publicación o invitación",
    Acrónimo=="ACFO" ~ "3-Apertura", 
    Acrónimo=="IFGRA" ~ "4-Evaluación",
    Acrónimo=="NOTIF" ~ "5-Preadjudicación", Acrónimo=="INFFC" ~ "5-Preadjudicación",
    Acrónimo=="PDISP" ~ "5-Preadjudicación", Acrónimo=="DICJU" ~ "5-Preadjudicación",
    Acrónimo=="DI" ~ "6-Adjudicación", 
    Acrónimo=="ORCOM" ~ "7-Perfeccionamiento de contrato", 
    Acrónimo=="CONTR" ~ "7-Perfeccionamiento de contrato")) %>% 
group_by(Expediente) %>% 
  arrange(Orden) %>%
  slice(n()) %>%
  ungroup %>% 
  as.data.frame()
nrow(ultimo.sin.pv.if.ifgra)

tabla.ultimos.acronimos<-ultimo.sin.pv.if.ifgra %>% 
  group_by(Acrónimo) %>% 
  summarise(n())
nrow(tabla.ultimos.acronimos)

library(R.utils)
base.tramitaciones<-loadObject("resultados/base_tram_limpia.Rbin")
base.tramitaciones.doc<-merge(base.tramitaciones, ultimo.sin.pv.if.ifgra, 
                               by.y="Expediente", by.x = "NumExpediente",
                               all.x = T)

#AGREGO LA CATEGORIA EVALUACION QUE SE REGISTRA POR BUZON Y NO XIFGRA
base.tramitaciones.doc$Etapa.del.proceso<-ifelse(
  base.tramitaciones.doc$`Usuario actual`=="DGPFE#MS-PROCESOS_EVALUACION" &
  base.tramitaciones.doc$Acrónimo=="BOFIC",
  "4-Evaluación", base.tramitaciones.doc$Etapa.del.proceso)

nrow(base.tramitaciones.doc)
names(base.tramitaciones.doc)
table(base.tramitaciones.doc$Acrónimo, base.tramitaciones.doc$Etapa.del.proceso)

#acorto la variable reparticion actual
base.tramitaciones.doc[116,"Repartición actual"]<-"MS#MS - Ministerio de salud"
base.tramitaciones.doc$`Repartición actual`
base.tramitaciones.doc$reparticion.actual.corta<-gsub("#MS","",
    unlist(strsplit(base.tramitaciones.doc$`Repartición actual`," ")) [grepl(
      "#",unlist(strsplit(base.tramitaciones.doc$`Repartición actual`," ")))
      ]
    )
table(base.tramitaciones.doc$reparticion.actual.corta)

#G)agrupo los nombres de REPARTICION ACTUAL CORTA
base.tramitaciones.doc<-base.tramitaciones.doc %>% 
mutate(reparticion.actual.agrup = case_when(
  reparticion.actual.corta=="DGPFE" ~ "DGPFI",
  reparticion.actual.corta=="DAFYP" ~ "DGPFI",
  reparticion.actual.corta=="DGAJ" ~ "DAL",
  reparticion.actual.corta=="DAL" ~ "DAL",
  reparticion.actual.corta=="DS" ~ "DAL",
  reparticion.actual.corta=="DGA" ~ "DGA",
  reparticion.actual.corta=="DCYC" ~ "Compras",
  reparticion.actual.corta=="SSGA" ~ "SGA",
  reparticion.actual.corta=="SGA" ~ "SGA",
  reparticion.actual.corta=="DTYC" ~ "Contabilidad",
  reparticion.actual.corta=="DCYT" ~ "Contabilidad",
  reparticion.actual.corta=="RCTD" ~ "Ejercito Argentino",
  reparticion.actual.corta=="DD" ~ "Despacho",
    TRUE                      ~ "Programas")) 

###ELIMINO EXPEDIENTES OPS QUE YA ESTAN ADJUDICADOS
base.tramitaciones.doc <- base.tramitaciones.doc %>% 
  filter(NumExpediente!="EX-2021-43106725-APN-SGA#MS") %>% 
  filter(NumExpediente!="EX-2021-39683804-APN-SGA#MS") %>% 
  filter(NumExpediente!="EX-2021-34655890-APN-SGA#MS") %>% 
  #4nuevos del 2/8
  filter(NumExpediente!="EX-2020-54928563-APN-SSGA#MS") %>% 
  filter(NumExpediente!="EX-2021-20351697-APN-SSGA#MS") %>% 
  filter(NumExpediente!="EX-2021-18665214-APN-SSGA#MS") %>% 
  filter(NumExpediente!="EX-2020-91708821-APN-SSGA#MS") 



##A)Seleccion de variables Y SACO LA ETAPA 7
tabla.tramitaciones.doc<-base.tramitaciones.doc %>% 
  mutate(dias.desde.creacion=
           as.Date(Sys.Date(), format="%Y/%m/%d")-
           as.Date(base.tramitaciones.doc$`Fecha de creación del documento`,format="%d/%m/%Y")
  ) %>% select(Etapa.del.proceso, Acrónimo,
         `Descripción`, NumExpediente, #`Estado expediente`,
         #`Tipo de trámite`, 
         reparticion.actual.agrup, `Usuario actual`,
         #`Cant. de doc`, 
         dias.desde.creacion,
         dias_ultimo_pase #,etapa.tramite
         ) %>% 
  arrange(Etapa.del.proceso, Acrónimo, dias_ultimo_pase) %>% 
  filter(Etapa.del.proceso != "7-Perfeccionamiento de contrato") %>% 
  filter(Etapa.del.proceso != "6-Adjudicación")

nrow(tabla.tramitaciones.doc)
table(tabla.tramitaciones.doc$Etapa.del.proceso)
#cambio los nombres de las variables
names(tabla.tramitaciones.doc)<-c("Etapa del proceso","Acrónimo",
                                  "Descripción", "Expediente", 
                              #"Estado del expediente", 
                              #"Tipo de trámite", 
                              "Repartición actual", "Usuario actual", 
                              #"Cantidad de documentos", 
                              "Dias desde el inicio",
                              "Días desde el último pase"
                              #,"Etapa de tramitación","Código de trámite"
                              )  
#guardo la tabla
saveObject(tabla.tramitaciones.doc,"resultados/tabla.tramitaciones.doc.Rbin")
names(tabla.tramitaciones.doc)

#########################
#    TABLA DINAMICA     #
#########################

library(DT)
datatable(tabla.tramitaciones.doc, 
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

