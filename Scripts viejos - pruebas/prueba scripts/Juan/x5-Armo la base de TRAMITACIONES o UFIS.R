#1-importo los archivos bajados de TABLEAU

#importo todos
library(readxl)
archivos<-list.files("bases/")
archivos

sco<- read_excel("bases/Hoja_Detalle_solicitudes_crosstab.xlsx")
sco.expaso<- read_excel("bases/Expedientes_Asociados_crosstab.xlsx")
sco.presup<- read_excel("bases/Información_presupuestaria_crosstab.xlsx")

procesos <- read_excel("bases/Hoja_Detalle_Procesos_Compra_crosstab.xlsx", 
                       col_types = c("text", "text", "text", "text", 
                                     "text", "text", "text", "text",
                                     "text", "text", "text", "text", 
                                     "numeric"))

expedientes<- read_excel("bases/Detalle_crosstab.xlsx")

#miro la dimension de cada uno
dim(sco) #621 631 645 701
dim(sco.expaso) #588 599 610 662
dim(sco.presup) #825 847 868 938
dim(procesos) #556 581 602 646
dim(expedientes) #1760 1781 1792 3203

#2-Junto las bases de las SCO
names(sco)
names(sco.expaso)
names(sco.presup)
sco2<-merge(sco, sco.expaso, by=c("Número solicitud", "Nombre solicitud"), 
            all = T)
dim(sco2)
sco3<-merge(sco2, sco.presup, by=c("Número solicitud", "Nombre solicitud"), 
            all = T)
dim(sco3)
#874 977 1049 786

#3-SELECCIONO LAS VARIABLES DE CADA BASE
#A)BASE DE SCO me quedo solo con vs de interes
library(dplyr)
sco4<-
  sco3 %>% 
  #filter(`Estado agrupado`=="Autorizada") %>% 
  select( `Número solicitud`, `Nombre solicitud`, `Estado agrupado`, 
          `Unidad Solicitante`,`Monto solicitud`, 
          `Nro. proceso`, `Estado proceso`,`Día de Fecha autorización`) %>% 
  as.data.frame()

#B)BASE DE PROCESOS me quedo solo con vs de interes
# y limpio la base de procesos, 
#descartando los q tienen proc ADJUDICADOS, DEJADOS SIN EFECTO O DESIERTOS
procesos2<-procesos %>% 
  select( `Nro. Pliego`, `Estado Pliego`, 
          `Número expediente electrónico`,
          `Día de Fecha creación proceso`) %>% 
  as.data.frame()

#C)FILTRO LA BASE DE EXPEDIENTES 
#(me quedo solo con vs de interes, PARA UFIS SE AGREAGRON NUEVAS VS)
#controlo q esten los GNE pedidos
#table(substr(expedientes$`Tipo de trámite`,1,8))
names(expedientes)
expedientes2<-expedientes %>% 
  select(Expediente, Descripción, `Estado expediente`, Estado,
         `Fecha de caratulación`, `Tipo de trámite`, `Sistema creador`,
          Organismo, Repartición, `Cant. de doc`,
         `Fecha de último pase`, `Usuario actual`,
         `Repartición actual`, `Fecha de último pase`, 
         `Motivo del último pase`,
         `Grupo de trámite`, 
         `Sistema creador`, Repartición, 
         `Sector caratulador`,`Usuario caratulador`) %>% 
  as.data.frame()

#4-MEZCLO LAS BASES
#A) MEZCLO LAS BASES DE PROCESOS Y EXPEDIENTES 

#(i)marco las bases
nrow(procesos2)
procesos2$baseprocesos2<-1:nrow(procesos2)
nrow(expedientes2)
expedientes2$baseexpedientes2<-1:nrow(expedientes2)

#(ii)hago homogenea las variables de mezcla
procesos2$NumExpediente <- procesos2$`Número expediente electrónico`
expedientes2$NumExpediente <- expedientes2$Expediente

#(iii)mezclo las bases conservando todos los Procesos
proc.ex <- merge(procesos2, expedientes2, by ="NumExpediente", all.y = T)
nrow(proc.ex) 
#1766 1793 1801 1803 3579 3157 3211

#B)MEZCLO LAS TRES BASES
#(i)marco las bases
nrow(proc.ex)
proc.ex$baseproc.ex<-paste("proc.ex",1:nrow(proc.ex),sep="")
nrow(sco4)
sco4$basesco.autorizados<-1:nrow(sco4)

#(ii)hago homogenea las variables de mezcla
sco4$PliegoProceso <- sco4$`Nro. proceso`
proc.ex$PliegoProceso <- proc.ex$`Nro. Pliego`

#(iii)mezclo las bases conservando todos los Procesos
sco.proc.ex <- merge(sco4, proc.ex, 
                     by ="PliegoProceso", all = T)
nrow(sco.proc.ex) 
#2547 2615 2648 2670 2679 2685 2687 4478 4064 4088 4154

#(iv)chequeo numSolicitud nuproceso y numpliego+EstadoProceso
table(sco.proc.ex$`Usuario actual`)
summary(as.factor(sco.proc.ex$`Usuario actual`))

#5-AGREGADO DE VARIABLES A CALCULAR
names(sco.proc.ex)
#A)Variable anio
sco.proc.ex$anioEXP<-substr(sco.proc.ex$Expediente,4,7) 
#(i)cuando anio esta vacio, lo completo con la fecha de inicio del SCO
library(stringi)
sco.proc.ex$anioSCO<-substr(sco.proc.ex$`Día de Fecha autorización`,
                            stri_length(sco.proc.ex$`Día de Fecha autorización`)-3,
                            stri_length(sco.proc.ex$`Día de Fecha autorización`))
#(ii)control de la variable anio
sco.proc.ex %>% 
  group_by(anioEXP) %>% 
  summarise(n()) 
#723 casos sin expediente y por lo tanto sin anio 805 809

sco.proc.ex %>% 
  group_by(anioSCO) %>% 
  summarise(n())
#1790 casos sin expediente y por lo tanto sin anio 3217 3230

#B)Variables de fecha
#cargo el dia de hoy
hoy <- as.Date(Sys.Date(), format="%Y/%m/%d")

sco.proc.ex$`Días desde autorización`<-hoy - 
  as.Date(sco.proc.ex$`Día de Fecha autorización`,format="%d/%m/%Y")

sco.proc.ex$`Días desde la creación del proceso`<-hoy - 
  as.Date(sco.proc.ex$`Día de Fecha creación proceso` , format="%d/%m/%Y")

sco.proc.ex$`Días desde última modificación`<-hoy - 
  as.Date(sco.proc.ex$`Fecha de último pase` , format="%d/%m/%Y")

#6-FILTROS
base.busqueda.new2 <-
  sco.proc.ex %>% 
  filter(Estado=="Abierto") %>% 
  filter(`Grupo de trámite`=="Compras" | 
         `Grupo de trámite`=="Compras y contrataciones") %>% 
  filter(`Usuario caratulador`=="CPGARCES" | #agrego los exp q no caratulo CAROLA
         Expediente=="EX-2021-9401126-APN-DD#MS" | 
         Expediente=="EX-2021-10947998-APN-DD#MS" |
         Expediente=="EX-2021-11982590-APN-DD#MS"|
         Expediente=="EX-2021-12815525-APN-DD#MS" |
         Expediente=="EX-2021-14047400-APN-DD#MS") %>% 
  filter(substr(`Usuario actual`,1,5)!="DADSE") %>% 
  filter(substr(`Usuario actual`,1,5)!="DGAGN") %>% 
  filter(substr(`Repartición actual`,1,4)!="DTYC") %>% 
  filter(as.Date(`Fecha de caratulación`, format="%d/%m/%y")>=
           as.Date("2020-01-01"))  %>% #a partir de 1/1/20 
  mutate(anio_caratulacion = substr(as.Date(`Fecha de caratulación`, 
                                            format="%d/%m/%y"),1,4)) %>% #Creo la variable anio_caratulacion 
  mutate(dias_ultimo_pase = hoy-
           as.Date(`Fecha de último pase`, format="%d/%m/%Y")) %>%  #Creo Dias desde ultimo pase
  select(NumExpediente, Descripción, `Estado expediente`, Estado,
         `Fecha de caratulación`, `Tipo de trámite`, `Sistema creador`,
         Repartición, `Cant. de doc`, `Usuario actual`,
         `Repartición actual`, `Fecha de último pase`, 
         `Motivo del último pase`, Organismo, anio_caratulacion,
         dias_ultimo_pase, `Usuario caratulador`)

base.busqueda.new2$dias_ultimo_pase

#View(table(base.busqueda.new$`Repartición`))
nrow(base.busqueda.new2) 
#79 68 98 96 90 104 112 117 127
base.busqueda.new2$dias_ultimo_pase

#7-ELIMINO DE LA BASE LOS QUE YA ESTAN EN EL TABLERO DE SCO

#A)importo la base de SCO y la marco 
tablafinalEP<-loadObject("resultados/base_tabladinamicaEP.Rbin")
tablafinalEP$base<-"tablafinal"

#B)mezclo las dos bases c/inlcusion forzosa de las tramitaciones
names(tablafinalEP)
names(base.busqueda.new2)
base.busqueda.mezclada<-merge(base.busqueda.new2, 
                              tablafinalEP[,c("NumExpediente","base")], 
                              by="NumExpediente", all.x = T)
nrow(base.busqueda.mezclada)

#C)elimino las que estan en ambos
summary(as.factor(base.busqueda.mezclada$base)) #todos NA, no pego ninguno
base.tram.limpia<-subset(base.busqueda.mezclada,
                         is.na(base.busqueda.mezclada$base))
nrow(base.tram.limpia)
#79 68 98 96 93 93 104 112 117 116 128 127

#D)guardo la base
write.csv2(base.tram.limpia, "resultados/base_tram_limpia")
library(R.utils)
saveObject(base.tram.limpia, "resultados/base_tram_limpia.Rbin")



