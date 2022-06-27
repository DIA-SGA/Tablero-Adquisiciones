#1-importo los archivos bajados de TABLEAU

#importo todos
library(readxl)
archivos<-list.files("bases/")
archivos

sco<- read_excel("bases/Hoja_Detalle_solicitudes_crosstab.xlsx")
sco.expaso<- read_excel("bases/Expedientes_Asociados_crosstab.xlsx")
sco.presup<- read_excel("bases/Información_presupuestaria_crosstab.xlsx")

# sco$`Día de Fecha autorización`<-as.Date(sco$`Día de Fecha autorización`,origin = "1899-12-30")
# sco$`Día de Fecha creación`<-as.Date(sco$`Día de Fecha creación`,origin = "1899-12-30")

procesos <- read_excel("bases/Hoja_Detalle_Procesos_Compra_crosstab.xlsx", 
                       col_types = c("text", "text", "text", "text", 
                                     "text", "text", "text", "text",
                                     "text", "text", "text", "text", 
                                     "numeric"))

expedientes<- read_excel("bases/Detalle_crosstab.xlsx")

## para resolver formato fecha caratulación ultima modificación y utimo pase
## separo fecha en 3 y me quedo con para primer parte
expedientes$`Fecha de caratulación`<- word(expedientes$`Fecha de caratulación`, 1, sep = fixed(" "))
expedientes$`Fecha de última modificación`<- word(expedientes$`Fecha de última modificación`, 1, sep = fixed(" "))
expedientes$`Fecha de último pase`<- word(expedientes$`Fecha de último pase`, 1, sep = fixed(" "))

## paso la fecha a formato lubridate ##
expedientes$`Fecha de caratulación`<-ymd(expedientes$`Fecha de caratulación`)
expedientes$`Fecha de última modificación`<-ymd(expedientes$`Fecha de última modificación`)
expedientes$`Fecha de último pase`<-ymd(expedientes$`Fecha de último pase`)

## genero cada fecha en el formato que la levanta el script más adelante
expedientes <- expedientes %>%
  mutate(año_cara=year(`Fecha de caratulación`),
         mes_cara=month(`Fecha de caratulación`),
         dia_cara=day(`Fecha de caratulación`),
         año_modi=year(`Fecha de última modificación`),
         mes_modi=month(`Fecha de última modificación`),
         dia_modi=day(`Fecha de última modificación`),
         año_pas=year(`Fecha de último pase`),
         mes_pas=month(`Fecha de último pase`),
         dia_pas=day(`Fecha de último pase`),
         fecha_cara=paste(dia_cara,mes_cara,año_cara,sep="/"), 
         fecha_modi=paste(dia_modi,mes_modi,año_modi,sep="/"),
         fecha_pas=paste(dia_pas,mes_pas,año_pas,sep="/")) %>%
  select(1:4,61,62,7:44,63,46:51) %>%
  rename(`Fecha de caratulación`="fecha_cara") %>%
  rename(`Fecha de última modificación`="fecha_modi") %>%
  rename(`Fecha de último pase`="fecha_pas")

# procesos$`Día de Fecha creación proceso`<-as.Date(procesos$`Día de Fecha creación proceso`,origin = "1899-12-30")
# expedientes$`Fecha de caratulación`<-as.Date(expedientes$`Fecha de caratulación`,origin = "1899-12-30")
# expedientes$`Fecha de último pase`<-as.Date(expedientes$`Fecha de último pase`,origin = "1899-12-30")
# expedientes$`Fecha de última modificación`<-as.Date(expedientes$`Fecha de última modificación`,origin = "1899-12-30")

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
#procesos2$NumExpediente <- procesos2$`Número expediente electrónico`
#expedientes2$NumExpediente <- expedientes2$Expediente

###corrijo los numeros de expedientes q tienen uno o dos ceros menos 
library(stringr)
#separo la variable para tener el numero de expediente en un vector separado
arreglo<-str_split_fixed(expedientes2$Expediente, "-", n=Inf)
#agrego los ceros correspondiente a los expedientes q no tienen 8 caracteres
arreglo[,3]<-ifelse(stri_length(arreglo[,3])==6, 
                    paste("00",arreglo[,3],sep = ""),arreglo[,3])
arreglo[,3]<-ifelse(stri_length(arreglo[,3])==7,
                    paste("0",arreglo[,3],sep = ""),arreglo[,3])
#verifico q todos tengan 8 caracteres
stri_length(arreglo[,3])
#reconstruyo la variable y la pego de nuevo en la variable NumExpediente
expedientes2$NumExpediente <-paste(
  arreglo[,1],arreglo[,2],arreglo[,3],arreglo[,4],arreglo[,5],sep="-")

###corrijo los numeros de expedientes q tienen uno o dos ceros menos 
library(stringr)
#separo la variable para tener el numero de expediente en un vector separado
arreglo<-str_split_fixed(procesos2$`Número expediente electrónico`, "-", n=Inf)
#agrego los ceros correspondiente a los expedientes q no tienen 8 caracteres
arreglo[,3]<-ifelse(stri_length(arreglo[,3])==6, 
                    paste("00",arreglo[,3],sep = ""),arreglo[,3])
arreglo[,3]<-ifelse(stri_length(arreglo[,3])==7,
                    paste("0",arreglo[,3],sep = ""),arreglo[,3])
#verifico q todos tengan 8 caracteres
stri_length(arreglo[,3])
#reconstruyo la variable y la pego de nuevo en la variable NumExpediente
procesos2$NumExpediente <-paste(
  arreglo[,1],arreglo[,2],arreglo[,3],arreglo[,4],arreglo[,5],sep="-")

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
  filter(`Usuario caratulador`=="CPGARCES" | #agrego los exp q caratulo CAROLA 
         `Usuario caratulador`=="CDUPONT" |
         `Usuario caratulador`=="MNLOMBARDO" |                                            #agrego los exp q caratulo CDUPONT
         Expediente=="EX-2021-9401126-APN-DD#MS" | #agrego los exp q no caratulo CDUPONT ni CAROLA
         Expediente=="EX-2021-10947998-APN-DD#MS" |
         Expediente=="EX-2021-11982590-APN-DD#MS"|
         Expediente=="EX-2021-12815525-APN-DD#MS" |
         Expediente=="EX-2021-14047400-APN-DD#MS" |
         Expediente=="EX-2021-107124000-APN-SGA#MS") %>% 
  filter(`Usuario actual`!="MAURIMONSALVO") %>% 
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

#C)elimino las que estan en ambos
summary(as.factor(base.busqueda.mezclada$base)) #todos NA, no pego ninguno
base.tram.limpia<-subset(base.busqueda.mezclada,
                         is.na(base.busqueda.mezclada$base))

view(base.busqueda.mezclada)
a<- base.busqueda.mezclada %>% 
  filter(NumExpediente=="EX-2022-05756447-APN-DCYC#MS") 
view(a)


#79 68 98 96 93 93 104 112 117 116 128 127

#D)guardo la base
write.csv2(base.tram.limpia, "resultados/base_tram_limpia")
library(R.utils)
saveObject(base.tram.limpia, "resultados/base_tram_limpia.Rbin")












