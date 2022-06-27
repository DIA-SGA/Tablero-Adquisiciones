#1-importo los archivos bajados de TABLEAU
#importo todos
library(readxl)
archivos<-list.files("bases/")
archivos

sco<- read_excel("bases/Hoja_Detalle_solicitudes_crosstab.xlsx")
sco.expaso<- read_excel("bases/Expedientes_Asociados_crosstab.xlsx")
sco.presup<- read_excel("bases/Información_presupuestaria_crosstab.xlsx")

##### MIRO LA CANTIDAD DE REGISTROS VACIOS DE LA PROGRAMATICA
library(dplyr)
sco.presup %>% 
  mutate(sinprograma=case_when(is.na(`Apertura programática`)~"vacio",
                               T ~ "ok")) %>% 
  group_by(Ejercicio, sinprograma) %>% 
  summarise(cantidad=n())
#141 144 145 156 156 151 153 150 151

procesos <- read_excel("bases/Hoja_Detalle_Procesos_Compra_crosstab.xlsx", 
                       col_types = c("text", "text", "text", "text", 
                                     "text", "text", "text", "text",
                                     "text", "text", "text", "text", 
                                     "numeric")) 
expedientes<- read_excel("bases/Detalle_crosstab.xlsx")

#miro la dimension de cada uno
dim(sco) #694 701 717 726 738 738 743
dim(sco.expaso) #660 662 677 686 698 699 703
dim(sco.presup) #934 938 954 971 989 991 712
dim(procesos) #642 646 653 655 660 664 676
dim(expedientes) #3190 3203 3234 3248 3263 3266 3293

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
#1040 1049 1066 1082 1100 1104 786

#3-SELECCIONO LAS VARIABLES DE CADA BASE
#A)BASE DE SCO me quedo solo con vs de interes
names(sco3)
library(dplyr)
sco4<-
  sco3 %>% 
  #filter(`Estado agrupado`=="Autorizada") %>% 
  select( `Número solicitud`, `Nombre solicitud`, `Estado agrupado`, 
          `Unidad Solicitante`,`Monto solicitud`, 
          `Nro. proceso`, `Estado proceso`,`Día de Fecha autorización`,
          `Apertura programática`, 
          `Nro. expediente (del pliego)`, `Nro. expediente (de la solicitud)`) %>% 
  as.data.frame()
names(sco4)

##### MIRO LA CANTIDAD DE REGISTROS VACIOS DE LA PROGRAMATICA
library(stringi)
sco4 %>% 
  mutate(sinprograma=case_when(is.na(`Apertura programática`)~"vacio",
                               T ~ "ok")) %>% 
  group_by(substr(`Número solicitud`,
                  stri_length(`Número solicitud`)-1,
                  stri_length(`Número solicitud`)),
           sinprograma) %>% 
  summarise(cantidad=n())

#B)BASE DE PROCESOS me quedo solo con vs de interes
# y limpio la base de procesos, 
#descartando los q tienen proc ADJUDICADOS, DEJADOS SIN EFECTO O DESIERTOS
procesos2<-procesos %>% 
  select( `Nro. Pliego`, `Nombre pliego`, `Estado Pliego`, 
          `Número expediente electrónico`,
          `Día de Fecha creación proceso`) %>% 
  as.data.frame()

#C)FILTRO LA BASE DE EXPEDIENTES (me quedo solo con vs de interes)
#controlo q esten los GNE pedidos
#table(substr(expedientes$`Tipo de trámite`,1,8))
names(expedientes)
expedientes2<-expedientes %>% 
#  select(Expediente, `Fecha de último pase`, 
#         `Usuario actual`, `Repartición actual`, Estado,
#         `Sistema creador`, `Grupo de trámite`, Repartición, `Sector caratulador`,
#         `Fecha de caratulación`) %>% 
#  as.data.frame()
select(Expediente, Descripción, `Estado expediente`, Estado,
       `Fecha de caratulación`, `Tipo de trámite`, `Sistema creador`,
       Organismo, Repartición, `Cant. de doc`,
       `Fecha de último pase`, `Usuario actual`,
       `Repartición actual`, `Fecha de último pase`, 
       `Motivo del último pase`,
       `Grupo de trámite`, `Sistema creador`, Repartición, `Sector caratulador`) %>% 
  as.data.frame()

#4-MEZCLO LAS BASES
#A) MEZCLO LAS BASES DE PROCESOS Y EXPEDIENTES 
#conservando todos los Procesos
nrow(procesos2)  
procesos2$baseprocesos2<-1:nrow(procesos2)  
nrow(expedientes2)
expedientes2$baseexpedientes2<-1:nrow(expedientes2) 

#hago homogenea las variables del merge
procesos2$NumExpediente <- procesos2$`Número expediente electrónico` 
expedientes2$NumExpediente <- expedientes2$Expediente 

###corrijo los numeros de expedientes q tienen uno o dos ceros menos 
library(stringr)
#separo la variable para tener el numero de expediente en un vector separado
arreglo<-str_split_fixed(expedientes2$NumExpediente, "-", n=Inf)
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

#incluyo forzosamente a la base de PROCESOS
proc.ex <- merge(procesos2, expedientes2, by ="NumExpediente", all.x=T)
nrow(proc.ex) #556 576 581 590 608 655 664
summary(as.factor(expedientes2$`Usuario actual`))
proc.ex$`Usuario actual`

#(i)EXISTEN REGISTROS SIN NUMEROS DE EXPEDIENTES?
summary(as.factor(proc.ex$NumExpediente))
nrow(subset(proc.ex, is.na(proc.ex$NumExpediente)))
#0

#B)MEZCLO LAS TRES BASES
#(i)marco la base proc.ex
nrow(proc.ex)
proc.ex$baseproc.ex<-paste("proc.ex",1:nrow(proc.ex),sep="")

#(ii)marco la base sco
nrow(sco4)
sco4$basesco.autorizados<-1:nrow(sco4)

#(iii)llamo con el mismo nombre a la variable ID
sco4$PliegoProceso <- sco4$`Nro. proceso`
proc.ex$PliegoProceso <- proc.ex$`Nro. Pliego`

#(iv)mezclo las bases incluyendo forzosamente a la base de PROCESOS
sco.proc.ex <- merge(sco4, proc.ex, by ="PliegoProceso", all.y = T)
nrow(sco.proc.ex) #912 919 932 963 968 974

#5-CONTROLES
#A)Chequeo q no haya NumExpediente y `Nro. expediente (del pliego)` distintos
nrow(subset(sco.proc.ex, 
            gsub("--","-",gsub(" ","",sco.proc.ex$`Nro. expediente (del pliego)`))!= sco.proc.ex$NumExpediente))
#todas iguales

#B)EXISTEN REGISTROS SIN NumExpediente?
nrow(subset(sco.proc.ex, is.na(sco.proc.ex$NumExpediente)))
#0

#6-AGREGADO DE VARIABLES A CALCULAR
names(sco.proc.ex)
#A)Variable anio
sco.proc.ex$anioEXP<-substr(sco.proc.ex$Expediente,4,7) 

#control de la variable anioEXP
sco.proc.ex %>% 
  group_by(anioEXP) %>% 
  summarise(n()) 
#584 no tienen anioEXP 652 654 658 660 666 671

###cuando anio esta vacio, lo completo con la fecha de inicio del SCO

sco.proc.ex$anioSCO<-substr(sco.proc.ex$`Día de Fecha autorización`,
                            stri_length(sco.proc.ex$`Día de Fecha autorización`)-3,
                            stri_length(sco.proc.ex$`Día de Fecha autorización`))

#control de la variable anioSCO
sco.proc.ex %>% 
  group_by(anioSCO) %>% 
  summarise(n()) 
#181 no tienen anioSCO 216 217 220 219 222 220

#B)ordeno las variable Estado de pliego
sco.proc.ex$`Estado Pliego`<-ifelse(sco.proc.ex$`Estado Pliego`=="Sin informar",
                                    "0-Sin informar",sco.proc.ex$`Estado Pliego`)
sco.proc.ex$`Estado Pliego`<-ifelse(sco.proc.ex$`Estado Pliego`=="Proceso Iniciado",
                                    "1-Proceso iniciado ",sco.proc.ex$`Estado Pliego`)
sco.proc.ex$`Estado Pliego`<-ifelse(sco.proc.ex$`Estado Pliego`=="Publicado",
                                    "2-Publicado",sco.proc.ex$`Estado Pliego`)
sco.proc.ex$`Estado Pliego`<-ifelse(sco.proc.ex$`Estado Pliego`=="En Apertura",
                                    "3-Apertura",sco.proc.ex$`Estado Pliego`)
sco.proc.ex$`Estado Pliego`<-ifelse(sco.proc.ex$`Estado Pliego`=="En Evaluación",
                                    "4-Evaluación",sco.proc.ex$`Estado Pliego`)
sco.proc.ex$`Estado Pliego`<-ifelse(sco.proc.ex$`Estado Pliego`=="En proceso de adjudicación",
                                    "5-Proceso de adjudicación",sco.proc.ex$`Estado Pliego`)
sco.proc.ex$`Estado Pliego`<-ifelse(sco.proc.ex$`Estado Pliego`=="Adjudicado",
                                    "6-Adjudicado",sco.proc.ex$`Estado Pliego`)
sco.proc.ex$`Estado Pliego`<-ifelse(sco.proc.ex$`Estado Pliego`=="Sin efecto",
                                    "9-Sin efecto",sco.proc.ex$`Estado Pliego`)
table(sco.proc.ex$`Estado Pliego`)

#C)Variables de fecha
#cargo el dia de hoy
hoy <- as.Date(Sys.Date(), format="%Y/%m/%d")

sco.proc.ex$`Días desde autorización`<-hoy - 
  as.Date(sco.proc.ex$`Día de Fecha autorización`,format="%d/%m/%Y")
sco.proc.ex %>% 
  group_by(`Día de Fecha autorización`, `Días desde autorización`) %>% 
  summarise(n())

sco.proc.ex$`Días desde la creación del proceso`<-hoy - 
  as.Date(sco.proc.ex$`Día de Fecha creación proceso` , format="%d/%m/%Y")
sco.proc.ex %>% 
  group_by(`Día de Fecha creación proceso`, `Días desde la creación del proceso`) %>% 
  summarise(n())

sco.proc.ex$`Días desde última modificación`<-hoy - 
  as.Date(sco.proc.ex$`Fecha de último pase` , format="%d/%m/%y")
sco.proc.ex %>% 
  group_by(`Fecha de último pase`, `Días desde última modificación`) %>% 
  summarise(n())

#D) reemplazo puntos y comas en la variable monto de la solicitud
sco.proc.ex$MontoSolic<-
  stri_replace_all(
    stri_replace_all(sco.proc.ex$`Monto solicitud`, "", fixed="."),
    "", fixed="$")
#hago la variable numerica y la redondeo sin decimales
sco.proc.ex$MontoSolic<-round(as.numeric(gsub(",",".",sco.proc.ex$MontoSolic)))

#7-Seleccion de variables
names(sco.proc.ex)
sco.proc.ex2<-sco.proc.ex %>% 
  select(`Unidad Solicitante`, NumExpediente, Estado, anioEXP, anioSCO,
         `Número solicitud`, `Nombre solicitud`, `Nro. Pliego`, `Nombre pliego`,
         MontoSolic, `Día de Fecha creación proceso`,
         `Días desde la creación del proceso`,
         `Estado proceso`,`Estado agrupado`,
         `Día de Fecha autorización`,`Días desde autorización`,
         `Estado Pliego`,`Fecha de último pase`,
         `Usuario actual`,`Repartición actual`,
         `Días desde última modificación`, `Apertura programática`,
         `Nro. expediente (de la solicitud)`, `Nro. expediente (del pliego)`,
         `Sistema creador`, `Grupo de trámite`, Repartición, `Sector caratulador`,
         `Fecha de caratulación`)
dim(sco.proc.ex2)

#8-PEGO LA BASE CON LA INF PROGRAMATICA
#A)importo la programatica
program <- read_excel("SUBSEdatos/RedProgramatica_Ejercicio2020_nivSecretaria_EDITADOcovid20200921.xlsx")
nrow(program) #128 #129 130

#B)unifico las variables
program$`Apertura programática`<-paste(program$Prog., 
                                       program$Subprog., 
                                       program$Proy., program$Act., 
                                       program$Obra, sep=".")

#C)mezclo las bases
basecompleta<-merge(sco.proc.ex2, program, by="Apertura programática", all.x = T)
nrow(basecompleta) 
#944 952 962 963 968 745

#D)
summary(as.factor(basecompleta$`Apertura programática`))
#vacios 219 214 223 228 233 245 249 250 258

#E)guardo la base
write.csv2(basecompleta, "resultados/base_completaPROCESO")
#View(basecompleta)
names(basecompleta)

