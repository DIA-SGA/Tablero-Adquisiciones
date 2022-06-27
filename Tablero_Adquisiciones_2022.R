rm (list=ls())

library(pacman)
p_load(tidyverse,lubridate,stringi,stringr,xlsx,readxl,tm)

## traigo tipo de cambio euro y dolr del momento que se corre el script

dolar<-123.75
euro<-125.75

## traigo fowmc y me quedo con expediente si hay en Expediente donde 
## se encuentra vinculado el formulario FOWMC y sino expediente

fowmc<- read_excel("bases/Form_FOWMC.xlsx") %>% 
  mutate(expediente=ifelse(`Expediente donde se encuentra vinculado el formulario FOWMC`=="Sin información",`Número de expediente`,`Expediente donde se encuentra vinculado el formulario FOWMC`))

##elimino puntos u otros characteres especiales en cmapo importe moneda total
fowmc$`Importe total moneda`<-gsub("[][!#%()*,.:;<=>@^_`|~.{}]","",fowmc$`Importe total moneda`)

## resuelvo tema formato importe total de fowmc
fowmc$`Importe Total` <- format(fowmc$`Importe Total`, scientific = F)
fowmc$`Importe Total`<- gsub(" ", "",fowmc$`Importe Total`)
fowmc$`Importe Total`<- str_sub(fowmc$`Importe Total`,1,nchar(fowmc$`Importe Total`)-3)
fowmc$`Importe Total`<-as.numeric(fowmc$`Importe Total`)

## armo importe final que pase todo a pesos según moneda
fowmc <- fowmc %>% 
  mutate(monto.en.pesos = case_when( `Importe total moneda`=="$" ~ `Importe Total`,
                                     `Importe total moneda`=="USD" ~ `Importe Total`*dolar,
                                     `Importe total moneda`=="EURO" ~ `Importe Total`*euro))

## arreglo por las dudas expediente en fowmc
library(stringr)
#separo la variable para tener el numero de expediente en un vector separado
arreglo<-str_split_fixed(fowmc$expediente, "-", n=Inf)
#agrego los ceros correspondiente a los expedientes q no tienen 8 caracteres
arreglo[,3]<-ifelse(stri_length(arreglo[,3])==6, 
                    paste("00",arreglo[,3],sep = ""),arreglo[,3])
arreglo[,3]<-ifelse(stri_length(arreglo[,3])==7,
                    paste("0",arreglo[,3],sep = ""),arreglo[,3])
#verifico q todos tengan 8 caracteres
stri_length(arreglo[,3])
#reconstruyo la variable y la pego de nuevo en la variable NumExpediente

fowmc$expediente <-paste(
  arreglo[,1],arreglo[,2],arreglo[,3],arreglo[,4],arreglo[,5],sep="-")

## quito espacios a expediente en fowmc
fowmc<-fowmc %>% 
  mutate(expediente=str_replace(expediente," ",""))

## traigo detalle crosstab
expedientes<- read_excel("bases/Detalle_crosstab.xlsx")

## resuelvo tema fechas
expedientes$`Fecha de caratulación`<- word(expedientes$`Fecha de caratulación`, 1, sep = fixed(" "))
expedientes$`Fecha de última modificación`<- word(expedientes$`Fecha de última modificación`, 1, sep = fixed(" "))
expedientes$`Fecha de último pase`<- word(expedientes$`Fecha de último pase`, 1, sep = fixed(" "))

## ## ve con cambio de navegador ##
expedientes$`Fecha de caratulación`<-ymd(expedientes$`Fecha de caratulación`)
expedientes$`Fecha de última modificación`<-ymd(expedientes$`Fecha de última modificación`)
expedientes$`Fecha de último pase`<-ymd(expedientes$`Fecha de último pase`)

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

## cambio nombre a expediente en detalle crosstab para que estén iguales a fowmc
expedientes<-expedientes %>% 
  rename(expediente=Expediente) %>% 
  select(1,5,6,45,42,43,44)

## arreglo por las dudas expediente en expedientes
library(stringr)
#separo la variable para tener el numero de expediente en un vector separado
arreglo<-str_split_fixed(expedientes$expediente, "-", n=Inf)
#agrego los ceros correspondiente a los expedientes q no tienen 8 caracteres
arreglo[,3]<-ifelse(stri_length(arreglo[,3])==6, 
                    paste("00",arreglo[,3],sep = ""),arreglo[,3])
arreglo[,3]<-ifelse(stri_length(arreglo[,3])==7,
                    paste("0",arreglo[,3],sep = ""),arreglo[,3])
#verifico q todos tengan 8 caracteres
stri_length(arreglo[,3])
#reconstruyo la variable y la pego de nuevo en la variable NumExpediente

expedientes$expediente <-paste(
  arreglo[,1],arreglo[,2],arreglo[,3],arreglo[,4],arreglo[,5],sep="-")

## quito espacios a expediente en fowmc
expedientes<-expedientes %>% 
  mutate(expediente=str_replace(expediente," ",""))

## joineo fowmc con detalle crostab_expedientes
crostab_fowmc<-fowmc %>%  
  left_join (expedientes, by=c("expediente"))

## cuento los expedientes duplicados
cuenta_dupl <- crostab_fowmc %>% 
  group_by(expediente) %>%
  summarise(n_exp=n()) %>%
  ungroup()

## le pego a la tabla el dato de duplicado 
crostab_fowmc<- crostab_fowmc %>% 
  left_join(cuenta_dupl, by=c("expediente"))

## divido la tabla en duplicados y no duplicados y luego las vuelvo a unir
## primero elimino los NA en variable "Previsto en el plan anual de compras"
crostab_fowmc <- crostab_fowmc[!is.na(crostab_fowmc$`¿Previsto en el plan anual de compras?`),]

## armo la base de duplicados
crostab_fowmc_dupl<-crostab_fowmc %>% 
  filter(n_exp>1) 

## armo la base de sin duplicados 
crostab_fowmc_no_dupl<-crostab_fowmc %>% 
  filter(n_exp==1) 

## me quedo con el primero de los duplicados
crostab_fowmc_dupl<- crostab_fowmc_dupl %>% 
  group_by(expediente) %>% 
  slice(1)

## uno ambas bases
dim(crostab_fowmc_dupl)
dim(crostab_fowmc_no_dupl)
crostab_fowmc_final<-rbind(crostab_fowmc_dupl,crostab_fowmc_no_dupl)
dim(crostab_fowmc_final)

## chequeo que no haya duplicados
crostab_fowmc_final_dup <- crostab_fowmc_final %>% 
  group_by(expediente) %>%
  summarise(n_exp=n()) %>%
  ungroup()

crostab_fowmc_final_dup %>% 
  filter(n_exp>1)

## elinino los na de campos de crosstab "sector" "reparticion" "fecha ultimo pase"

sum (is.na(crostab_fowmc_final$`Sector actual`))

crostab_fowmc_final <- crostab_fowmc_final[!is.na(crostab_fowmc_final$`Sector actual`),]

sum (is.na(crostab_fowmc_final$`Sector actual`))
dim(crostab_fowmc_final)

## guardo la tabla para chequear datos
#openxlsx::write.xlsx(crostab_fowmc_final, file = "crostab_fowmc_final.xlsx")

###############
#primera parte# ###lista####
###############

##1- si hay campo valido en Expediente donde se encuentra vinculado el formulario FOWMC, entonces esto, sino
##el campo Expediente

##2-de los que están duplicado nos quedamos con el que tiene si o no en pac previsto

##3-lo que no joineo son los que están cerrados, por lo tanto vuelan
# se excluyen

###############
#segunda parte#
###############

## 1-Convertir USD y EUR en ARG ------LISTO
## 2-Normalizar el campo "repartición actual" siguiendo los criterios que usamos en el tablero de adquisiciones actual ------LISTO

### reemplazamos texto por NA, esto en próximas versiones será necesario

crostab_fowmc_final$Dirección<-ifelse(crostab_fowmc_final$Dirección=="N/A",
                        NA,crostab_fowmc_final$Dirección)
crostab_fowmc_final$Dirección<-ifelse(crostab_fowmc_final$Dirección=="S/D",
                        NA,crostab_fowmc_final$Dirección)

crostab_fowmc_final$`Dirección General`<-ifelse(crostab_fowmc_final$`Dirección General`=="N/A",
                                  NA,crostab_fowmc_final$`Dirección General`)
crostab_fowmc_final$`Dirección General`<-ifelse(crostab_fowmc_final$`Dirección General`=="S/D",
                                  NA,crostab_fowmc_final$`Dirección General`)

crostab_fowmc_final$Secretaria<-ifelse(crostab_fowmc_final$Secretaría=="N/A",
                         NA,crostab_fowmc_final$Secretaría)
crostab_fowmc_final$Secretaria<-ifelse(crostab_fowmc_final$Secretaría=="S/D",
                         NA,crostab_fowmc_final$Secretaría)

fowmc$Subsecretaria<-ifelse(fowmc$Subsecretaria=="N/A",
                            NA,fowmc$Subsecretaria)
fowmc$Subsecretaria<-ifelse(fowmc$Subsecretaria=="S/D",
                            NA,fowmc$Subsecretaria)

## corrijo la variable reparticion actual

#c)agrupo la variable ubicacion
crostab_fowmc_final<-crostab_fowmc_final %>% 
  mutate(ubicacion.actual = case_when(
    substr(`Repartición actual`,1,5)=="DGPFE" ~ "DGPFE",
    substr(`Repartición actual`,1,5)=="DAFYP" ~ "DGPFE",
    substr(`Repartición actual`,1,4)=="DGAJ" ~ "DAL",
    substr(`Repartición actual`,1,3)=="DAL" ~ "DAL",
    #substr(`Repartición actual`,1,2)=="DS" ~ "DAL",
    substr(`Repartición actual`,1,3)=="DGA" ~ "DGA",
    substr(`Repartición actual`,1,4)=="DCYC" ~ "Compras",
    substr(`Repartición actual`,1,4)=="SSGA" & substr(`Sector actual`,1,3)=="CRD" ~ "CRD",
    substr(`Repartición actual`,1,3)=="SGA" & substr(`Sector actual`,1,3)=="CRD" ~ "CRD",
    substr(`Repartición actual`,1,4)=="SSGA" & substr(`Sector actual`,1,3)!="CRD" ~ "SGA",
    substr(`Repartición actual`,1,3)=="SGA" & substr(`Sector actual`,1,3)!="CRD" ~ "SGA",
    substr(`Repartición actual`,1,4)=="DTYC" ~ "Contabilidad",
    substr(`Repartición actual`,1,4)=="DCYT" ~ "Contabilidad",
    substr(`Repartición actual`,1,4)=="RCTD" ~ "Ejercito Argentino",
    substr(`Repartición actual`,1,2)=="DD" ~ "Despacho",
    substr(`Repartición actual`,1,3)=="DSB" ~ "Programas",
    substr(`Repartición actual`,1,3)=="DSD" ~ "Programas",
    substr(`Repartición actual`,1,9)=="DSFYTT#MS" ~ "Programas",
    substr(`Repartición actual`,1,3)=="DSO" ~ "RRHH",
    substr(`Repartición actual`,1,3)=="DGO" ~ "DGA",
    substr(`Repartición actual`,1,6)=="DGPYCP" ~ "Presupuesto",
    TRUE                      ~ "Programas")) 
nrow(crostab_fowmc_final)
unique(crostab_fowmc_final$ubicacion.actual)

## 3-Joinear vía número de expedientes contra detalle_procesos_crosstab y traer el campo "Estado pliego"
## 4-joinear contra documentos asociados 2021 y 2022 y traer el acronimo del último documento asociado
### ver que hay que expluir ifgra, pv, nota ver
## 5-luego traer etapas por acronimo

## levanto detalle_procesos_crosstab

detalle_proceso_crost<- read_excel("bases/Hoja_Detalle_Procesos_Compra_crosstab.xlsx") %>% 
  select(5,4) %>% 
  rename(expediente=`Número expediente electrónico`) 

## arreglo por las dudas expediente en detalle_proceso_crost
library(stringr)
#separo la variable para tener el numero de expediente en un vector separado
arreglo<-str_split_fixed(detalle_proceso_crost$expediente, "-", n=Inf)
#agrego los ceros correspondiente a los expedientes q no tienen 8 caracteres
arreglo[,3]<-ifelse(stri_length(arreglo[,3])==6, 
                    paste("00",arreglo[,3],sep = ""),arreglo[,3])
arreglo[,3]<-ifelse(stri_length(arreglo[,3])==7,
                    paste("0",arreglo[,3],sep = ""),arreglo[,3])
#verifico q todos tengan 8 caracteres
stri_length(arreglo[,3])
#reconstruyo la variable y la pego de nuevo en la variable NumExpediente

detalle_proceso_crost$expediente <-paste(
  arreglo[,1],arreglo[,2],arreglo[,3],arreglo[,4],arreglo[,5],sep="-")

## quito espacios a expediente en fowmc
detalle_proceso_crost<-detalle_proceso_crost %>% 
  mutate(expediente=str_replace(expediente," ",""))

## junto detalle porcesos contra fowmc y detalle crosstab

crostab_fowmc_final<- crostab_fowmc_final %>% 
  left_join(detalle_proceso_crost, by=c("expediente"))

## chequeo duplicados luego de joinear fowmc y procesos crostab

crostab_fowmc_final_dupl <- crostab_fowmc_final %>% 
  group_by(expediente) %>%
  summarise(n_exp=n()) %>%
  ungroup()

## le pego a crostab_fowmc_final el n_exp
crostab_fowmc_final<-crostab_fowmc_final %>% 
  left_join(crostab_fowmc_final_dupl, by=c("expediente"))

## elimno los duplicados sacando a los que dice "sin efecto" en estado pliego

crostab_fowmc_final<-crostab_fowmc_final %>% 
  mutate(estado_n_exp=paste0(n_exp.y,`Estado Pliego`)) %>% 
  filter(estado_n_exp!="2Sin efecto")

## traigo documentos asociados a expedientes 2021,2022

doc.acronimos2020<-read_xlsx("bases/Documentos_vinculados_a_expedientes_crosstab2020.xlsx")
doc.acronimos2021<-read_xlsx("bases/Documentos_vinculados_a_expedientes_crosstab2021.xlsx")
doc.acronimos2022<-read_xlsx("bases/Documentos_vinculados_a_expedientes_crosstab2022.xlsx")
nrow(doc.acronimos2020)
nrow(doc.acronimos2021) 
nrow(doc.acronimos2022) 

names(doc.acronimos2020)
names(doc.acronimos2021)
names(doc.acronimos2022)

## construyo las fechas en el formato en que las levante luego el script
## me quedo con la primer parte de la fecha
doc.acronimos2020$`Fecha de creación del documento` <- word(doc.acronimos2020$`Fecha de creación del documento`, 1, sep = fixed(" "))
doc.acronimos2021$`Fecha de creación del documento` <- word(doc.acronimos2021$`Fecha de creación del documento`, 1, sep = fixed(" "))
doc.acronimos2022$`Fecha de creación del documento` <- word(doc.acronimos2022$`Fecha de creación del documento`, 1, sep = fixed(" "))

## las paso a formato lubridate ##
doc.acronimos2020$`Fecha de creación del documento`<-ymd(doc.acronimos2020$`Fecha de creación del documento`)
doc.acronimos2021$`Fecha de creación del documento`<-ymd(doc.acronimos2021$`Fecha de creación del documento`)
doc.acronimos2022$`Fecha de creación del documento`<-ymd(doc.acronimos2022$`Fecha de creación del documento`)

## genero cada fecha en el formato que la levanta el script más adelante
## las dejo en el formato dia mes año 
doc.acronimos2020 <- doc.acronimos2020 %>%
  mutate(año=year(`Fecha de creación del documento`),
         mes=month(`Fecha de creación del documento`),
         dia=day(`Fecha de creación del documento`),
         fecha=paste(dia,mes,año,sep="/")) %>%
  select(1:17,26,19:22) %>%
  rename(`Fecha de creación del documento`="fecha")

doc.acronimos2021 <- doc.acronimos2021 %>%
  mutate(año=year(`Fecha de creación del documento`),
         mes=month(`Fecha de creación del documento`),
         dia=day(`Fecha de creación del documento`),
         fecha=paste(dia,mes,año,sep="/")) %>%
  select(1:17,26,19:22) %>%
  rename(`Fecha de creación del documento`="fecha")

doc.acronimos2022 <- doc.acronimos2022 %>%
  mutate(año=year(`Fecha de creación del documento`),
         mes=month(`Fecha de creación del documento`),
         dia=day(`Fecha de creación del documento`),
         fecha=paste(dia,mes,año,sep="/")) %>%
  select(1:17,26,19:22) %>%
  rename(`Fecha de creación del documento`="fecha")

## une acrónimos 2020,21,22
doc.acronimos<-rbind(doc.acronimos2020, doc.acronimos2021,doc.acronimos2022[,1:22])
names(doc.acronimos)
nrow(doc.acronimos)

## arreglo por las dudas expediente en acrónimos
library(stringr)
#separo la variable para tener el numero de expediente en un vector separado
arreglo<-str_split_fixed(doc.acronimos$Expediente, "-", n=Inf)
#agrego los ceros correspondiente a los expedientes q no tienen 8 caracteres
arreglo[,3]<-ifelse(stri_length(arreglo[,3])==6, 
                    paste("00",arreglo[,3],sep = ""),arreglo[,3])
arreglo[,3]<-ifelse(stri_length(arreglo[,3])==7,
                    paste("0",arreglo[,3],sep = ""),arreglo[,3])
#verifico q todos tengan 8 caracteres
stri_length(arreglo[,3])
#reconstruyo la variable y la pego de nuevo en la variable NumExpediente

doc.acronimos$expediente <-paste(
  arreglo[,1],arreglo[,2],arreglo[,3],arreglo[,4],arreglo[,5],sep="-")

## quito espacios a expediente en fowmc
doc.acronimos<-doc.acronimos %>% 
  mutate(expediente=str_replace(expediente," ",""))

#####################################################################
#######si están los siguientes acro en doc.acrónimos y los saco de
############################crostab_fowmc_final#######################
######################################################################

## genero un vector con los acrónimos que queremos sacar
orcom.di.contr<-c("DI","ORCOM","CONTR","FOCSZ")

## filtro esos acrónimos en doc.acronimos
doc.acronimos.off<-doc.acronimos %>% 
  filter(Acrónimo %in% orcom.di.contr) %>% 
  group_by(expediente) %>% 
  arrange(Orden) %>% 
  slice(n()) %>% 
  select (23) 

## hago un antijoin contra esos expedientes en crostab_fowmc_final
crostab_fowmc_final_acro.off<-crostab_fowmc_final %>% 
  anti_join(doc.acronimos.off, by=c("expediente"))

## sacamos a mano tres expedientes que tienen acro contr pero 
## que por no estar actualizado doc.asoc.2020 tienen como último
## acro dictamen jurídico.

exped<-c("EX-2021-102311397-APN-SGA#MS",
         "EX-2020-83857526-APN-SSGA#MS",
         "EX-2021-67223653-APN-SGA#MS")

dim(crostab_fowmc_final_acro.off)
crostab_fowmc_final_acro.off <- crostab_fowmc_final_acro.off %>% 
  filter(!(expediente %in% exped))
  
#B) hago una tabla con los ultimos documentos de c/expediente
##exceptuando PV, IF e IFGRA
library(dplyr)
ultimo.sin.pv.if.ifgra<-doc.acronimos %>%
#  rename(expediente="Expediente") %>%
  select(expediente, `Orden`, `Número`,
         `Usuario asociador del documento`, `Usuario creador del documento`,
         `Fecha de creación del documento`, `Tipo de documento`, `Acrónimo`, 
         `Motivo del documento`) %>% 
  filter(Acrónimo=="CAREX" | Acrónimo=="PLIEG" | 
           #Acrónimo=="NOBAN" | 
           Acrónimo=="BOFIC" | Acrónimo=="PUCON" | 
           Acrónimo=="ACFO" | 
           #Acrónimo=="IFGRA" | Acrónimo=="NOBAN" | 
           Acrónimo=="NOTIF" | Acrónimo=="INFFC" | Acrónimo=="PDISP" |
           Acrónimo=="DICJU" | Acrónimo=="DI" | Acrónimo=="ORCOM" | 
           Acrónimo=="CONTR") %>%  
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
  group_by(expediente) %>% 
  arrange(Orden) %>%
  slice(n()) %>%
  ungroup %>% 
  as.data.frame()
nrow(ultimo.sin.pv.if.ifgra)

tabla.ultimos.acronimos<-ultimo.sin.pv.if.ifgra %>% 
  group_by(Acrónimo) %>% 
  summarise(n())

# base.tramitaciones.doc<-merge(crostab_fowmc_final, ultimo.sin.pv.if.ifgra, 
#                               by.y="Expediente", by.x = "expediente",
#                               all.x = T)

base.tramitaciones.doc<-crostab_fowmc_final_acro.off %>% 
  left_join(ultimo.sin.pv.if.ifgra, by=c("expediente"))

# ## para ver porque nom macheaban tantos expedientes
# 
# view(base.tramitaciones.doc)
# a<-base.tramitaciones.doc %>% 
#   filter (is.na(Orden))
# view(a)
# unique(a$`Estado Pliego`)

#a<-c("EX-2021-53722548-APN-DCYC#MS","EX-2021-81316714-APN-DCYC#MS","EX-2021-89066104-APN-DCYC#MS","EX-2021-70351054-APN-DCYC#MS")

#b<-crostab_fowmc_final %>%
#filter(expediente %in% a)  
#view(b)

###### ver esto  #####
###### ver esto  #####
#### acá tenemos un problema que cuándo asocio lo último de crostab_fial(144)
#### pasamos a tener 148, agún expediente está duplicado en procesos ver estooo 
## de los duplicados elimino los sin efecto 

#openxlsx::write.xlsx(b, file = "asi_qeudarian_esos_cuatro.xlsx")
#unique(crostab_fowmc_final$`Estado Pliego`)

 ## campos que van 
openxlsx::write.xlsx(a,file = "campos.xlsx")

##unifico estado pliego todo en una de modelo comprar y acrónimos

base.tramitaciones.doc <- base.tramitaciones.doc %>% 
mutate(estado_pliego_2=case_when(`Estado Pliego`=="En Apertura" ~ "3-Apertura", 
                                 `Estado Pliego`=="Proceso Iniciado" ~ "1-Inicial",
                                 `Estado Pliego`=="En proceso de adjudicación" ~ "5-Preadjudicación",
                                 `Estado Pliego`=="En Evaluación" ~ "4-Evaluación",
                                 `Estado Pliego`=="Publicado" ~ "2-Publicación o invitación",
                                 `Estado Pliego`=="Sin efecto" ~ "Sin efecto",
  TRUE                      ~ "NA")) 

base.tramitaciones.doc <- base.tramitaciones.doc %>% 
  mutate(Etapa.del.proceso.final=case_when(estado_pliego_2=="NA" ~ Etapa.del.proceso, 
                                   TRUE                      ~ estado_pliego_2)) 
#view(base.tramitaciones.doc)
#b<-base.tramitaciones.doc %>% 
#  select(23,46,47,45,34)
#view(base_final_parcial)

## tabla final para ver
base_final_parcial<-base.tramitaciones.doc %>% 
  select(4,6,9,11,24,15,18,20,21,23,25,27,28,29,30,33,39,40,47) 

## armo variables repartición actual y repartición actual corta 
## acorto la variable reparticion actual
base_final_parcial$`Repartición actual`
base_final_parcial$reparticion.actual.corta<-gsub("#MS","",
                                                      unlist(strsplit(base_final_parcial$`Repartición actual`," ")) [grepl(
                                                        "#",unlist(strsplit(base_final_parcial$`Repartición actual`," ")))
                                                      ]
)
table(base_final_parcial$reparticion.actual.corta)

## agrupo los nombres de REPARTICION ACTUAL CORTA
base_final_parcial<-base_final_parcial %>% 
  mutate(reparticion.actual.agrup = case_when(
    reparticion.actual.corta=="DGPFE" ~ "DGPFE",
    reparticion.actual.corta=="DAFYP" ~ "DGPFE",
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
    reparticion.actual.corta=="DSB" ~ "Programas",
    reparticion.actual.corta=="DSD" ~ "Programas",
    reparticion.actual.corta=="DSFYTT" ~ "Programas",
    reparticion.actual.corta=="DSO" ~ "RRHH",
    reparticion.actual.corta=="DGO" ~ "DGA",
    reparticion.actual.corta=="DGPYCP" ~ "Presupuesto",
    TRUE                      ~ "Programas")) 

## armo las variables tipo proceso inciso y fuente financiamiento 

base_final_parcial<-base_final_parcial %>% 
  mutate (`Tipo de proceso`=substr(`Tipo de proceso`,start=1,stop=4)) %>% 
  mutate(`Fuente de financiamiento`=(str_sub(`Fuente de financiamiento`,4,nchar
                             (`Fuente de financiamiento`)-0))) %>% 
  mutate(Inciso=(str_sub(Inciso,3,nchar
                           (Inciso)-0))) 

## le saco espacio y caracteres raros a tipo proceso
base_final_parcial$`Tipo de proceso`<-gsub("[][!#%()*,.:;<=>@^_`|~.{}-]","",base_final_parcial$`Tipo de proceso`)

## armo las variables de tiempo días desde el inicio y desde ultimo pase
hoy <- as.Date(Sys.Date(), format="%Y/%m/%d")

base_final_parcial$`Días del el inicio`<-hoy - 
  as.Date(base_final_parcial$`Fecha de caratulación`, format="%d/%m/%Y")

base_final_parcial$`Días desde el último pase`<-hoy - 
  as.Date(base_final_parcial$`Fecha de último pase` , format="%d/%m/%Y")

## base final 
base_final_parcial<- base_final_parcial %>% 
  select(-17,-18,-20,-21)

## nombres definitivos de las variables
names(base_final_parcial)<-c("Secretaría","Dirección General","Nombre del proceso",
                             "Tipo de Proceso","Importe Total","Previsto en PAC",
                             "Inciso","Rubro","Fuente Financiamiento","Expediente",
                             "Fecha caratulación","Fecha de último pase","Usuario actual",
                             "Sector Actual","Repartición Actual","Ubicación actual","Estado Pliego",
                             "Días desde el inicio","Días desde el último pase")

acronimos_que_van<- c("1-Inicial","3-Apertura","4-Evaluación","5-Preadjudicación","2-Publicación o invitación")

base_final_parcial <- base_final_parcial %>% 
  filter(`Estado Pliego`%in% acronimos_que_van)
  
## guardamos la base

library(R.utils)
saveObject(base_final_parcial, "resultados/base_tabla_reporte.Rbin")
#library(R.utils)
tablafinal<-loadObject("resultados/base_tabla_reporte.Rbin")
nrow(tablafinal)

## agregamos una piso más abajo con grafico de importe total por previsto en pac
## en una tabla poner secretaría cantiad de procesos y cantidad de guita
## gráfico que apile fuente financiamiento y tipo proceso
## gráfico torta rubro e inciso

