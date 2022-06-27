#9-APLICO LOS FILTROS
#A)Filtros de expedientes
basecompleta2 <-
  basecompleta %>% 
  filter(`Estado Pliego`!="9-Sin efecto") %>% 
  filter(`Estado Pliego`!="6-Adjudicado") %>% 
  filter(`Sistema creador`=="EE" | `Sistema creador`=="COMPRAR") %>% 
  filter(substr(`Usuario actual`,1,5)!="DADSE") %>% 
  filter(substr(`Usuario actual`,1,5)!="DGAGN") %>% 
  mutate(anio_caratulacion = substr(as.Date(`Fecha de caratulación`, 
                                            format="%d/%m/%Y"),1,4)) %>% #Creo la variable anio_caratulacion
  mutate(dias_ultimo_pase = hoy-
           as.Date(`Fecha de último pase`, format="%d/%m/%Y")) 
nrow(basecompleta2)

basecompleta$`Fecha de último pase`[1:10]
basecompleta2$dias_ultimo_pase
basecompleta$`Fecha de caratulación`
basecompleta2$anio_caratulacion

#B)Filtros de procesos
#(i)Saco los pliegos con estado: Adjudicado o sin Efecto
#(conservo el resto, incluso los vacios)
filtro.eproceso<-basecompleta2 %>% 
  filter(is.na(`Estado proceso`) | 
           (`Estado proceso`!="99-Adjudicado" & 
              `Estado proceso`!="99-Dejado Sin Efecto" & 
              `Estado proceso`!="99-Fracasado" & 
              `Estado proceso`!="99-Desierto" ))
nrow(filtro.eproceso)
#217 220 219 217 151
table(filtro.eproceso$`Estado proceso`, filtro.eproceso$`Estado Pliego`)

#guardo la base
write.csv2(filtro.eproceso, "resultados/base.filtro_estadoPROCESO")

#C)Saco las solicitudes q no son autorizadas, conservando las q estan vacias
#(conservo los vacios)
filtro.eproceso.auto<-filtro.eproceso %>% 
  filter(is.na(`Estado agrupado`) | `Estado agrupado` =="Autorizada")
nrow(filtro.eproceso.auto) 
#216 219 223 218 216 150

#guardo la base
write.csv2(filtro.eproceso.auto, "resultados/base.filtro_estadoPROCESO_sinDGAGN_scoAUTO")

##### MIRO LA CANTIDAD DE REGISTROS VACIOS DE LA PROGRAMATICA
#cuento cuantos son
filtro.eproceso.auto %>% 
  mutate(sinprograma=case_when(is.na(`Apertura programática`)~"vacio", T~"ok")) %>% 
  group_by(sinprograma, anioEXP, anioSCO) %>% 
  summarise(cantidad=n())

#miro cuales son
sinprograma <- filtro.eproceso.auto %>%
    mutate(sinprograma=case_when(is.na(`Apertura programática`)~"vacio", T~"ok")) %>% 
    filter(sinprograma=="vacio")
sinprograma

