  #10-TABLA DINAMICA
  #A)Elimino los anteriores a 2020
  #nrow(filtro.eproceso.auto)
  library(stringr)
  paralatablaEP<-filtro.eproceso.auto %>% 
  filter(substr(`Día de Fecha creación proceso`,
                str_count(`Día de Fecha creación proceso`)-1,
                str_count(`Día de Fecha creación proceso`))=="20" | 
           substr(`Día de Fecha creación proceso`,
                  str_count(`Día de Fecha creación proceso`)-1,
                  str_count(`Día de Fecha creación proceso`))=="21")
  nrow(paralatablaEP)
  
  #208 212 216 211 209 143
  
  #B)Me fijo si existen numero de solicitudes repetidos?
  repetidas<-paralatablaEP %>% 
    #group_by(`Número solicitud`) %>% 
    group_by(`Nro. Pliego`) %>% 
    #group_by(`Número solicitud`, `Nro. Pliego`) %>% 
    summarise(freq=n()) %>% 
    filter(freq>1)
  sum(repetidas$freq)-nrow(repetidas) 
  #View(repetidas)
  #43 repetidas 116 113 110 41
  
  #C)Elimino los repetidos
  tablafinalEP<-paralatablaEP %>% 
  #  group_by(`Número solicitud`) %>% 
  group_by(`Nro. Pliego`) %>% #CAMBIEN NRO.SOLICITUD X NRO.PLIEGO,XQ ALGUNOS NOtienen SCO
    slice(1) %>% 
    as.data.frame()
  nrow(tablafinalEP)
  #53 46 52 49 48 48 51 85 86 88 88 89 93 97 100 98 99 102
  
  #D)Elimino LAS SCO CON FECHA DE AUTORIZACION 
  #anterior a 2020 y q no tienen proc asociado
  library(stringr)
  #View(tablafinalEP)
  
  #E)Guardo la tabla final
  library(R.utils)
  saveObject(tablafinalEP, "resultados/base_tabladinamicaEP.Rbin")
  
