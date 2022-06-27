

### **Importe a comprometer.**
```{r fila12a}
library(flexdashboard)
valueBox(paste("M",
               round(sum(as.numeric(tabla.para.indicadores$`Monto solicitado`, na.rm=T))/1000000,0),sep=""),
         #quantile(as.numeric(tablafinalEP$`Días desde autorización`), na.rm=T)[2],
         #quantile(as.numeric(tablafinalEP$`Días desde autorización`), na.rm=T)[4],
         icon="fa-calendar",col="red")
```




### **Procesos de adquisiciones en curso desde el momento en que se genera el expediente hasta el perfeccionamiento contractual, para todas las modalidades de compra y todas las fuentes de financiamiento**
```{r fila10a}
##A)Seleccion de variables 
library(flexdashboard)
valueBox("Incluye",
         #value = shiny::tags$p("Incluye", style = "font-size: 4px;"),
         icon="fa-comments", col="#303F9F")

```
