

library(R.utils)
#importo base SCO
tablafinalEP<-loadObject("resultados/base_tabladinamicaEP.Rbin")
#importo base TRATAMIENTOS
tabla.tramitaciones.doc<-loadObject("resultados/tabla.tramitaciones.doc.Rbin")
library(dplyr)
tabla.para.indicadores1<-tablafinalEP %>% 
  select(`Estado Pliego`, reparticion.actual.agrup,
         `Días desde autorización`, dias_ultimo_pase,MontoSolic)
tabla.tramitaciones.doc$MontoSolic<-0
tabla.para.indicadores2<-tabla.tramitaciones.doc %>% 
  select(`Etapa del proceso`, `Repartición actual`,
         `Dias desde el inicio`, `Días desde el último pase`,MontoSolic)

nombres<-c("Etapa del proceso", "reparticion.actual.agrup", 
           "Dias desde el inicio", "Días desde el último pase","Monto solicitado")
names(tabla.para.indicadores1)<-nombres
names(tabla.para.indicadores2)<-nombres
tabla.para.indicadores<-rbind(tabla.para.indicadores1, tabla.para.indicadores2)

tabla.etapaproc<-tabla.para.indicadores %>% 
  filter(`Etapa del proceso`=="5-Preadjudicación" | `Etapa del proceso`=="1-Inicial") %>%
  group_by(`Etapa del proceso`,reparticion.actual.agrup) %>% 
  summarise(`Cantidad de expedientes`=n())

library(ggplot2)
install.packages("treemapify")
install.packages("forcats")
library(treemapify)
library(waffle)
library(forcats)

View(tabla.etapaproc)
class(tabla.etapaproc$`Cantidad de expedientes`)

comunas_plot <- c(`Compras` = 31, `DAL` = 12, `DGA` = 2, `DGPFI` = 13,`Programas` = 26)
g <- waffle(comunas_plot,
            rows = 6,
            colors = c("#3182BD", "#6BAED6", "#9ECAE1", "#C6DBEF", "#E6550D"),
            legend_pos = "right",
            title = "Avisos de Airbnb por comuna en CABA, abril 2019")
plot(g)

library(RColorBrewer)

ggplot(tabla.etapaproc,aes(y=reparticion.actual.agrup,x=`Cantidad de expedientes`,fill=reparticion.actual.agrup))+
  geom_col(position = "dodge")+
  scale_fill_manual(values=brewer.pal(n = 7, name = "Accent"))+ 
  geom_text(aes(label = `Cantidad de expedientes`),position=position_stack(vjust=,0.5), colour="black")+
  #coord_flip() +
  #theme(legend.position = "none")+
  #theme(axis.text.x= element_text(angle = 90)) +
  theme(margin(0.25, 1, 0.25, 0.1, "cm"), legend.key.size = unit(0.25, "cm"))+ 
  labs(title = "Cantidad procesos según repartición actual")+
  labs(fill="")+
  theme(legend.position = "none")+  
  theme_minimal() +
  theme(legend.position = "none")  +
  theme(axis.title.y = element_blank(),
  axis.title.x = element_blank())+
  facet_wrap(~ `Etapa del proceso`, nrow = 2)


theTable <- data.frame(
  Name = c('James', 'Frank', 'Jean', 'Steve', 'John', 'Tim'),
  Position = c('Zoalkeeper', 'Zoalkeeper', 'Defense',
               'Defense', 'Defense', 'Striker'))
View(theTable)

theTable %>%
  count(Position) %>%
  mutate(Position = fct_reorder(Position, n, .desc = TRUE)) %>%
  ggplot(aes(x = Position, y = n)) + geom_bar(stat = 'identity')
