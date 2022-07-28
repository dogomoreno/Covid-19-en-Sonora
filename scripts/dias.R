# Paquetes

library(tidyverse)
library(extrafont)
library(scales)
library(plotly)
library(htmlwidgets)
library(tint)
library(rgdal)
library(rgeos)
library(ggiraph)
library(miniUI)
library(units)
library(reactable)
library(zoo)
library(lubridate)
library(treemapify)
library(wesanderson)
library(ggsci)
library("Cairo")
library(directlabels)
library(ggtext)
library(patchwork)

Sonora.DF <- read_csv("Bases/ST_SonoraReporte_SSFED.csv", 
                      col_types = cols(X1 = col_skip(), 
                                       fecha_reporte = col_date(format = "%Y-%m-%d")))


Dias <- read_csv("Bases/Dias.csv")
Dias[is.na(Dias)] <-0

Dias <- Dias %>% mutate(diferencia=casos24-casos17)

dia<-max(as.Date(Sonora.DF$fecha_reporte))

lundom <- weekdays(dia)
Fechasem <- paste0("Al reporte del ", day(dia), " de ", months.Date(dia)," de ", year(dia)," | Confirmados acumulados de ",weekdays(dia+1)," a ", weekdays(dia))
Fechaobs <- paste0("Al reporte del ", day(dia), " de ", months.Date(dia)," de ", year(dia)," | Ingresados acumulados de ",weekdays(dia+1)," a ", weekdays(dia))
Fechadom <- paste0("Al reporte del ", day(dia), " de ", months.Date(dia)," de ", year(dia),"  | Cifras al ", weekdays(dia)," de cada semana.")
Fechahoy <- paste0("Al reporte del ", day(dia), " de ", months.Date(dia)," de ", year(dia))
fuente <- "Elaboración Luis Armando Moreno con información de la Secretaría de Salud del Gobierno de la República\nwww.luisarmandomoreno.com"
fuentefech <- "Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud del Gobierno de la República\n*El registro de las últimas 2 semanas aún se encuentra en proceso.| www.luisarmandomoreno.com"
fuentedes <- "Elaboración Luis Armando Moreno con información de la Secretaría de Salud del Gobierno de la República\n* www.luisarmandomoreno.com"
fuenteedad <- "Elaboración Luis Armando Moreno con información de la Secretaría de Salud del Gobierno de la República\n*Por fecha de reporte, negativos por ajustes. | www.luisarmandomoreno.com"
fuentepruebas <- "Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud del Gobierno de la República\n*Incluye resultados válidos de pruebas PCR y antigénicas | www.luisarmandomoreno.com"
fuenteingreso<- "Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud del Gobierno de la República\n*El registro de las última semana aún se encuentra en proceso.| www.luisarmandomoreno.com"
temaejes <- theme(plot.margin = margin(10, 25, 10, 25),
                  plot.title = element_markdown(family = "Lato Black", size = 25), 
                  panel.grid=element_blank(), panel.border=element_blank(), 
                  axis.line= element_line(color = "black", size = 0.3), 
                  plot.subtitle = element_text(family = "Lato Light", size = 10, color = "black"), legend.title = element_blank(),
                  strip.text = element_text(family = "Lato Black", size = 10),
                  axis.text = element_text(family = "Lato", size =6),
                  plot.background = element_rect(fill = "white", color = "white", size = 3),
                  axis.title.x = element_text(family = "Lato Light", size = 8, hjust=1),
                  axis.title.y = element_text(family = "Lato Light", size = 8, hjust=1), 
                  plot.caption = element_text(family = "Lato", size = 6),
                  legend.text = element_blank(),
                  legend.position = "none", plot.title.position = 'plot', plot.caption.position = 'plot')

temasinejes <-  theme(axis.line = element_blank(),
                      plot.margin = margin(10, 25, 10, 25),
                      plot.title = element_markdown(family = "Lato Black", size = 25),  
                      plot.subtitle = element_text(family = "Lato Light", size = 10, color = "black"), legend.title = element_blank(),
                      axis.text.x = element_text(family = "Lato", size =6, angle=90, hjust=0.95,vjust=0.5),   panel.grid= element_blank(),
                      axis.text.y = element_blank(),
                      plot.background = element_rect(fill = "white", color = "white", size = 3),
                      axis.title.x = element_text(family = "Lato Light", size = 8, hjust=1),
                      axis.title.y = element_text(family = "Lato Light", size = 8, hjust=1), 
                      plot.caption = element_text(family = "Lato", size = 6, color = "black"),
                      legend.text = element_text(family = "Lato", size = 8),
                      legend.position = "none",  legend.justification="left", plot.title.position = 'plot', plot.caption.position = 'plot')

temadiario <- theme(plot.margin = margin(10, 25, 10, 25),
                    plot.title = element_markdown(family = "Lato Black", size = 25), 
                    panel.grid=element_blank(), panel.border=element_blank(), axis.line= element_line(color = "black", size = 0.3),
                    plot.subtitle = element_text(family = "Lato Light", size = 10, color = "black"), legend.title = element_blank(),
                    strip.text = element_text(family = "Lato Black", size = 8, hjust=0),
                    axis.text = element_text(family = "Lato", size =6),
                    plot.background = element_rect(fill = "white", color = "white", size = 3),
                    axis.title.x = element_text(family = "Lato Light", size = 12, hjust=1),
                    axis.title.y = element_text(family = "Lato Light", size = 12, hjust=1), 
                    plot.caption = element_text(family = "Lato", size = 6),
                    legend.text = element_blank(), legend.background =  element_rect(fill="transparent", color="transparent"),
                    legend.box.background=  element_rect(fill="transparent", color="transparent" ), 
                    legend.key = element_rect(fill="transparent", color="transparent"),
                    legend.position = "none", plot.title.position = 'plot', plot.caption.position = 'plot')


Casosfiltro <- Dias %>% filter(fecha_sintomas>=as.Date("2022-01-01")) 

Casossemson <- Casosfiltro %>% ggplot() +
  geom_col(aes(x=fecha_sintomas, y= casos24), fill="#005156", color= "white") +
  geom_col(aes(x=fecha_sintomas, y= casos17), fill="#01A2AC", color= "white") +
  geom_text(data=subset(Casosfiltro, diferencia>0), aes(x=fecha_sintomas, y= casos24 + 30, 
                label= diferencia), family="Lato Black", size= 1.8, color="#005156", hjust=0.5) +
  annotation_custom(SeD, xmin = as.Date("2022-03-31"), xmax = as.Date("2022-04-08"), ymin = 2300, ymax = 2600) +
  geom_text(aes(x=as.Date("2022-02-08"), y= 1000, 
                label= "De los 467 casos confirmados\ndurante la última semana,\nsólo 61 iniciaron síntomas\na partir del mes de marzo\nde 2022 (13%)."), family="Lato Black", size= 5, color="#005156", hjust=0) +
  # geom_text( data=subset(Casosfiltro, Casos.semana<500), aes(x=Fecha, y= Casos.semana, label= Casos.semana), family="Lato Black", size= 3, color="#01A2AC", angle=90, hjust = -0.2) +
  scale_x_date(expand=c(0,5), date_breaks = "1 week", labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                                                                    paste(day(x),"\n",month(x, label = TRUE), "\n", year(x)), 
                                                                                    paste(day(x),"\n",month(x, label = TRUE)))) +
  # scale_x_date(expand=c(0,5), date_breaks = "1 month", date_labels = "%B") +
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_minimal() +
  temaejes +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Sonora:</span><br><span style = 'color:#01A2AC';>Casos confirmados por fecha de inicio de síntomas </span>", 
       subtitle= paste0("Casos confirmados por fecha de inicio de síntomas a partir del 2022 \nAl reporte del ", day(dia), " de ", months.Date(dia)," de ", year(dia)), caption =fuente)
Casossemson
ggsave("Gráficos diarios/diasSon.png",Casossemson, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


filtro<- Dias %>% filter(fecha_sintomas>=as.Date("2022-03-01")) %>% summarise(suma=sum(diferencia))



ggplot(mapping = aes(x = 0:1, y = 1)) +
  theme_void() +
  annotation_custom(SeD, xmin = .8, xmax = 1)
