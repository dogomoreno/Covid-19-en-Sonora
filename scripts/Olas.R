rm(list=ls())

# Paquetes

library(tidyverse)
library(extrafont)
library(scales)
library(zoo)
library(lubridate)
library("Cairo")
library(directlabels)
library(ggtext)
library(geofacet)


Sonora.DF <- read_csv("Bases/ST_SonoraReporte_SSFED.csv", 
                      col_types = cols(X1 = col_skip(), 
                                       fecha_reporte = col_date(format = "%Y-%m-%d"))) %>% rename(Fecha=fecha_reporte)
dia<-max(as.Date(Sonora.DF$Fecha))
lundom <- "domingo"


Sonora.Defuncion <- read_csv("Bases/ST_SonoraDefuncion_SSFED.csv", 
                             col_types = cols(fecha_def = col_date(format = "%Y-%m-%d"))) %>% rename(Fecha=fecha_def) %>% filter(Fecha>as.Date("2020-03-03"))
Sonora.Sintomas <- read_csv("Bases/ST_SonoraSintomas_SSFED.csv", 
                            col_types = cols(fecha_sintomas = col_date(format = "%Y-%m-%d"))) %>% rename(Fecha=fecha_sintomas) %>% filter(Fecha>as.Date("2020-03-03"))

Sonora.Ingreso <- read_csv("Bases/ST_SonoraIngreso_SSFED.csv", 
                           col_types = cols(fecha_ingreso = col_date(format = "%Y-%m-%d"))) %>% rename(Fecha=fecha_ingreso) %>% filter(Fecha>as.Date("2020-03-03"))

dia.ev <- dia-14
dia.act <- max(as.Date(Sonora.DF$Fecha))-14

lundom <- "domingo"
Fechasem <- paste0("Al reporte del ", day(dia), " de ", months.Date(dia)," de ", year(dia)," | Confirmados acumulados de ",weekdays(dia+1)," a ", weekdays(dia))
Fechaobs <- paste0("Al reporte del ", day(dia), " de ", months.Date(dia)," de ", year(dia)," | Ingresados acumulados de ",weekdays(dia+1)," a ", weekdays(dia))
Fechadom <- paste0("Al reporte del ", day(dia), " de ", months.Date(dia)," de ", year(dia),"  | Cifras al ", weekdays(dia)," de cada semana.")
Fechahoy <- paste0("Al reporte del ", day(dia), " de ", months.Date(dia)," de ", year(dia))
fuente <- "Elaboración Luis Armando Moreno con información de la Secretaría de Salud del Gobierno de la República\n*Por fecha de reporte. | www.luisarmandomoreno.com"
fuentefech <- paste0("Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud del Gobierno de la República\n*",Fechahoy,", el registro de las últimas 2 semanas aún se encuentra en proceso.| www.luisarmandomoreno.com")
fuentedes <- "Elaboración Luis Armando Moreno con información de la Secretaría de Salud del Gobierno de la República\n* www.luisarmandomoreno.com"
fuenteedad <- "Elaboración Luis Armando Moreno con información de la Secretaría de Salud del Gobierno de la República\n*Por fecha de reporte, negativos por ajustes. | www.luisarmandomoreno.com"
fuentepruebas <- "Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud del Gobierno de la República\n*Incluye resultados válidos de pruebas PCR y antigénicas | www.luisarmandomoreno.com"
fuenteingreso<- "Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud del Gobierno de la República\n*El registro de las última semana aún se encuentra en proceso.| www.luisarmandomoreno.com"
temaejes <- theme(plot.margin = margin(10, 25, 10, 25),
                  plot.title = element_markdown(family = "Lato Black", size =  46), 
                  panel.grid=element_blank(), panel.border=element_blank(), 
                  axis.line= element_line(color = "black", size = 0.3), 
                  plot.subtitle = element_text(family = "Lato Light", size = 10, color = "black"), legend.title = element_blank(),
                  strip.text = element_text(family = "Lato Black", size = 10),
                  axis.text = element_text(family = "Lato", size =6),
                  plot.background = element_rect(fill = "white", color = "white", size = 3),
                  axis.title.x = element_text(family = "Lato Light", size = 8, hjust=1),
                  axis.title.y = element_text(family = "Lato Light", size = 8, hjust=1), 
                  plot.caption = element_text(family = "Lato", size = 14),
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

# Gráfico estatal

Fechas <- data.frame(Fecha=as.Date(Sonora.DF$Fecha)) 

Sintomassemanas <- Sonora.Sintomas %>% 
  full_join(Fechas, by="Fecha") %>%
  mutate_if(is.numeric,coalesce,0) %>% mutate(Casos.semana=rollsum(Casos, 7, align="right", fill = 0),
                                              Decesos.semana=rollsum(Decesos, 7, align="right", fill = 0),
                                              Hospitalizados.semana=rollsum(Hospitalizados, 7, align="right", fill = 0),
                                              C_0_14.semana=rollsum(C_0_14, 7, align="right", fill = 0),
                                              C_15_29.semana=rollsum(C_15_29, 7, align="right", fill = 0),
                                              C_30_59.semana=rollsum(C_30_59, 7, align="right", fill = 0),
                                              C_60_mas.semana=rollsum(C_60_mas, 7, align="right", fill = 0),
                                              D_0_14.semana=rollsum(D_0_14, 7, align="right", fill = 0),
                                              D_15_29.semana=rollsum(D_15_29, 7, align="right", fill = 0),
                                              D_30_59.semana=rollsum(D_30_59, 7, align="right", fill = 0),
                                              D_60_mas.semana=rollsum(D_60_mas, 7, align="right", fill = 0),
                                              Confirmados_lab.semana=rollsum(Confirmados_lab, 7, align="right", fill = 0),
                                              Resultados_lab.semana=rollsum(Resultados_lab, 7, align="right", fill = 0)) %>% 
  select(Fecha, Casos.semana, Decesos.semana,Hospitalizados.semana, C_0_14.semana, C_15_29.semana, C_30_59.semana, 
         C_60_mas.semana, D_0_14.semana, D_15_29.semana, D_30_59.semana, D_60_mas.semana,Confirmados_lab.semana,Resultados_lab.semana) %>% 
  mutate(diasemana = weekdays(Fecha)) %>% 
  filter(diasemana==lundom) %>% filter(Fecha>=(as.Date("2020-03-15")))

Sintomassemanas <- Sintomassemanas %>% 
  mutate(Ola=if_else(Fecha<=as.Date("2020-11-01"),"A",
                     if_else(Fecha<=as.Date("2021-05-09"),"B",
                             if_else(Fecha<=as.Date("2021-10-31"),"C","D"))))
Olas <- Sintomassemanas %>% 
  group_by(Ola) %>% summarise(Inicio=min(Fecha)-6, Fin=max(Fecha), Casos=sum(Casos.semana), Decesos=sum(Decesos.semana), 
                              Hospitalizados=sum(Hospitalizados.semana),
                              C_0_14=sum(C_0_14.semana),
                              C_15_29=sum(C_15_29.semana),
                              C_30_59=sum(C_30_59.semana),
                              C_60_mas=sum(C_60_mas.semana),
                              D_0_14=sum(D_0_14.semana),
                              D_15_29=sum(D_15_29.semana),
                              D_30_59=sum(D_30_59.semana),
                              D_60_mas=sum(D_60_mas.semana),
                              Confirmados_lab=sum(Confirmados_lab.semana),
                              Resultados_lab=sum(Resultados_lab.semana), Casos.max=max(Casos.semana), 
                              Decesos.max=max(Decesos.semana),
                              Hospitalizados.max=max(Hospitalizados.semana)) %>% 
  mutate(Dias=as.numeric(Fin-Inicio), Positividad=round(Confirmados_lab*100/Resultados_lab,1), 
         Letalidad=round(Decesos*100/Casos,1), Hospitalización=round(Hospitalizados*100/Casos,1), 
         Letalidad.Hosp=round(Decesos*100/Hospitalizados,1), Casos.dia=round(Casos/Dias,1),Decesos.dia=round(Decesos/Dias,1))
write.csv(Olas,'ResultadoCSV/Olas.csv', row.names = FALSE)

