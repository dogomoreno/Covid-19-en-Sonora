rm(list=ls())

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
library(gtrendsR)
trend <- gtrends(keyword = c("pruebas covid"), geo = c("MX-SON"),  time = "2020-03-01 2022-01-05")$interest_over_time
trend <- trend %>% mutate(date=as.Date(date))


Sonora.DF <- read_csv("Bases/ST_SonoraReporte_SSFED.csv", 
                      col_types = cols(X1 = col_skip(), 
                                       fecha_reporte = col_date(format = "%Y-%m-%d")))

Sonora.Defuncion <- read_csv("Bases/ST_SonoraDefuncion_SSFED.csv", 
                             col_types = cols(fecha_def = col_date(format = "%Y-%m-%d"))) %>% rename(Fecha=fecha_def) %>% filter(Fecha>as.Date("2020-03-03"))
Sonora.Sintomas <- read_csv("Bases/ST_SonoraSintomas_SSFED.csv", 
                            col_types = cols(fecha_sintomas = col_date(format = "%Y-%m-%d"))) %>% rename(Fecha=fecha_sintomas) %>% filter(Fecha>as.Date("2020-03-03"))

dia<-max(as.Date(Sonora.DF$fecha_reporte))

lundom <- "domingo"
Fechasem <- paste0("Al reporte del ", day(dia), " de ", months.Date(dia)," de ", year(dia)," | Confirmados acumulados de ",weekdays(dia+1)," a ", weekdays(dia))
Fechadom <- paste0("Al reporte del ", day(dia), " de ", months.Date(dia)," de ", year(dia),"  | Cifras al ", weekdays(dia)," de cada semana.")
Fechahoy <- paste0("Al reporte del ", day(dia), " de ", months.Date(dia)," de ", year(dia))
fuente <- "Elaboración Luis Armando Moreno con información de la Secretaría de Salud del Gobierno de la República\n*Por fecha de reporte. | www.luisarmandomoreno.com"
fuentefech <- "Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud del Gobierno de la República\n*Los últimos 14 días aún se encuentran en proceso.| www.luisarmandomoreno.com"
fuentedes <- "Elaboración Luis Armando Moreno con información de la Secretaría de Salud del Gobierno de la República\n* www.luisarmandomoreno.com"
fuenteedad <- "Elaboración Luis Armando Moreno con información de la Secretaría de Salud del Gobierno de la República\n*Por fecha de reporte, negativos por ajustes. | www.luisarmandomoreno.com"
fuentepruebas <- "Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud del Gobierno de la República\n*Incluye resultados válidos de pruebas PCR y antigénicas | www.luisarmandomoreno.com"

temaejes <- theme(plot.margin = margin(10, 25, 10, 25),
                  plot.title = element_markdown(family = "Lato Black", size = 25),  
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
                    plot.subtitle = element_text(family = "Lato Light", size = 10, color = "black"), legend.title = element_blank(),
                    strip.text = element_text(family = "Lato Black", size = 8, hjust=0),
                    axis.text = element_text(family = "Lato", size =4),
                    plot.background = element_rect(fill = "white", color = "white", size = 3),
                    axis.title.x = element_text(family = "Lato Light", size = 12, hjust=1),
                    axis.title.y = element_text(family = "Lato Light", size = 12, hjust=1), 
                    plot.caption = element_text(family = "Lato", size = 6),
                    legend.text = element_blank(), legend.background =  element_rect(fill="transparent", color="transparent"),
                    legend.box.background=  element_rect(fill="transparent", color="transparent" ), 
                    legend.key = element_rect(fill="transparent", color="transparent"),
                    legend.position = "none", plot.title.position = 'plot', plot.caption.position = 'plot')


Sonora.DF <- read_csv("Bases/ST_SonoraReporte_SSFED.csv", 
                      col_types = cols(fecha_reporte = col_date(format = "%Y-%m-%d"))) %>% 
  rename(Fecha=fecha_reporte, Pruebas=Resultados_lab, Confirmados=Casos) 
Sonora.DF <- mutate(Sonora.DF, Positividad.acum= round((Confirmados_lab / Pruebas)*100,1))
dia.act <- max(as.Date(Sonora.DF$Fecha))-14

# Gráfico estatal
Casossemana <- Sonora.DF %>% mutate(diasemana = weekdays(Fecha)) %>% 
  filter(diasemana==lundom)

Casossemana <- mutate(Casossemana, Analizados.semana= Analizados - lag(Analizados, default = Analizados[1], order_by=Fecha))
Casossemana <- mutate(Casossemana, Casos.semana= Confirmados - lag(Confirmados, default = Confirmados[1], order_by=Fecha))
Casossemana <- mutate(Casossemana, Confirmados.semana= Confirmados_lab - lag(Confirmados_lab, default = Confirmados_lab[1], order_by=Fecha))
Casossemana <- mutate(Casossemana, Decesos.semana= Decesos - lag(Decesos, default = Decesos[1], order_by=Fecha))
Casossemana <- mutate(Casossemana, Pruebas.semana= Pruebas - lag(Pruebas, default = Pruebas[1]))
Casossemana <- mutate(Casossemana, Sospechosos.semana= Sospechosos - lag(Sospechosos, default = Sospechosos[1]))
Casossemana <- mutate(Casossemana, Recuperados.semana= Recuperados - lag(Recuperados, default = Recuperados[1]))
Casossemana <- mutate(Casossemana, Positividad.semanal= round((Confirmados.semana / Pruebas.semana)*100,1))
Casossemana <- mutate(Casossemana, ISSSTESON= C_Estatal - lag(C_Estatal, default = C_Estatal[1], order_by=Fecha))
Casossemana <- mutate(Casossemana, C_0_14= C_0_14 - lag(C_0_14, default = C_0_14[1], order_by=Fecha))
Casossemana <- mutate(Casossemana, C_15_29= C_15_29 - lag(C_15_29, default = C_15_29[1], order_by=Fecha))
Casossemana <- mutate(Casossemana, C_30_59= C_30_59 - lag(C_30_59, default = C_30_59[1], order_by=Fecha))
Casossemana <- mutate(Casossemana, C_60_mas= C_60_mas - lag(C_60_mas, default = C_60_mas[1], order_by=Fecha))
Casossemana <- mutate(Casossemana, D_0_14= D_0_14 - lag(D_0_14, default = D_0_14[1], order_by=Fecha))
Casossemana <- mutate(Casossemana, D_15_29= D_15_29 - lag(D_15_29, default = D_15_29[1], order_by=Fecha))
Casossemana <- mutate(Casossemana, D_30_59= D_30_59 - lag(D_30_59, default = D_30_59[1], order_by=Fecha))
Casossemana <- mutate(Casossemana, D_60_mas= D_60_mas - lag(D_60_mas, default = D_60_mas[1], order_by=Fecha))
Casossemana <- Casossemana %>% filter(Fecha>=as.Date("2020-03-01")) %>% mutate(pctj_menores=round(C_0_14*100/Casos.semana, 1)) 

Sonoraacum <- Sonora.DF %>% filter( Fecha == max(Fecha))

Fechas <- data.frame(Fecha=as.Date(Sonora.DF$Fecha)) 

Sintomassemanas <- Sonora.Sintomas %>% 
  full_join(Fechas, by="Fecha") %>%
  mutate_if(is.numeric,coalesce,0) %>% mutate(Casos.semana=rollsum(Casos, 7, align="right", fill = 0),
                                              Decesos.semana=rollsum(Decesos, 7, align="right", fill = 0),
                                              C_0_14.semana=rollsum(C_0_14, 7, align="right", fill = 0),
                                              C_15_29.semana=rollsum(C_15_29, 7, align="right", fill = 0),
                                              C_30_59.semana=rollsum(C_30_59, 7, align="right", fill = 0),
                                              C_60_mas.semana=rollsum(C_60_mas, 7, align="right", fill = 0),
                                              D_0_14.semana=rollsum(D_0_14, 7, align="right", fill = 0),
                                              D_15_29.semana=rollsum(D_15_29, 7, align="right", fill = 0),
                                              D_30_59.semana=rollsum(D_30_59, 7, align="right", fill = 0),
                                              D_60_mas.semana=rollsum(D_60_mas, 7, align="right", fill = 0)) %>% 
  select(Fecha, Casos.semana, Decesos.semana, C_0_14.semana, C_15_29.semana, C_30_59.semana, 
         C_60_mas.semana, D_0_14.semana, D_15_29.semana, D_30_59.semana, D_60_mas.semana) %>% 
  mutate(diasemana = weekdays(Fecha)) %>% 
  filter(diasemana==lundom) %>% mutate(pctj_menores=round(C_0_14.semana*100/Casos.semana, 2))

Decesossemanas <- Sonora.Defuncion %>% 
  full_join(Fechas, by="Fecha") %>%
  mutate_if(is.numeric,coalesce,0) %>% mutate(Casos.semana=rollsum(Casos, 7, align="right", fill = 0),
                                              Decesos.semana=rollsum(Decesos, 7, align="right", fill = 0),
                                              C_0_14.semana=rollsum(C_0_14, 7, align="right", fill = 0),
                                              C_15_29.semana=rollsum(C_15_29, 7, align="right", fill = 0),
                                              C_30_59.semana=rollsum(C_30_59, 7, align="right", fill = 0),
                                              C_60_mas.semana=rollsum(C_60_mas, 7, align="right", fill = 0),
                                              D_0_14.semana=rollsum(D_0_14, 7, align="right", fill = 0),
                                              D_15_29.semana=rollsum(D_15_29, 7, align="right", fill = 0),
                                              D_30_59.semana=rollsum(D_30_59, 7, align="right", fill = 0),
                                              D_60_mas.semana=rollsum(D_60_mas, 7, align="right", fill = 0)) %>% 
  select(Fecha, Decesos.semana, D_0_14.semana, D_15_29.semana, D_30_59.semana, D_60_mas.semana) %>% 
  mutate(diasemana = weekdays(Fecha)) %>% 
  filter(diasemana==lundom)

Casos2021 <- Casossemana 
Recuperados <- ggplot() +
  #geom_line(data= Casos2021, aes(x=Fecha, y= Casos.semana), color= "#01A2AC", linetype= "solid", size=1, alpha=0.8)+
  geom_line(data= Sintomassemanas, aes(x=Fecha, y= Casos.semana), color= "red", linetype= "solid", size=1, alpha=0.8)+
  geom_line(data= trend, aes(x=date, y= hits*33.2), color= "blue", linetype= "solid", size=1, alpha=0.8)+
  scale_y_continuous(expand = c(0, 0), limits = c(0,3500), name = "Casos", sec.axis = sec_axis(~./33.2, name="Interés (google)")) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-03-01"), (max(as.Date(Casos2021$Fecha)+60))), date_breaks = "1 month", date_labels = "%B") +
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_bw() +
  temaejes + theme(axis.title.y = element_markdown(family = "Lato", size =6)) +
  labs(y = NULL, 
       x = NULL,legend= NULL, 
       title  = "<span style = 'font-size:14pt'>Covid-19 en Sonora:</span><br><span style = 'color:red';>Inicio de síntomas</span> y <span style = 'color:blue';>búsqueda de pruebas</span> por semana", 
       subtitle= Fechasem, caption =fuente)
Recuperados
ggsave("Gráficos semanales/str.png",Recuperados, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)
