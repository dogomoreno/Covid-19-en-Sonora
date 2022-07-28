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

Hermosillo.DF <- read_csv("Bases/ST_HermosilloReporte_SSFED.csv", 
                      col_types = cols(X1 = col_skip(), 
                                       fecha_reporte = col_date(format = "%Y-%m-%d")))

Hermosillo.Defuncion <- read_csv("Bases/ST_HermosilloDefuncion_SSFED.csv", 
                             col_types = cols(fecha_def = col_date(format = "%Y-%m-%d"))) %>% rename(Fecha=fecha_def) %>% filter(Fecha>as.Date("2020-03-03"))
Hermosillo.Sintomas <- read_csv("Bases/ST_HermosilloSintomas_SSFED.csv", 
                            col_types = cols(fecha_sintomas = col_date(format = "%Y-%m-%d"))) %>% rename(Fecha=fecha_sintomas) %>% filter(Fecha>as.Date("2020-03-03"))

dia<-max(as.Date(Hermosillo.DF$fecha_reporte))

lundom <- weekdays(dia)
Fechasem <- paste0("Al reporte del ", day(dia), " de ", months.Date(dia)," de ", year(dia)," | Confirmados acumulados de ",weekdays(dia+1)," a ", weekdays(dia))
Fechadom <- paste0("Al reporte del ", day(dia), " de ", months.Date(dia)," de ", year(dia),"  | Cifras al ", weekdays(dia)," de cada semana.")
Fechahoy <- paste0("Al reporte del ", day(dia), " de ", months.Date(dia)," de ", year(dia))
fuente <- "Elaboración Luis Armando Moreno con información de la Secretaría de Salud del Gobierno de la República\n*Por fecha de reporte. | www.luisarmandomoreno.com"
fuentefech <- "Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud del Gobierno de la República\n*El registro de las últimas 2 semanas aún se encuentra en proceso.| www.luisarmandomoreno.com"
fuentedes <- "Elaboración Luis Armando Moreno con información de la Secretaría de Salud del Gobierno de la República\n* www.luisarmandomoreno.com"
fuenteedad <- "Elaboración Luis Armando Moreno con información de la Secretaría de Salud del Gobierno de la República\n*Por fecha de reporte, negativos por ajustes. | www.luisarmandomoreno.com"
fuentepruebas <- "Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud del Gobierno de la República\n*Incluye resultados válidos de pruebas PCR y antigénicas | www.luisarmandomoreno.com"

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


# Carga base estatal
Hermosillo.DF <- read_csv("Bases/ST_HermosilloReporte_SSFED.csv", 
                      col_types = cols(fecha_reporte = col_date(format = "%Y-%m-%d"))) %>% 
  rename(Fecha=fecha_reporte, Pruebas=Resultados_lab, Confirmados=Casos) 
Hermosillo.DF <- mutate(Hermosillo.DF, Positividad.acum= round((Confirmados_lab / Pruebas)*100,1))
dia.act <- max(as.Date(Hermosillo.DF$Fecha))-14

# Gráfico estatal
Casossemana <- Hermosillo.DF %>% mutate(diasemana = weekdays(Fecha)) %>% 
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
Casossemana <- Casossemana %>% filter(Fecha>=as.Date("2020-10-15")) %>% mutate(pctj_menores=round(C_0_14*100/Casos.semana, 1)) 

Hermosilloacum <- Hermosillo.DF %>% filter( Fecha == max(Fecha))

Fechas <- data.frame(Fecha=as.Date(Hermosillo.DF$Fecha)) 

Sintomassemanas <- Hermosillo.Sintomas %>% 
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

Decesossemanas <- Hermosillo.Defuncion %>% 
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







# Gráfico Treemap confirmados estatales
Hermosillo.DF.hoy <- filter(Hermosillo.DF, Fecha == max(Fecha))
Hermosillo.DF.hoy <- select(Hermosillo.DF.hoy, Hospitalizados.Activos, Ambulatorios.Activos, Decesos, Recuperados)
Hermosillo.DF.hoy <- rename(Hermosillo.DF.hoy, "Ambulatorios\nactivos"= Ambulatorios.Activos, "Hospitalizados activos"=Hospitalizados.Activos)
Hermosillo.DF.hoy <- gather(Hermosillo.DF.hoy, key= Estatus, value= Casos.confirmados) 
tituestatus <- paste0("<span style = 'font-size:14pt'>Covid-19 en Hermosillo:</span><br>Estatus de los <span style = 'color:#01A2AC';>", prettyNum(as.numeric(sum(Hermosillo.DF.hoy$Casos.confirmados)), big.mark=",", preserve.width="none"), "</span> casos confirmados")


Hermosillo.DF.hoy$label <- c(paste0(Hermosillo.DF.hoy$Estatus, "\n ",prettyNum(as.numeric(Hermosillo.DF.hoy$Casos.confirmados), big.mark=",", preserve.width="none"), "\n ", (round((Hermosillo.DF.hoy$Casos.confirmados/(sum(Hermosillo.DF.hoy$Casos.confirmados))*100), digits = 1)),"%"))
Hosp.hoy <- Hermosillo.DF.hoy %>% filter(Estatus=="Hospitalizados activos")

hosplab <- Hosp.hoy$label

Estatus <- ggplot(Hermosillo.DF.hoy, aes(area = Casos.confirmados, fill= Estatus, label= Hermosillo.DF.hoy$label)) + geom_treemap( size=2, color= "white") +
  scale_fill_manual(values= c("#01A2AC", "#993366", "#F79646", "#4BACC6")) +
  scale_color_manual(values=c("#005156","#4D1933", "#984807",  "#215968" )) +
  geom_treemap_text(family = "Lato Black", colour = "white", place = "topleft",
                    grow = FALSE, size=10) +  theme_void() +
  #annotate(geom = "text", x = 0.93, y = 0.99, label = hosplab, color= "white", family= "Lato Black",  hjust=1, size=2) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_void() + temasinejes +
  theme(plot.tag = element_text(family = "Lato Black", size = 6,color = "#F79646",hjust=1),
        plot.tag.position = c(1, 0.84), axis.line.x = element_blank(), axis.text.x = element_blank())+
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = tituestatus, tag = hosplab, 
       subtitle= Fechahoy, caption =fuente)
Estatus

ggsave("Gráficos semanales/Hermosillo/s02.png",Estatus , width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


# Casos semanal Estatal

Casossemson <- ggplot(Casossemana) +
  geom_col(aes(x=Fecha, y= Casos.semana, fill= Casos.semana), color= "#005156", size=0.15) +
  scale_fill_gradient2(low = "#DEF2F2", mid= "#01A2AC", high = "#005156", midpoint = 1500) +
  geom_text( aes(x=Fecha, y= Casos.semana, label= Casos.semana), family="Lato Black", size= 3, color="white", angle=90, hjust = 1.1) +
  scale_x_date(expand=c(0,5), breaks = Casossemana$Fecha, date_labels = "%d/%m") +
  # scale_x_date(expand=c(0,5), date_breaks = "1 month", date_labels = "%B") +
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_minimal() +
  temasinejes +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo:</span><br><span style = 'color:#01A2AC';>Casos confirmados semanalmente</span>", 
       subtitle= Fechasem, caption =fuente)
Casossemson
ggsave("Gráficos semanales/Hermosillo/s03.png",Casossemson, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)

CasossemSintomas <- ggplot(Sintomassemanas) +
  geom_col(data=subset(Sintomassemanas, Fecha <=dia.act),aes(x=Fecha, y= Casos.semana, fill= Casos.semana), color= "#005156", size=0.15) +
  geom_col(data=subset(Sintomassemanas, Fecha >dia.act),aes(x=Fecha, y= Casos.semana, fill= Casos.semana), color= "#005156", size=0.15) +
  scale_fill_gradient2(low = "#DEF2F2", mid= "#01A2AC", high = "#005156", midpoint = 1500) +
  geom_text( aes(x=Fecha, y= Casos.semana, label= Casos.semana), family="Lato Black", size= 2.2, color="white", angle=90, hjust = 1.1) +
  scale_x_date(expand=c(0,5), breaks = Sintomassemanas$Fecha, date_labels = "%d/%m") +
  # scale_x_date(expand=c(0,5), date_breaks = "1 month", date_labels = "%B") +
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_minimal() +
  temasinejes + 
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo:</span><br><span style = 'color:#01A2AC';>Casos confirmados por semana de inicio de síntomas</span>", 
       subtitle= Fechasem, caption =fuentefech)
CasossemSintomas

ggsave("Gráficos semanales/Hermosillo/s03sint.png",CasossemSintomas, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


# Decesos diarios Estatal

Decesossemson <- ggplot(Casossemana) +
  geom_col(aes(x=Fecha, y= Decesos.semana, fill= Decesos.semana), color= "#4D1933", size=0.15) +
  scale_fill_gradient2(low = "#F0D1E0", mid= "#993366", high = "#4D1933", midpoint = 200) +
  geom_text( aes(x=Fecha, y= Decesos.semana, label= Decesos.semana), family="Lato Black", size= 3, color="white", angle=90, hjust = 1.1) +
  scale_x_date(expand=c(0,5), breaks = Casossemana$Fecha, date_labels = "%d/%m") +
  # scale_x_date(expand=c(0,5), date_breaks = "1 month", date_labels = "%B") +
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_minimal() +
  temasinejes +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo:</span><br><span style = 'color:#993366';>Decesos confirmados semanalmente</span>", 
       subtitle= Fechasem, caption =fuente)
Decesossemson

ggsave("Gráficos semanales/Hermosillo/s09.png",Decesossemson, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)

DecesosDefuncion <- ggplot(Decesossemanas) +
  geom_col(data=subset(Decesossemanas, Fecha <=dia.act), aes(x=Fecha, y= Decesos.semana, fill= Decesos.semana), color= "#4D1933", size=0.15) +
  geom_col(data=subset(Decesossemanas, Fecha > dia.act), aes(x=Fecha, y= Decesos.semana, fill= Decesos.semana), color= "#4D1933", size=0.15) +
  scale_fill_gradient2(low = "#F0D1E0", mid= "#993366", high = "#4D1933", midpoint = 200) +
  geom_text( aes(x=Fecha, y= Decesos.semana, label= Decesos.semana), family="Lato Black", size= 2.2, color="white", angle=90, hjust = 1.1) +
  scale_x_date(expand=c(0,5), breaks = Decesossemanas$Fecha, date_labels = "%d/%m") +
  # scale_x_date(expand=c(0,5), date_breaks = "1 month", date_labels = "%B") +
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_minimal() +
  temasinejes +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo:</span><br><span style = 'color:#993366';>Decesos confirmados por semana de ocurridos</span>", 
       subtitle= Fechasem, caption =fuentefech)
DecesosDefuncion


ggsave("Gráficos semanales/Hermosillo/s09d.png",DecesosDefuncion, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)

# Hospitalizados diarios Estatal

Hospsemana <- Casossemana %>% filter(Fecha >= as.Date("2020-10-15"))
Hospsemson <- ggplot(Hospsemana) +
  geom_col(aes(x=Fecha, y= Hospitalizados.Activos, fill= Hospitalizados.Activos), color= "#984807", size=0.15) +
  scale_fill_gradient2(low = "#F79646", mid=  "#E46C0A", high = "#984807", midpoint = 100) +
  geom_text( aes(x=Fecha, y= Hospitalizados.Activos, label= Hospitalizados.Activos), family="Lato Black", size= 3, color="white", angle=90, hjust = 1.1) +
  scale_x_date(expand=c(0,5), breaks = Hospsemana$Fecha, date_labels = "%d/%m") +
  # scale_x_date(expand=c(0,5), date_breaks = "1 month", date_labels = "%B") +
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_minimal() +
  temasinejes +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo:</span><br><span style = 'color:#F79646';>Casos activos hospitalizados al cierre de semana</span>", 
       subtitle= paste0(Fechadom, "\nSe considera caso activo aquel que inicio síntomas dentro de los 14 días previos a la fecha de corte."), caption =fuente)
 Hospsemson

ggsave("Gráficos semanales/Hermosillo/s04.png",Hospsemson, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)

# Activos leves

Activossem <- Casossemana %>% filter(Fecha >= as.Date("2020-10-15"))
Activos <- ggplot(Activossem) +
  geom_col(aes(x=Fecha, y= Ambulatorios.Activos, fill= Ambulatorios.Activos), color= "#3B9494", size=0.15) +
  scale_fill_gradient2(low = "#BCE4E4", mid= "#58BCBC", high = "#3B9494", midpoint = 1500) +
  geom_text( aes(x=Fecha, y= Ambulatorios.Activos, label= Ambulatorios.Activos), family="Lato Black", size= 3, color="white", angle=90, hjust = 1.1) +
  scale_x_date(expand=c(0,5), breaks = Casossemana$Fecha, date_labels = "%d/%m") +
  # scale_x_date(expand=c(0,5), date_breaks = "1 month", date_labels = "%B") +
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_minimal() +
  temasinejes +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo:</span><br><span style = 'color:#58BCBC';>Ambulatorios activos al cierre de semana</span>", 
       subtitle= paste0(Fechadom, "\nSe considera caso activo aquel que inicio síntomas dentro de los 14 días previos a la fecha de corte."), caption =fuente)
Activos

ggsave("Gráficos semanales/Hermosillo/s05.png",Activos, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


Activossem <- Casossemana %>% filter(Fecha >= as.Date("2020-10-15"))
Activos <- ggplot(Activossem) +
  geom_col(aes(x=Fecha, y= Activos, fill= Activos), color= "#3B9494", size=0.15) +
  scale_fill_gradient2(low = "#BCE4E4", mid= "#58BCBC", high = "#3B9494", midpoint = 1500) +
  geom_text( aes(x=Fecha, y= Activos, label= Activos), family="Lato Black", size= 3, color="white", angle=90, hjust = 1.1) +
  scale_x_date(expand=c(0,5), breaks = Casossemana$Fecha, date_labels = "%d/%m") +
  # scale_x_date(expand=c(0,5), date_breaks = "1 month", date_labels = "%B") +
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_minimal() +
  temasinejes +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo:</span><br><span style = 'color:#58BCBC';>Casos activos al cierre de semana</span>", 
       subtitle= paste0(Fechadom, "\nSe considera caso activo aquel que inicio síntomas dentro de los 14 días previos a la fecha de corte."), caption =fuente)
Activos

ggsave("Gráficos semanales/Hermosillo/s28.png",Activos, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)

# Pruebas semanales

Pruebassemson <- ggplot(Casossemana) +
  geom_col(aes(x=Fecha, y= Pruebas.semana, fill= Pruebas.semana), color="#215968", size=0.15) +
  scale_fill_gradient2(low = "#B7DEE8", mid= "#4BACC6", high = "#215968", midpoint = 2300) +
  geom_text( aes(x=Fecha, y= Pruebas.semana, label= Pruebas.semana), family="Lato Black", size= 3, color="white", angle=90, hjust = 1.1) +
  scale_x_date(expand=c(0,5), breaks = Casossemana$Fecha, date_labels = "%d/%m") +
  # scale_x_date(expand=c(0,5), date_breaks = "1 month", date_labels = "%B") +
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_minimal() +
  temasinejes +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo:</span><br><span style = 'color:#4BACC6';>Resultados válidos de pruebas semanales</span>", 
       subtitle= Fechasem, caption =fuentepruebas)
Pruebassemson

ggsave("Gráficos semanales/Hermosillo/s11.png",Pruebassemson, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)

Semanamax <- Casossemana %>% filter(Fecha==max(Fecha))
Sospsemson <- ggplot(Casossemana) +
  geom_line(aes(x=Fecha, y= Sospechosos), color="#A96AC2", size=1, alpha=0.75) +
  geom_point( data = subset(Casossemana , Fecha == max(Fecha)), aes(x=Fecha, y= Sospechosos), fill="#A96AC2", size=2 , shape=21, color="white", stroke=1) +
  geom_dl( data = subset(Casossemana , Fecha == max(Fecha)), 
           aes(x=Fecha, y= Sospechosos, label = paste0("\n\n\n", Sospechosos,"\nsospechosos\n(", if_else(Semanamax$Sospechosos.semana<0,"","+"),Semanamax$Sospechosos.semana," respecto a la\nsemana anterior)", sep="")), color="#A96AC2", 
           method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.8, fontfamily= "Lato Black")) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-10-15"), (max(as.Date(Casossemana$Fecha)) + 80)), 
               date_breaks = "1 month", labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                                                     paste(month(x, label = TRUE), "\n", year(x)),
                                                                     paste(month(x, label = TRUE)))) +
  # scale_x_date(expand=c(0,5), date_breaks = "1 month", date_labels = "%B") +
  scale_y_continuous(expand=c(0,0), limits=c(1000,8000))+
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_bw() +
  temaejes +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo:</span><br><span style = 'color:#A96AC2';>Casos sospechosos al cierre de semana</span>", 
       subtitle= Fechadom, caption =fuente)
Sospsemson

ggsave("Gráficos semanales/Hermosillo/s26.png",Sospsemson, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)

 #Positividad

library(directlabels)
Positividad <- ggplot() +
  geom_line(data= Hermosillo.DF, aes(x=Fecha, y= Positividad.acum), color= "#215968", linetype= "solid", size=1, alpha=0.6)+
  geom_line(data= Casossemana, aes(x=Fecha, y= Positividad.semanal), color= "#4BACC6", linetype= "solid", size=1, alpha=0.6)+
  geom_point( data = subset(Hermosillo.DF , Fecha == max(Fecha)), aes(x=Fecha, y= Positividad.acum), fill="#215968", size=2 , shape=21, color="white", stroke=1) +
  geom_point( data = subset(Casossemana , Fecha == max(Fecha)), aes(x=Fecha, y= Positividad.semanal), fill="#4BACC6", size=2 , shape=21, color="white", stroke=1) +
  geom_dl( data = subset(Hermosillo.DF , Fecha == max(Fecha)), aes(x=Fecha, y= Positividad.acum, label = paste0("Positividad\nacumulada\n", Positividad.acum,"%", sep="")), color="#215968", 
           method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.6, fontfamily= "Lato Black")) +
  geom_dl( data = subset(Casossemana , Fecha == max(Fecha)), aes(x=Fecha, y= Positividad.semanal,  label = paste0("Positividad\núlt. 7 días\n", Positividad.semanal,"%\n", sep="")), color="#4BACC6", 
           method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.6, fontfamily= "Lato Black")) +
  scale_y_continuous(expand = c(0, 0), limits= c(0,100), breaks=seq(0,100,20)) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-10-15"), (max(as.Date(Hermosillo.DF$Fecha)) + 60)), 
               date_breaks = "1 month", labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                                                     paste(month(x, label = TRUE), "\n", year(x)),
                                                                     paste(month(x, label = TRUE)))) +
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_bw() +
  temaejes +
  labs(y = "%", 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo:</span><br><span style = 'color:#4BACC6';>Positividad de resultados válidos de pruebas</span>", 
       subtitle= Fechahoy, caption =fuentepruebas)
Positividad
ggsave("Gráficos semanales/Hermosillo/s12.png",Positividad, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)

# Municipales 

Casos <- read_csv("Bases/Casosdiarios_SSFED.csv", 
                  col_types = cols(CASOS = col_integer(), 
                                   CVEGEO = col_character(), Fecha = col_date(format = "%Y-%m-%d"), 
                                   MUNICIPIO = col_character(), NUEVOS = col_integer(), X1 = col_skip()), 
                  locale = locale(encoding = "ISO-8859-1"))
Decesos <- read_csv("Bases/Decesosdiarios_SSFED.csv", 
                    col_types = cols(DECESOS = col_integer(), 
                                     CVEGEO = col_character(), Fecha = col_date(format = "%Y-%m-%d"), 
                                     MUNICIPIO = col_character(), NUEVOS = col_integer(), X1 = col_skip()), 
                    locale = locale(encoding = "ISO-8859-1"))

HilloCasos <- Casos %>% group_by(MUNICIPIO) %>% 
  mutate(diasemana = weekdays(Fecha), casos.hmo = rollsum(NUEVOS, 7, align="right", fill = 0)) %>% rename(ACUMULADOS=CASOS) %>% 
  filter(diasemana==lundom) %>% filter(MUNICIPIO=="Hermosillo") %>% select(Fecha, casos.hmo) 


HilloDecesos <- Decesos %>% group_by(MUNICIPIO) %>% 
  mutate(diasemana = weekdays(Fecha), decesos.hmo = rollsum(NUEVOS, 7, align="right", fill = 0)) %>% rename(ACUMULADOS=DECESOS) %>% 
  filter(diasemana==lundom) %>% filter(MUNICIPIO=="Hermosillo") %>% select(Fecha, decesos.hmo)

Casossemana <- Casossemana %>% left_join(HilloCasos, by="Fecha") %>% left_join(HilloDecesos, by="Fecha") %>% mutate(casos.resto= Casos.semana-casos.hmo, decesos.resto=Decesos.semana-decesos.hmo)

Casosmuni <- ggplot(Casossemana) +
  geom_line(aes(x=Fecha, y= casos.resto), linetype = "solid", color = "#F79646", size=0.8, alpha=0.3) +
  geom_line(aes(x=Fecha, y= casos.hmo), linetype = "solid", color = "#01A2AC", size=0.8, alpha=0.3)+
  geom_point(aes(x=Fecha, y= casos.resto), size=0.8, color = "#F79646", alpha=0.6) +
  geom_point(aes(x=Fecha, y= casos.hmo), size=0.8, color = "#01A2AC", alpha=0.6)+
  geom_point( data = subset(Casossemana , Fecha == max(Fecha)), aes(x=Fecha, y= casos.resto), fill="#F79646", size=2 , shape=21, color="white", stroke=1) +
  geom_point( data = subset(Casossemana , Fecha == max(Fecha)), aes(x=Fecha, y= casos.hmo), fill="#01A2AC", size=2 , shape=21, color="white", stroke=1) +
  geom_dl( data = subset(Casossemana , Fecha == max(Fecha)), aes(x=Fecha, y= casos.resto,  label = paste0("Otros municipios\n", casos.resto,"\n", sep="")), color="#F79646", 
           method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.6, fontfamily= "Lato Black")) +
  geom_dl( data = subset(Casossemana , Fecha == max(Fecha)), aes(x=Fecha, y= casos.hmo, label = paste0("\nHermosillo\n", casos.hmo, sep="")), color="#01A2AC", 
           method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.6, fontfamily= "Lato Black")) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-10-15"),  (max(as.Date(Hermosillo.DF$Fecha)) + 82)), 
               date_breaks = "1 month", labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                                                     paste(month(x, label = TRUE), "\n", year(x)),
                                                                     paste(month(x, label = TRUE)))) +
  coord_cartesian(expand = TRUE, clip = 'off') +
  theme_bw() +
  temaejes +
  labs(y = "Casos confirmados", 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo:</span><br><span style = 'color:#01A2AC';>Casos confirmados semanalmente</span>", 
       subtitle= Fechasem, caption =fuente)
Casosmuni
ggsave("Gráficos semanales/Hermosillo/s06.png",Casosmuni, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


Casossem <- Casos %>% group_by(MUNICIPIO) %>% 
  mutate(diasemana = weekdays(Fecha), 'CASOS SEMANALES' = rollsum(NUEVOS, 7, align="right", fill = 0)) %>% rename(ACUMULADOS=CASOS) %>% 
  filter(diasemana==weekdays(max(as.Date(Fecha)))) %>% filter(Fecha>as.Date("2020-11-01"))

Casossem[Casossem<0] <- 0

Casossem <- Casossem %>% group_by(MUNICIPIO) %>% 
mutate(label=paste0(MUNICIPIO, "<br><span style = 'font-size:6pt; font-family:Lato'> ",prettyNum(`CASOS SEMANALES`[Fecha==max(Fecha)], big.mark=",", preserve.width="none"), " casos esta semana</span>"))

Casos_Semanales <- ggplot(subset(Casossem, MUNICIPIO %in% c("Hermosillo", "Cajeme", "Nogales", "San Luis Río Colorado", "Navojoa", "Guaymas", "Caborca", "Agua Prieta", "Huatabampo", "Puerto Peñasco", "Etchojoa", "Empalme", "Cananea","Magdalena", "Álamos", "Bácum"))) +
  geom_col(mapping = aes(x = Fecha, y = `CASOS SEMANALES`, fill = `CASOS SEMANALES`)) +
  scale_fill_gradient(low = "#58BCBC", high = "black") + 
  scale_x_date(expand=c(0,5),
               date_breaks = "3 month", labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                                                     paste(month(x, label = TRUE), "\n", year(x)),
                                                                     paste(month(x, label = TRUE)))) +
  coord_cartesian(expand = TRUE, clip = 'off') +
  facet_wrap(~ label, scales = "free_y") + 
  theme_bw() + temadiario  +
  theme(legend.text = element_text(family = "Lato", size = 7), legend.background = element_rect(fill="transparent"),
        axis.line=element_line(size=0.2),axis.ticks=element_line(size=0.2),
        legend.position = "none",  legend.justification="left", legend.margin=margin(t = 0, unit='cm'), axis.text = element_text(size=4.5),
        panel.grid.minor = element_blank(), strip.text = element_markdown(family = "Lato Black", size = 9, hjust=0),
        legend.key = element_rect(fill="transparent", color="transparent"), strip.background = element_blank()) +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo:</span><br><span style = 'color:#01A2AC';>Casos confirmados semanalmente</span>", 
       subtitle= paste0(Fechasem, " | 12 municipios más poblados del Estado."), caption =fuente)
Casos_Semanales

ggsave("Gráficos semanales/Hermosillo/s06des.png",Casos_Semanales, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)

Decesosmuni <- ggplot(Casossemana) +
  geom_line(aes(x=Fecha, y= decesos.resto), linetype = "solid", color = "#4BACC6", size=0.8, alpha=0.3) +
  geom_line(aes(x=Fecha, y= decesos.hmo), linetype = "solid", color = "#993366", size=0.8, alpha=0.3)+
  geom_point(aes(x=Fecha, y= decesos.resto), size=0.8, color = "#4BACC6", alpha=0.6) +
  geom_point(aes(x=Fecha, y= decesos.hmo), size=0.8, color = "#993366", alpha=0.6)+
  geom_point( data = subset(Casossemana , Fecha == max(Fecha)), aes(x=Fecha, y= decesos.resto), fill="#4BACC6", size=2 , shape=21, color="white", stroke=1) +
  geom_point( data = subset(Casossemana , Fecha == max(Fecha)), aes(x=Fecha, y= decesos.hmo), fill="#993366", size=2 , shape=21, color="white", stroke=1) +
  geom_dl( data = subset(Casossemana , Fecha == max(Fecha)), aes(x=Fecha, y= decesos.resto,  label = paste0("\nOtros municipios\n", decesos.resto,"\n\n",  sep="")), color="#4BACC6", 
           method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.6, fontfamily= "Lato Black")) +
  geom_dl( data = subset(Casossemana , Fecha == max(Fecha)), aes(x=Fecha, y= decesos.hmo, label = paste0("Hermosillo\n", decesos.hmo, sep="")), color="#993366", 
           method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.6, fontfamily= "Lato Black")) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-10-15"), (max(as.Date(Hermosillo.DF$Fecha)) + 80)),
               date_breaks = "1 month", labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                                                     paste(month(x, label = TRUE), "\n", year(x)),
                                                                     paste(month(x, label = TRUE)))) +
  coord_cartesian(expand = TRUE, clip = 'off') +
  theme_bw() +
  temaejes +
  labs(y = "Decesos confirmados", 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo:</span><br><span style = 'color:#993366';>Decesos confirmados semanalmente</span>", 
       subtitle= Fechasem, caption =fuente)
Decesosmuni
ggsave("Gráficos semanales/Hermosillo/s14.png",Decesosmuni, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


Casossemana <- Casossemana %>% mutate(POB=2944840) %>% mutate(mortalidad=round(Decesos.semana*1000000/POB,1), incidencia=round(Casos.semana*100000/POB,1), inciact=round(Activos*100000/POB,1))


Indicadores <- ggplot() +
  geom_hline(yintercept=100, linetype="dashed", color = "#CE3F41", alpha=0.5) +
  geom_text(aes(x = as.Date("2020-11-01"), y = (102),
                label = "Alta"), stat = "unique", family = "Lato Black",
            size = 2, color = "#CE3F41",  hjust=0, alpha=0.8) +
  geom_hline(yintercept=50, linetype="dashed", color = "#FFA17B", alpha=0.5) +
  geom_text(aes(x = as.Date("2020-11-01"), y = (52),
                label = "Substancial"), stat = "unique", family = "Lato Black",
            size = 2, color = "#FFA17B",  hjust=0, alpha=0.8) +
  geom_hline(yintercept=10, linetype="dashed", color = "#FECF7D", alpha=0.5) +
  geom_text(aes(x = as.Date("2020-11-01"), y = (12),
                label = "Moderada"), stat = "unique", family = "Lato Black",
            size = 2, color = "#FECF7D",  hjust=0, alpha=0.8) +
  geom_line(data= Casossemana, aes(x=Fecha, y= mortalidad), color= "#993366", linetype= "solid", size=1, alpha=0.8)+
  geom_line(data= Casossemana, aes(x=Fecha, y= incidencia), color= "#01A2AC", linetype= "solid", size=1, alpha=0.8)+
  geom_point( data = subset(Casossemana , Fecha == max(Fecha)), aes(x=Fecha, y= mortalidad), fill="#993366", size=2 , shape=21, color="white", stroke=1) +
  geom_point( data = subset(Casossemana , Fecha == max(Fecha)), aes(x=Fecha, y= incidencia), fill="#01A2AC", size=2 , shape=21, color="white", stroke=1) +
  geom_dl( data = subset(Casossemana, Fecha == max(Fecha)), aes(x=Fecha, y= mortalidad, label = paste0(mortalidad, " decesos\npor millón de habs.", sep="")), color="#993366", 
           method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.8, fontfamily= "Lato Black")) +
  geom_dl( data = subset(Casossemana , Fecha == max(Fecha)), aes(x=Fecha, y= incidencia,  label = paste0(incidencia," casos\npor 100 mil habs.",  sep="")), color="#01A2AC", 
           method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.8, fontfamily= "Lato Black")) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,max(Casossemana$incidencia)+20)) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-10-15"), (max(as.Date(Casossemana$Fecha)) + 100)), 
               date_breaks = "1 month", labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                                                     paste(month(x, label = TRUE), "\n", year(x)),
                                                                     paste(month(x, label = TRUE)))) +
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_bw() +
  temaejes + theme(axis.title.y = element_markdown(family = "Lato", size =6)) +
  labs(y = "<span style = 'color:#01A2AC';>Incidencia (casos por 100 mil de habs.)</span><br><span style = 'color:#993366';>Mortalidad (decesos por millón de habs.)</span>", 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo:</span><br><span style = 'color:#01A2AC';>Incidencia</span> y <span style = 'color:#993366';>mortalidad</span> por semana", 
       subtitle= Fechahoy, caption =fuente)
Indicadores
ggsave("Gráficos semanales/Hermosillo/s18.png",Indicadores, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


IndicadoresAct <- ggplot() +
  geom_line(data= Casossemana, aes(x=Fecha, y= inciact), color= "#3B9494", linetype= "solid", size=1, alpha=0.8)+
  geom_point( data = subset(Casossemana , Fecha == max(Fecha)), aes(x=Fecha, y= inciact), fill="#3B9494", size=2 , shape=21, color="white", stroke=1) +
  geom_dl( data = subset(Casossemana , Fecha == max(Fecha)), aes(x=Fecha, y= inciact,  label = paste0(inciact," casos activos\npor 100 mil habs.",  sep="")), color="#3B9494", 
           method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.8, fontfamily= "Lato Black")) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,max(Casossemana$inciact+20))) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-10-15"), (max(as.Date(Casossemana$Fecha)) + 100)), 
               date_breaks = "1 month", labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                                                     paste(month(x, label = TRUE), "\n", year(x)),
                                                                     paste(month(x, label = TRUE)))) +
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_bw() +
  temaejes + theme(axis.title.y = element_markdown(family = "Lato", size =6)) +
  labs(y = "<span style = 'color:#01A2AC';>Incidencia (casos por 100 mil de habs.)</span>", 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo:</span><br><span style = 'color:#3B9494';>Incidencia de casos activos al cierre de semana</span>",
       subtitle= Fechahoy, caption =fuente)
IndicadoresAct
ggsave("Gráficos semanales/Hermosillo/s30.png",IndicadoresAct, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


Casos2021 <- Casossemana %>% filter(Fecha>as.Date("2020-10-14"))
Recuperados <- ggplot() +
  geom_line(data= Casos2021, aes(x=Fecha, y= Casos.semana), color= "#01A2AC", linetype= "solid", size=1, alpha=0.8)+
  geom_line(data= Casos2021, aes(x=Fecha, y= Recuperados.semana), color= "#2D6669", linetype= "solid", size=1, alpha=0.8)+
  geom_point( data = subset(Casos2021 , Fecha == max(Fecha)), aes(x=Fecha, y= Casos.semana), fill="#01A2AC", size=2 , shape=21, color="white", stroke=1) +
  geom_point( data = subset(Casos2021 , Fecha == max(Fecha)), aes(x=Fecha, y= Recuperados.semana), fill="#2D6669", size=2 , shape=21, color="white", stroke=1) +
   geom_dl( data = subset(Casossemana, Fecha == max(Fecha)), aes(x=Fecha, y= Recuperados.semana, label = paste0( "\n",Recuperados.semana, "\nrecuperados", sep="")), color="#2D6669", 
            method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.8, fontfamily= "Lato Black")) +
  geom_dl( data = subset(Casossemana , Fecha == max(Fecha)), aes(x=Fecha, y= Casos.semana,  label = paste0(Casos.semana,"\nconfirmados","\n", sep="")), color="#01A2AC", 
            method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.8, fontfamily= "Lato Black")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,max(Casossemana$Casos.semana)+100)) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-10-15"), (max(as.Date(Casos2021$Fecha)+60))), 
               date_breaks = "1 month", labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                                                     paste(month(x, label = TRUE), "\n", year(x)),
                                                                     paste(month(x, label = TRUE)))) +
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_bw() +
  temaejes + theme(axis.title.y = element_markdown(family = "Lato", size =6)) +
  labs(y = NULL, 
       x = NULL,legend= NULL, 
       title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo:</span><br>Casos <span style = 'color:#2D6669';>recuperados</span> y <span style = 'color:#01A2AC';>confirmados</span> por semana", 
       subtitle= Fechasem, caption =fuente)
Recuperados
ggsave("Gráficos semanales/Hermosillo/s22.png",Recuperados, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


write.csv(Casossemana, "ResultadoCSV/Semana.csv")

# Casos ISSSTESON
casosisst <- Casossemana %>% filter(Fecha >= as.Date("2020-05-28"))
ISSSTESON <- ggplot(casosisst) +
  geom_col(aes(x=Fecha, y= ISSSTESON, fill= ISSSTESON),size=0.15) +
  scale_fill_gradient2(low = "#FCD5B5", mid= "#F79646", high = "#984807", midpoint = 170) +
  geom_text( aes(x=Fecha, y= ISSSTESON, label= ISSSTESON), family="Lato Black", size= 3, color="white", angle=90, hjust = 1.1) +
  scale_x_date(expand=c(0,5), breaks = casosisst$Fecha, date_labels = "%d/%m") +
  # scale_x_date(expand=c(0,5), date_breaks = "1 month", date_labels = "%B") +
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_minimal() +
  temasinejes +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo:</span><br><span style = 'color:#F79646';>Casos atendidos en ISSSTESON semanalmente</span>", 
       subtitle= Fechasem, caption =fuente)
ISSSTESON

ggsave("Gráficos semanales/Hermosillo/ISSSTESON.png",ISSSTESON, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


c14 <- ggplot(Casossemana) +
  geom_col(aes(x=Fecha, y= C_0_14, fill= C_0_14), color= "#005156", size=0.15) +
  scale_fill_gradient2(low = "#DEF2F2", mid= "#01A2AC", high = "#005156", midpoint = 100) +
  geom_text( aes(x=Fecha, y= C_0_14, label= C_0_14), family="Lato Black", size= 3, color="white", angle=90, hjust = 1.1) +
  scale_x_date(expand=c(0,5), breaks = Casossemana$Fecha, date_labels = "%d/%m") +
  # scale_x_date(expand=c(0,5), date_breaks = "1 month", date_labels = "%B") +
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_minimal() +
  temasinejes +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo:</span><br><span style = 'color:#01A2AC';>Casos de 0 a 14 años de edad por semana</span>", 
       subtitle= Fechasem, caption =fuente)
c14

ggsave("Gráficos semanales/Hermosillo/c0_14.png",c14, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)

Acumedad <- Hermosilloacum %>% select(C_0_14, C_15_29,C_30_59,C_60_mas) %>% 
  rename ("0 a 14 años"=C_0_14, "15 a 29 años"=C_15_29,"30 a 59 años"=C_30_59,"60 o más años"=C_60_mas) %>% 
  gather(key= "grupoedad", value= "ACUMULADOS", ends_with("años"))
Casosedad <- Casossemana %>% select(Fecha, C_0_14, C_15_29,C_30_59,C_60_mas) %>% 
  select(Fecha, C_0_14, C_15_29,C_30_59,C_60_mas) %>% 
  rename ("0 a 14 años"=C_0_14, "15 a 29 años"=C_15_29,"30 a 59 años"=C_30_59,"60 o más años"=C_60_mas) %>% 
  gather(key= "grupoedad", value= "casos", ends_with("años")) %>% 
  left_join(Acumedad, by="grupoedad") %>% 
  mutate(label=paste0(grupoedad, ":"," ", prettyNum(ACUMULADOS, big.mark=",", preserve.width="none"), " acumulados")) %>% 
  rename(corte=Fecha)

CasosEdadG <- ggplot(Casosedad) +
  geom_area(aes(x= corte, y= casos), fill= "#58BCBC", alpha=0.3)+
  geom_segment(aes(x= corte, y= casos, xend = corte, yend = 0), size = 0.3, lineend = "butt", color="white", alpha=0.5) +
  geom_line(aes(x= corte, y= casos, color= "Tendencia promedio móvil 7 días"), linetype= "solid", size=.4)+
  
  scale_color_manual(name="", values= c("Tendencia promedio móvil 7 días" = "#01787E", "Casos diarios" = "white")) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_date(expand=c(0,5),
               date_breaks = "3 month", labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                                                     paste(month(x, label = TRUE), "\n", year(x)),
                                                                     paste(month(x, label = TRUE)))) +
  facet_wrap(~label, scales = "free")+
  theme_bw() + temadiario  +
  theme(legend.text = element_text(family = "Lato", size = 7), legend.background = element_rect(fill="transparent"),
        legend.position = "none",  legend.justification="left", legend.margin=margin(t = 0, unit='cm'), axis.text = element_text(size=6), strip.text = element_text(size=9),
        panel.grid.minor = element_blank(),
        legend.key = element_rect(fill="transparent", color="transparent"), strip.background = element_blank()) +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo:</span><br><span style = 'color:#01A2AC';>Casos confirmados por semana</span>", 
       subtitle= Fechasem, caption =fuente)

CasosEdadG

ggsave("Gráficos semanales/Hermosillo/s07.png",CasosEdadG, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)

AcumedadD <- Hermosilloacum %>% select(D_0_14, D_15_29,D_30_59,D_60_mas) %>% 
  rename ("0 a 14 años"=D_0_14, "15 a 29 años"=D_15_29,"30 a 59 años"=D_30_59,"60 o más años"=D_60_mas) %>% 
  gather(key= "grupoedad", value= "ACUMULADOS", ends_with("años"))
Decesosedad <- Casossemana %>% select(Fecha, D_0_14, D_15_29,D_30_59,D_60_mas) %>% 
  select(Fecha, D_0_14, D_15_29,D_30_59,D_60_mas) %>% 
  rename ("0 a 14 años"=D_0_14, "15 a 29 años"=D_15_29,"30 a 59 años"=D_30_59,"60 o más años"=D_60_mas) %>% 
  gather(key= "grupoedad", value= "decesos", ends_with("años")) %>% 
  left_join(AcumedadD, by="grupoedad") %>% 
  mutate(label=paste0(grupoedad, ":"," ", prettyNum(ACUMULADOS, big.mark=",", preserve.width="none"), " acumulados")) %>% 
  rename(corte=Fecha)


int_breaks_rounded <- function(x, n = 5)  pretty(x, n)[round(pretty(x, n),1) %% 1 == 0]
DecesosEdadG <- ggplot(Decesosedad) +
  geom_area(aes(x= corte, y= decesos), fill= "#D075A3", alpha=0.3)+
  geom_segment(aes(x= corte, y= decesos, xend = corte, yend = 0), size = 0.3, lineend = "butt", color="white", alpha=0.5) +
  geom_line(aes(x= corte, y= decesos, color= "Tendencia promedio móvil 7 días"), linetype= "solid", size=.4)+
  scale_color_manual(name="", values= c("Tendencia promedio móvil 7 días" = "#73264D")) +
  scale_y_continuous(expand = c(0,0), breaks = int_breaks_rounded) +
  scale_x_date(expand=c(0,5), 
               date_breaks = "3 month", labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                                                     paste(month(x, label = TRUE), "\n", year(x)),
                                                                     paste(month(x, label = TRUE)))) +
  facet_wrap(~label, scales = "free")+
  theme_bw() + temadiario  +
  theme(legend.text = element_text(family = "Lato", size = 7), legend.background = element_rect(fill="transparent"),
        legend.position = "none",  legend.justification="left", legend.margin=margin(t = 0, unit='cm'), axis.text = element_text(size=6), strip.text = element_text(size=9),
        panel.grid.minor = element_blank(),
        legend.key = element_rect(fill="transparent", color="transparent"), strip.background = element_blank()) +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo:</span><br><span style = 'color:#73264D';>Decesos confirmados por semana</span>", 
       subtitle= Fechasem, caption =fuenteedad)

DecesosEdadG

ggsave("Gráficos semanales/Hermosillo/s10.png",DecesosEdadG, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


SINTOMAS <- read_csv("Bases/COVIDSONORA_SINTOMAS.csv", 
                     col_types = cols(X1 = col_skip(), fecha_sintomas = col_date(format = "%Y-%m-%d")), 
                     locale = locale(encoding = "ISO-8859-1")) %>% filter(NOM_MUN=="Hermosillo")
SINTOMAS <- SINTOMAS %>% 
  select(fecha_sintomas, confirmados, decesos, hospitalizados) %>% group_by(fecha_sintomas) %>% summarise(confirmados=sum(confirmados), decesos=sum(decesos), hospitalizados=sum(hospitalizados)) %>% 
  mutate(confirmados.acum=cumsum(confirmados), decesos.acum=cumsum(decesos), hospitalizados.acum=cumsum(hospitalizados)) %>% 
  mutate(diasemana = weekdays(fecha_sintomas)) %>% 
  filter(diasemana==lundom)

SINTOMAS <- mutate(SINTOMAS , casos.semana= confirmados.acum - lag( confirmados.acum, default =  confirmados.acum[1], order_by=fecha_sintomas))
SINTOMAS <- mutate(SINTOMAS , decesos.semana= decesos.acum - lag( decesos.acum, default =  decesos.acum[1], order_by=fecha_sintomas))
SINTOMAS <- mutate(SINTOMAS , hospitalizados.semana= hospitalizados.acum - lag( hospitalizados.acum, default =  hospitalizados.acum[1], order_by=fecha_sintomas))
sintomassemana <- SINTOMAS %>% mutate(letalidad=round(decesos.semana*100/casos.semana,1), hospitalización=round(hospitalizados.semana*100/casos.semana,1))

temadiario <- theme(plot.margin = margin(10, 25, 10, 25),
                    plot.title = element_markdown(family = "Lato Black", size = 25),
                    panel.grid=element_blank(), panel.border=element_blank(), axis.line= element_line(color = "black", size = 0.3),
                    plot.subtitle = element_text(family = "Lato Light", size = 10, color = "black"), legend.title = element_blank(),
                    strip.text = element_text(family = "Lato Black", size = 8, hjust=0),
                    axis.text = element_text(family = "Lato", size =6),
                    plot.background = element_rect(fill = "white", color = "white", size = 3),
                    axis.title.x = element_text(family = "Lato Light", size = 6, hjust=1),
                    axis.title.y = element_text(family = "Lato Light", size = 6, hjust=1), 
                    plot.caption = element_text(family = "Lato", size = 6),
                    legend.text = element_blank(), legend.background =  element_rect(fill="transparent", color="transparent"),
                    legend.box.background=  element_rect(fill="transparent", color="transparent" ), 
                    legend.key = element_rect(fill="transparent", color="transparent"),
                    legend.position = "none", plot.title.position = 'plot', plot.caption.position = 'plot')

Hospitalizados <- sintomassemana  %>%
  filter(fecha_sintomas<=dia-14) %>%  ggplot(aes(x= fecha_sintomas, y= hospitalización)) +
  geom_area(color= "transparent", fill= "#F79646",alpha=0.1)+
  geom_segment(aes(xend = fecha_sintomas, yend = 0), size = 0.6, lineend = "butt", color="white", alpha=0.7) +
  geom_line(color= "#F79646", linetype= "solid", size=1, alpha=0.4 )+
  geom_point( data = subset(sintomassemana , fecha_sintomas == max(fecha_sintomas)-7), fill="#F79646", size=1 , shape=21, color="#F79646", stroke=1) +
  geom_point( data = subset(sintomassemana , fecha_sintomas < max(fecha_sintomas)-7), fill="white", size=1 , shape=21, color="#F79646", stroke=0.8) +
  geom_dl( data = subset(sintomassemana, fecha_sintomas == max(fecha_sintomas)-7), aes(label = paste0(hospitalización,"%")), color="#F79646", method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 1.4, fontfamily= "Lato Black")) +
  scale_y_continuous(expand = c(0, 0), limits= c(0,35), breaks=seq(0,35,5)) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-05-01"), max(as.Date(sintomassemana$fecha_sintomas)+30)),
               date_breaks = "1 month", labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                                                     paste(month(x, label = TRUE), "\n", year(x)),
                                                                     paste(month(x, label = TRUE)))) +
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_bw() +
  temadiario + theme(axis.text.y= element_text(size=8)) +
  labs(y = "Hospitalizados por cada 100 casos confirmados", 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo:</span><br><span style = 'color:#F79646';>Hospitalización<br>por semana de inicio de síntomas</span>", 
       subtitle= paste0("Casos confirmados que requirieron hospitalización respecto al total por semana de inicio de síntomas \n", Fechahoy, " con fecha de inicio de síntomas hasta el ",day(dia-14), "/", month(dia-14),"/", year(dia-14)), caption =fuentedes)  

Hospitalizados
ggsave("Gráficos semanales/Hermosillo/s20.png",Hospitalizados , width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


Letalidad <- sintomassemana %>% filter(fecha_sintomas<=dia-21) %>%
ggplot(aes(x= fecha_sintomas, y= letalidad)) +
  geom_area(color= "transparent", fill= "#D075A3",alpha=0.1)+
  geom_segment(aes(xend = fecha_sintomas, yend = 0), size = 0.6, lineend = "butt", color="white", alpha=0.7) +
  geom_line(color= "#73264D", linetype= "solid", size=1, alpha=0.4)+
  geom_point( data = subset(sintomassemana , fecha_sintomas == max(fecha_sintomas)-14), fill="#73264D", size=1 , shape=21, color="#73264D", stroke=1) +
  geom_point( data = subset(sintomassemana , fecha_sintomas < max(fecha_sintomas)-14), fill="white", size=1 , shape=21, color="#73264D", stroke=0.8) +
  geom_dl( data = subset(sintomassemana, fecha_sintomas == max(fecha_sintomas)-14), aes(label = paste0(letalidad,"%")), color="#73264D", method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 1.4, fontfamily= "Lato Black")) +
  scale_y_continuous(expand = c(0, 0), limits= c(0,20), breaks=seq(0,20,2)) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-05-01"), max(as.Date(sintomassemana$fecha_sintomas)+30)), 
               date_breaks = "1 month", labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                                                     paste(month(x, label = TRUE), "\n", year(x)),
                                                                     paste(month(x, label = TRUE)))) +
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_bw() +
  temadiario + theme(axis.text.y= element_text(size=8)) +
  labs(y = "Decesos por cada 100 casos confirmados", 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo:</span><br><span style = 'color:#73264D';>Letalidad<br>por semana de inicio de síntomas</span>", 
       subtitle= paste0("Casos confirmados fallecidos respecto al total por semana de inicio de síntomas \n", Fechahoy, " con fecha de inicio de síntomas hasta el ",day(dia-21), "/", month(dia-21),"/", year(dia-21)), caption =fuentedes)  

Letalidad
ggsave("Gráficos semanales/Hermosillo/s24.png",Letalidad , width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


Menores <- Casossemana %>% ggplot(aes(x= Fecha, y= pctj_menores)) +
  geom_area(color= "transparent", fill= "#01A2AC",alpha=0.1)+
  geom_segment(aes(xend = Fecha, yend = 0), size = 0.6, lineend = "butt", color="white", alpha=0.7) +
  geom_line(color= "#01A2AC", linetype= "solid", size=1, alpha=0.8)+
  geom_point( data = subset(Casossemana , Fecha == max(Fecha)), fill="white", size=1 , shape=21, color="#01A2AC", stroke=1) +
  geom_dl( data = subset(Casossemana, Fecha == max(Fecha)), aes(label = paste0(pctj_menores,"%")), color="#01A2AC", method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 1.5, fontfamily= "Lato Black")) +
  scale_y_continuous(expand = c(0, 0), limits= c(0,12), breaks=seq(0,12,2)) +
  scale_x_date(expand=c(0,0), limits = c(min(as.Date(Casossemana$Fecha)), max(as.Date(Casossemana$Fecha)+60)), 
               date_breaks = "1 month", labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                                                     paste(month(x, label = TRUE), "\n", year(x)),
                                                                     paste(month(x, label = TRUE)))) +
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_bw() +
  temadiario + theme(axis.text.y= element_text(size=6)) +
  labs(y = "Casos en menores de 15 años por cada 100 casos confirmados", 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo:</span><br><span style = 'color:#01A2AC';>Casos pediátricos confirmados<br>respecto al total reportado</span>", 
       subtitle= paste0("Casos confirmados en menores de 15 años respecto a los confirmados totales según semana de reporte\n", Fechahoy), caption =fuentedes)  

Menores
ggsave("Gráficos semanales/Hermosillo/s25.png",Menores , width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


INGRESO <- read_csv("Bases/ST_HermosilloIngreso_SSFED.csv", 
                     col_types = cols(X1 = col_skip(), fecha_ingreso = col_date(format = "%Y-%m-%d")), 
                     locale = locale(encoding = "ISO-8859-1")) %>% rename(Fecha=fecha_ingreso)

Fechas <- data.frame(Fecha=as.Date(Hermosillo.DF$Fecha)) 

Ingresosemanas <- INGRESO %>% 
  full_join(Fechas, by="Fecha") %>%
  mutate_if(is.numeric,coalesce,0) %>% mutate(Analizados.semana=rollsum(Analizados, 7, align="right", fill = 0),
                                              Casos.semana=rollsum(Casos, 7, align="right", fill = 0),
                                              Decesos.semana=rollsum(Decesos, 7, align="right", fill = 0),
                                              C_0_14.semana=rollsum(C_0_14, 7, align="right", fill = 0),
                                              C_15_29.semana=rollsum(C_15_29, 7, align="right", fill = 0),
                                              C_30_59.semana=rollsum(C_30_59, 7, align="right", fill = 0),
                                              C_60_mas.semana=rollsum(C_60_mas, 7, align="right", fill = 0),
                                              D_0_14.semana=rollsum(D_0_14, 7, align="right", fill = 0),
                                              D_15_29.semana=rollsum(D_15_29, 7, align="right", fill = 0),
                                              D_30_59.semana=rollsum(D_30_59, 7, align="right", fill = 0),
                                              D_60_mas.semana=rollsum(D_60_mas, 7, align="right", fill = 0)) %>% 
  select(Fecha, Analizados.semana,Casos.semana, Decesos.semana, C_0_14.semana, C_15_29.semana, C_30_59.semana, 
         C_60_mas.semana, D_0_14.semana, D_15_29.semana, D_30_59.semana, D_60_mas.semana) %>% 
  mutate(diasemana = weekdays(Fecha)) %>% 
  filter(diasemana==lundom) %>% mutate(pctj_menores=round(C_0_14.semana*100/Casos.semana, 2))


Sonora.Defuncion <- read_csv("Bases/ST_HermosilloDefuncion_SSFED.csv", 
                             col_types = cols(fecha_def = col_date(format = "%Y-%m-%d"))) %>% rename(Fecha=fecha_def) %>% filter(Fecha>as.Date("2020-03-03"))
Sonora.Defuncion <- Fechas %>% left_join(Sonora.Defuncion, by="Fecha")
Sonora.Sintomas <- read_csv("Bases/ST_HermosilloSintomas_SSFED.csv", 
                            col_types = cols(fecha_sintomas = col_date(format = "%Y-%m-%d"))) %>% rename(Fecha=fecha_sintomas) %>% filter(Fecha>as.Date("2020-03-03"))
Sonora.Sintomas <- Fechas %>% left_join(Sonora.Sintomas, by="Fecha")
Sonora.Ingreso <- read_csv("Bases/ST_HermosilloIngreso_SSFED.csv", 
                           col_types = cols(fecha_ingreso = col_date(format = "%Y-%m-%d"))) %>% rename(Fecha=fecha_ingreso) %>% filter(Fecha>as.Date("2020-03-03"))
Sonora.Ingreso <- Fechas %>% left_join(Sonora.Ingreso, by="Fecha")
Sonora.DF <- read_csv("Bases/ST_HermosilloReporte_SSFED.csv", 
                      col_types = cols(fecha_reporte = col_date(format = "%Y-%m-%d"))) %>% 
  rename(Fecha=fecha_reporte, Pruebas=Resultados_lab, Confirmados=Casos) 
Sonora.DF <- mutate(Sonora.DF, Positividad.acum= round((Confirmados_lab / Pruebas)*100,1))

dia.ev <- dia-14

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
Casossemana <- Casossemana %>% filter(Fecha>=as.Date("2020-10-15")) %>% mutate(pctj_menores=round(C_0_14*100/Casos.semana, 1)) 

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
  filter(diasemana==lundom) 
Casossemanas <- Sintomassemanas %>% select(Fecha, Casos.semana)

Decesossemanas <- Sonora.Defuncion %>%
  mutate_if(is.numeric,coalesce,0) %>% mutate(Decesos.semana=rollsum(Decesos, 7, align="right", fill = 0),
  ) %>% 
  select(Fecha, Decesos.semana) %>% 
  mutate(diasemana = weekdays(Fecha)) %>% 
  filter(diasemana==lundom)

Hospitalizadossemanas <- Sonora.Ingreso %>%  
  mutate_if(is.numeric,coalesce,0) %>% mutate(Hospitalizados.semana=rollsum(Hospitalizados, 7, align="right", fill = 0)) %>% 
  select(Fecha, Hospitalizados.semana) %>%
  mutate(diasemana = weekdays(Fecha)) %>% 
  filter(diasemana==lundom)




Evolucion <- Casossemanas %>% left_join(Decesossemanas, by="Fecha") %>% left_join(Hospitalizadossemanas, by="Fecha")
Evolucion <- Evolucion %>% mutate( Casos=round(Casos.semana/1636,3), Decesos=round(Decesos.semana/160,3), Hospitalizados=round(Hospitalizados.semana/327,3))
write.csv(Evolucion,"ResultadoCSV/evolucion.csv")

CasosSon <- ggplot() +
  geom_vline(xintercept=as.Date("2021-02-15"), linetype="dashed", color = "gray45") +
  # geom_hline(yintercept=estata.hoy$Casos.diarios, linetype="dashed", color = "red") +  geom_line(aes(data=Sintomassemanas, x= Fecha, y= Casos.semana),color="#01A2AC", lineend="round", linejoin="round",linetype= "solid", size=.7) +
  geom_line(data=subset(Decesossemanas, Fecha <=dia.act),aes( x= Fecha, y= Decesos.semana*4),color="#73264D", lineend="round", linejoin="round",linetype= "solid", size=.7) +
  geom_line(data=subset(Hospitalizadossemanas, Fecha <=dia.act),aes( x= Fecha, y= Hospitalizados.semana*4),color="#F79646", lineend="round", linejoin="round",linetype= "solid", size=.7) +
  geom_line(data=subset(Casossemanas, Fecha <=dia.act),aes( x= Fecha, y= Casos.semana),color="#01A2AC", lineend="round", linejoin="round",linetype= "solid", size=.7) +
  #geom_point(data=subset(Hermosillo.Sintomas, Fecha <=dia.act), aes(x= Fecha, y= Casos,fill="Casos que iniciaron síntomas el día correspondiente"), color = "white", size = 0.7, stroke=0.2,shape = 21) +
  #geom_point(data=subset(Hermosillo.Sintomas, Fecha >dia.act), aes(x= Fecha, y= Casos), fill= alpha("#01787E", 0.45), color = "white", size = 0.7, stroke=0.2, alpha=0.45, shape = 21) +
  # geom_text(aes(x = as.Date("2020-03-08"), y = (estata.semana$Casos.diarios + 30),
  #               label = paste0("+", estata.semana$Casos.diarios, " casos el mismo día semana anterior")), stat = "unique", family = "Lato Black",
  #           size = 2, color = "gray45", hjust=0)+
  geom_text(aes(x = as.Date("2021-02-10"), y = 1400,
                label = "Inicia campaña de vacunación en Sonora"), stat = "unique", family = "Lato Light",
            size = 2, color = "gray45",  hjust=0, angle=90)+
  scale_y_continuous(expand = c(0, 0), limits=c(0,max(Sintomassemanas$Casos.semana)), name= "<span style = 'color:#01A2AC';>Casos confirmados por semana de inicio de síntomas</span>", sec.axis = sec_axis( trans=~./4, name="<span style = 'color:#73264D';>Decesos por semana de ocurridos</span><br><span style = 'color:#F79646';>Hospitalizados por semana de ingreso</span>")) +
  scale_x_date(expand=c(0,0),limits= c(min(Sonora.Sintomas$Fecha)-5,max(Sonora.Sintomas$Fecha)+10), date_breaks = "1 month", 
               labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                            paste(month(x, label = TRUE), "\n", year(x)), 
                                            paste(month(x, label = TRUE)))) +
  guides(fill = guide_legend(order = 1), 
         color = guide_legend(order = 2)) +
  theme_linedraw()+ temaejes +
  theme(legend.text = element_text(family = "Lato", size = 8), legend.background = element_rect(fill="transparent"), legend.box = "horizontal",
        axis.title.y = element_markdown(family = "Lato Light", size = 8, hjust=1),
        axis.title.y.right = element_markdown(family = "Lato Light", size = 8, hjust=0), 
        panel.grid.major = element_line(linetype = "dashed", color="gray90", size=.2), panel.background = element_rect(color="black"),
        legend.position = c(0.02,0.95),  legend.justification="left", legend.margin=margin(t = 0, unit='cm'),
        legend.key = element_rect(fill="transparent", color="transparent")) +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo</span><br><span style = 'color:#01A2AC';>Casos, </span><span style = 'color:#F79646';>Hospitalizados</span> y </span><span style = 'color:#73264D';>Decesos</span>", 
       subtitle= paste0(Fechahoy, " con corte al 30 de enero de 2022"), caption =fuente)
CasosSon 

ggsave("Gráficos semanales/evolucion.png",CasosSon, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)



CasosSonrelativa <- ggplot() +
  geom_hline(yintercept=1, linetype="dashed", color = "black", size=0.3) +
  geom_vline(xintercept=as.Date("2021-02-15"), linetype="solid", color = "#4BACC6", size=0.5, ) +
  annotate("rect", xmin = as.Date("2021-02-15"), xmax = max(Sonora.Sintomas$Fecha)+90, ymin = 0, ymax = 2,
           alpha = .07,fill = "#4BACC6") +
  # geom_hline(yintercept=estata.hoy$Casos.diarios, linetype="dashed", color = "red") +  geom_line(aes(data=Sintomassemanas, x= Fecha, y= Casos.semana),color="#01A2AC", lineend="round", linejoin="round",linetype= "solid", size=.7) +
  geom_line(data=subset(Evolucion, Fecha <=dia.ev),aes( x= Fecha, y= Decesos),color="#73264D", lineend="round", linejoin="round",linetype= "solid", size=.7, alpha=0.7) +
  geom_line(data=subset(Evolucion, Fecha <=dia.ev),aes( x= Fecha, y= Hospitalizados),color="#F79646", lineend="round", linejoin="round",linetype= "solid", size=.7, alpha=0.7) +
  geom_line(data=subset(Evolucion, Fecha <=dia.ev),aes( x= Fecha, y= Casos),color="#01A2AC", lineend="round", linejoin="round",linetype= "solid", size=.7) +
  geom_point(data=subset(Evolucion, Fecha ==dia.ev),aes( x= Fecha, y= Decesos),color="#73264D",  size=1) +
  geom_point(data=subset(Evolucion, Fecha ==dia.ev),aes( x= Fecha, y= Hospitalizados),color="#F79646", size=1) +
  geom_point(data=subset(Evolucion, Fecha ==dia.ev),aes( x= Fecha, y= Casos),color="#01A2AC", size=1) +
  geom_dl( data=subset(Evolucion, Fecha ==dia.ev), aes(x= Fecha, y= Decesos), label = "\nDecesos\npor ocurrencia", color="#993366", 
           method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.6, fontfamily= "Lato Black")) +
  geom_dl( data=subset(Evolucion, Fecha ==dia.ev), aes(x= Fecha, y= Hospitalizados),  label ="Hospitalizaciones\npor ingreso\n", color="#F79646", 
           method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.6, fontfamily= "Lato Black")) +
  geom_dl( data=subset(Evolucion, Fecha ==dia.ev), aes(x= Fecha, y= Casos),  label = "Casos\npor inicio de\nsíntomas", color="#01A2AC", 
           method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.6, fontfamily= "Lato Black")) +
  #geom_point(data=subset(Hermosillo.Sintomas, Fecha <=dia.act), aes(x= Fecha, y= Casos,fill="Casos que iniciaron síntomas el día correspondiente"), color = "white", size = 0.7, stroke=0.2,shape = 21) +
  #geom_point(data=subset(Hermosillo.Sintomas, Fecha >dia.act), aes(x= Fecha, y= Casos), fill= alpha("#01787E", 0.45), color = "white", size = 0.7, stroke=0.2, alpha=0.45, shape = 21) +
  # geom_text(aes(x = as.Date("2020-03-08"), y = (estata.semana$Casos.diarios + 30),
  #               label = paste0("+", estata.semana$Casos.diarios, " casos el mismo día semana anterior")), stat = "unique", family = "Lato Black",
  #           size = 2, color = "gray45", hjust=0)+
  geom_text(aes(x = as.Date("2021-02-19"), y = 1.9,
                label = "Campaña de vacunación en Sonora"), stat = "unique", family = "Lato",
            size = 3, color = "#4BACC6",  hjust=0)+
  scale_y_continuous(expand = c(0, 0), limits= c(0,2), labels = scales::percent_format(scale = 100), position = "right") +
  scale_x_date(expand=c(0,0),limits= c(min(Sonora.Sintomas$Fecha)-5,max(Sonora.Sintomas$Fecha)+90), date_breaks = "1 month", 
               labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                            paste(month(x, label = TRUE), "\n", year(x)), 
                                            paste(month(x, label = TRUE)))) +  theme_linedraw()+ temaejes +
  theme(legend.text = element_text(family = "Lato", size = 8), legend.background = element_rect(fill="transparent"), legend.box = "horizontal",
        axis.title.y = element_markdown(family = "Lato Light", size = 8, hjust=1),plot.subtitle = element_markdown(family = "Lato Light", size = 10, color = "black"),
        panel.grid.major = element_line(linetype = "dashed", color="gray90", size=.2), panel.background = element_rect(color="transparent"),
        axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        legend.position = c(0.02,0.95),  legend.justification="left", legend.margin=margin(t = 0, unit='cm'),
        legend.key = element_rect(fill="transparent", color="transparent")) +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo</span><br>Evolución semanal de los confirmados", 
       subtitle= paste0("Respecto al máximo semanal previo a 2022.<br>", Fechahoy, " con corte al ", day(dia.ev)," de ",months.Date(dia.ev)," de ",year(dia.ev),"*"),
       caption =fuentefech)

CasosSonrelativa 

ggsave("Gráficos semanales/Hermosillo/evolucionrelativa.png",CasosSonrelativa, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)







