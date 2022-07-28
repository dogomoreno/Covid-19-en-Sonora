rm(list=ls())

# Paquetes

library(tidyverse)
library(extrafont)
library(scales)
library(plotly)
library(htmlwidgets)
library(showtext)
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

SLRC.DF <- read_csv("Bases/ST_SLRCReporte_SSFED.csv", 
                      col_types = cols(X1 = col_skip(), 
                                       fecha_reporte = col_date(format = "%Y-%m-%d")))

SLRC.Defuncion <- read_csv("Bases/ST_SLRCDefuncion_SSFED.csv", 
                             col_types = cols(fecha_def = col_date(format = "%Y-%m-%d"))) %>% rename(Fecha=fecha_def) %>% filter(Fecha>as.Date("2020-03-03"))
SLRC.Sintomas <- read_csv("Bases/ST_SLRCSintomas_SSFED.csv", 
                            col_types = cols(fecha_sintomas = col_date(format = "%Y-%m-%d"))) %>% rename(Fecha=fecha_sintomas) %>% filter(Fecha>as.Date("2020-03-03"))

dia<-max(as.Date(SLRC.DF$fecha_reporte))

lundom <- weekdays(dia)
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


# Carga base estatal
SLRC.DF <- read_csv("Bases/ST_SLRCReporte_SSFED.csv", 
                      col_types = cols(fecha_reporte = col_date(format = "%Y-%m-%d"))) %>% 
  rename(Fecha=fecha_reporte, Pruebas=Resultados_lab, Confirmados=Casos) 
SLRC.DF <- mutate(SLRC.DF, Positividad.acum= round((Confirmados_lab / Pruebas)*100,1))
dia.act <- max(as.Date(SLRC.DF$Fecha))-14

# Gráfico estatal
Casossemana <- SLRC.DF %>% mutate(diasemana = weekdays(Fecha)) %>% 
  filter(diasemana==lundom)

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

SLRCacum <- SLRC.DF %>% filter( Fecha == max(Fecha))

Fechas <- data.frame(Fecha=as.Date(SLRC.DF$Fecha)) 

Sintomassemanas <- SLRC.Sintomas %>% 
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

Decesossemanas <- SLRC.Defuncion %>% 
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
SLRC.DF.hoy <- filter(SLRC.DF, Fecha == max(Fecha))
SLRC.DF.hoy <- select(SLRC.DF.hoy, Hospitalizados.Activos, Ambulatorios.Activos, Decesos, Recuperados)
SLRC.DF.hoy <- rename(SLRC.DF.hoy, "Ambulatorios activos"= Ambulatorios.Activos, "Hospitalizados Activos"=Hospitalizados.Activos)
SLRC.DF.hoy <- gather(SLRC.DF.hoy, key= Estatus, value= Casos.confirmados) 
tituestatus <- paste0("<span style = 'font-size:14pt'>Covid-19 en SLRC:</span><br>Estatus de los <span style = 'color:#01A2AC';>", prettyNum(as.numeric(sum(SLRC.DF.hoy$Casos.confirmados)), big.mark=",", preserve.width="none"), "</span> casos confirmados")


SLRC.DF.hoy$label <- c(paste0(SLRC.DF.hoy$Estatus, "\n ",prettyNum(as.numeric(SLRC.DF.hoy$Casos.confirmados), big.mark=",", preserve.width="none"), "\n ", (round((SLRC.DF.hoy$Casos.confirmados/(sum(SLRC.DF.hoy$Casos.confirmados))*100), digits = 1)),"%"))
Hosp.hoy <- SLRC.DF.hoy %>% filter(Estatus=="Hospitalizados Activos")

hosplab <- Hosp.hoy$label

Estatus <- ggplot(SLRC.DF.hoy, aes(area = Casos.confirmados, fill= Estatus, label= SLRC.DF.hoy$label)) + geom_treemap( size=2, color= "white") +
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

ggsave("Gráficos semanales/SLRC/SLRC/s02.png",Estatus , width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


# Casos semanal Estatal

Casossemson <- ggplot(Casossemana) +
  geom_col(aes(x=Fecha, y= Casos.semana, fill= Casos.semana), color= "#005156", size=0.15) +
  scale_fill_gradient2(low = "#DEF2F2", mid= "#01A2AC", high = "#005156", midpoint = 75) +
  geom_text( aes(x=Fecha, y= Casos.semana, label= Casos.semana), family="Lato Black", size= 3, color="white", angle=90, hjust = 1.1) +
  scale_x_date(expand=c(0,5), breaks = Casossemana$Fecha, date_labels = "%d/%m") +
  # scale_x_date(expand=c(0,5), date_breaks = "1 month", date_labels = "%B") +
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_minimal() +
  temasinejes +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en SLRC:</span><br><span style = 'color:#01A2AC';>Casos confirmados semanalmente</span>", 
       subtitle= Fechasem, caption =fuente)
Casossemson
ggsave("Gráficos semanales/SLRC/s03.png",Casossemson, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)

CasossemSintomas <- ggplot(Sintomassemanas) +
  geom_col(data=subset(Sintomassemanas, Fecha <=dia.act),aes(x=Fecha, y= Casos.semana, fill= Casos.semana), color= "#005156", size=0.15) +
  geom_col(data=subset(Sintomassemanas, Fecha >dia.act),aes(x=Fecha, y= Casos.semana), fill= "gray75", size=0.15, alpha=0.7) +
  scale_fill_gradient2(low = "#DEF2F2", mid= "#01A2AC", high = "#005156", midpoint = 75) +
  geom_text( aes(x=Fecha, y= Casos.semana, label= Casos.semana), family="Lato Black", size= 2.5, color="white", angle=90, hjust = 1.1) +
  scale_x_date(expand=c(0,5), breaks = Sintomassemanas$Fecha, date_labels = "%d/%m") +
  # scale_x_date(expand=c(0,5), date_breaks = "1 month", date_labels = "%B") +
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_minimal() +
  temasinejes +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en SLRC:</span><br><span style = 'color:#01A2AC';>Casos confirmados por semana de inicio de síntomas</span>", 
       subtitle= Fechasem, caption =fuentefech)
CasossemSintomas

ggsave("Gráficos semanales/SLRC/s03sint.png",CasossemSintomas, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


# Decesos diarios Estatal

Decesossemson <- ggplot(Casossemana) +
  geom_col(aes(x=Fecha, y= Decesos.semana, fill= Decesos.semana), color= "#4D1933", size=0.15) +
  scale_fill_gradient2(low = "#F0D1E0", mid= "#993366", high = "#4D1933", midpoint = 12) +
  geom_text( aes(x=Fecha, y= Decesos.semana, label= Decesos.semana), family="Lato Black", size= 3, color="white", angle=90, hjust = 1.1) +
  scale_x_date(expand=c(0,5), breaks = Casossemana$Fecha, date_labels = "%d/%m") +
  # scale_x_date(expand=c(0,5), date_breaks = "1 month", date_labels = "%B") +
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_minimal() +
  temasinejes +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en SLRC:</span><br><span style = 'color:#993366';>Decesos confirmados semanalmente</span>", 
       subtitle= Fechasem, caption =fuente)
Decesossemson

ggsave("Gráficos semanales/SLRC/s09.png",Decesossemson, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)

DecesosDefuncion <- ggplot(Decesossemanas) +
  geom_col(data=subset(Decesossemanas, Fecha <=dia.act), aes(x=Fecha, y= Decesos.semana, fill= Decesos.semana), color= "#4D1933", size=0.15) +
  geom_col(data=subset(Decesossemanas, Fecha > dia.act), aes(x=Fecha, y= Decesos.semana), fill= "gray75", size=0.15, alpha=0.7) +
  scale_fill_gradient2(low = "#F0D1E0", mid= "#993366", high = "#4D1933", midpoint = 12) +
  geom_text( aes(x=Fecha, y= Decesos.semana, label= Decesos.semana), family="Lato Black", size= 2.5, color="white", angle=90, hjust = 1.1) +
  scale_x_date(expand=c(0,5), breaks = Decesossemanas$Fecha, date_labels = "%d/%m") +
  # scale_x_date(expand=c(0,5), date_breaks = "1 month", date_labels = "%B") +
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_minimal() +
  temasinejes +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en SLRC:</span><br><span style = 'color:#993366';>Decesos confirmados por semana de ocurridos</span>", 
       subtitle= Fechasem, caption =fuentefech)
DecesosDefuncion


ggsave("Gráficos semanales/SLRC/s09d.png",DecesosDefuncion, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)

# Hospitalizados diarios Estatal

Hospsemana <- Casossemana %>% filter(Fecha >= as.Date("2020-10-15"))
Hospsemson <- ggplot(Hospsemana) +
  geom_col(aes(x=Fecha, y= Hospitalizados.Activos, fill= Hospitalizados.Activos), color= "#984807", size=0.15) +
  scale_fill_gradient2(low = "#F79646", mid=  "#E46C0A", high = "#984807", midpoint = 5) +
  geom_text( aes(x=Fecha, y= Hospitalizados.Activos, label= Hospitalizados.Activos), family="Lato Black", size= 3, color="white", angle=90, hjust = 1.1) +
  scale_x_date(expand=c(0,5), breaks = Hospsemana$Fecha, date_labels = "%d/%m") +
  # scale_x_date(expand=c(0,5), date_breaks = "1 month", date_labels = "%B") +
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_minimal() +
  temasinejes +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en SLRC:</span><br><span style = 'color:#F79646';>Casos activos hospitalizados al cierre de semana</span>", 
       subtitle= Fechadom, caption =fuente)
 Hospsemson

ggsave("Gráficos semanales/SLRC/s04.png",Hospsemson, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)

# Activos leves

Activossem <- Casossemana %>% filter(Fecha >= as.Date("2020-10-15"))
Activos <- ggplot(Activossem) +
  geom_col(aes(x=Fecha, y= Ambulatorios.Activos, fill= Ambulatorios.Activos), color= "#3B9494", size=0.15) +
  scale_fill_gradient2(low = "#BCE4E4", mid= "#58BCBC", high = "#3B9494", midpoint = 40) +
  geom_text( aes(x=Fecha, y= Ambulatorios.Activos, label= Ambulatorios.Activos), family="Lato Black", size= 3, color="white", angle=90, hjust = 1.1) +
  scale_x_date(expand=c(0,5), breaks = Casossemana$Fecha, date_labels = "%d/%m") +
  # scale_x_date(expand=c(0,5), date_breaks = "1 month", date_labels = "%B") +
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_minimal() +
  temasinejes +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en SLRC:</span><br><span style = 'color:#58BCBC';>Ambulatorios activos cierre de semana</span>", 
       subtitle= Fechadom, caption =fuente)
Activos

ggsave("Gráficos semanales/SLRC/s05.png",Activos, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)

# Pruebas semanales

Pruebassemson <- ggplot(Casossemana) +
  geom_col(aes(x=Fecha, y= Pruebas.semana, fill= Pruebas.semana), color="#215968", size=0.15) +
  scale_fill_gradient2(low = "#B7DEE8", mid= "#4BACC6", high = "#215968", midpoint = 200) +
  geom_text( aes(x=Fecha, y= Pruebas.semana, label= Pruebas.semana), family="Lato Black", size= 3, color="white", angle=90, hjust = 1.1) +
  scale_x_date(expand=c(0,5), breaks = Casossemana$Fecha, date_labels = "%d/%m") +
  # scale_x_date(expand=c(0,5), date_breaks = "1 month", date_labels = "%B") +
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_minimal() +
  temasinejes +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en SLRC:</span><br><span style = 'color:#4BACC6';>Resultados válidos de pruebas semanales</span>", 
       subtitle= Fechasem, caption =fuentepruebas)
Pruebassemson

ggsave("Gráficos semanales/SLRC/s11.png",Pruebassemson, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)

Semanamax <- Casossemana %>% filter(Fecha==max(Fecha))
Sospsemson <- ggplot(Casossemana) +
  geom_line(aes(x=Fecha, y= Sospechosos), color="#A96AC2", size=1, alpha=0.75) +
  geom_point( data = subset(Casossemana , Fecha == max(Fecha)), aes(x=Fecha, y= Sospechosos), fill="#A96AC2", size=2 , shape=21, color="white", stroke=1) +
  geom_dl( data = subset(Casossemana , Fecha == max(Fecha)), 
           aes(x=Fecha, y= Sospechosos, label = paste0("\n\n\n", Sospechosos,"\nsospechosos\n(", if_else(Semanamax$Sospechosos.semana<0,"-","+"),Semanamax$Sospechosos.semana," respecto al\ndomingo anterior)", sep="")), color="#A96AC2", 
           method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.8, fontfamily= "Lato Black")) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-10-15"), (max(as.Date(Casossemana$Fecha)) + 80)), date_breaks = "1 month", date_labels = "%B") +
  # scale_x_date(expand=c(0,5), date_breaks = "1 month", date_labels = "%B") +
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_minimal() +
  temaejes +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en SLRC:</span><br><span style = 'color:#A96AC2';>Casos sospechosos al cierre de semana</span>", 
       subtitle= Fechadom, caption =fuente)
Sospsemson

ggsave("Gráficos semanales/SLRC/s26.png",Sospsemson, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)

 #Positividad

library(directlabels)
Positividad <- ggplot() +
  geom_line(data= SLRC.DF, aes(x=Fecha, y= Positividad.acum), color= "#215968", linetype= "solid", size=1, alpha=0.6)+
  geom_line(data= Casossemana, aes(x=Fecha, y= Positividad.semanal), color= "#4BACC6", linetype= "solid", size=1, alpha=0.6)+
  geom_point( data = subset(SLRC.DF , Fecha == max(Fecha)), aes(x=Fecha, y= Positividad.acum), fill="#215968", size=2 , shape=21, color="white", stroke=1) +
  geom_point( data = subset(Casossemana , Fecha == max(Fecha)), aes(x=Fecha, y= Positividad.semanal), fill="#4BACC6", size=2 , shape=21, color="white", stroke=1) +
  geom_dl( data = subset(SLRC.DF , Fecha == max(Fecha)), aes(x=Fecha, y= Positividad.acum, label = paste0("Positividad\nacumulada\n", Positividad.acum,"%", sep="")), color="#215968", 
           method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.6, fontfamily= "Lato Black")) +
  geom_dl( data = subset(Casossemana , Fecha == max(Fecha)), aes(x=Fecha, y= Positividad.semanal,  label = paste0("Positividad\núlt. 7 días\n", Positividad.semanal,"%", sep="")), color="#4BACC6", 
           method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.6, fontfamily= "Lato Black")) +
  scale_y_continuous(expand = c(0, 0), limits= c(0,100), breaks=seq(0,100,20)) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-10-15"), (max(as.Date(SLRC.DF$Fecha)) + 60)), date_breaks = "1 month", date_labels = "%B") +
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_bw() +
  temaejes +
  labs(y = "%", 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en SLRC:</span><br><span style = 'color:#4BACC6';>Positividad de resultados válidos de pruebas</span>", 
       subtitle= Fechahoy, caption =fuentepruebas)
Positividad
ggsave("Gráficos semanales/SLRC/s12.png",Positividad, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


Casossemana <- Casossemana %>% mutate(POB=199021) %>% mutate(mortalidad=round(Decesos.semana*1000000/POB,1), incidencia=round(Casos.semana*100000/POB,1))


Indicadores <- ggplot() +
  geom_hline(yintercept=100, linetype="dashed", color = "#CE3F41", alpha=0.5) +
  geom_text(aes(x = as.Date("2021-11-20"), y = (102),
                label = "Alta"), stat = "unique", family = "Lato Black",
            size = 2, color = "#CE3F41",  hjust=0, alpha=0.8) +
  geom_hline(yintercept=50, linetype="dashed", color = "#FFA17B", alpha=0.5) +
  geom_text(aes(x = as.Date("2021-11-20"), y = (52),
                label = "Substancial"), stat = "unique", family = "Lato Black",
            size = 2, color = "#FFA17B",  hjust=0, alpha=0.8) +
  geom_hline(yintercept=10, linetype="dashed", color = "#FECF7D", alpha=0.5) +
  geom_text(aes(x = as.Date("2021-11-20"), y = (12),
                label = "Moderada"), stat = "unique", family = "Lato Black",
            size = 2, color = "#FECF7D",  hjust=0, alpha=0.8) +
  geom_line(data= Casossemana, aes(x=Fecha, y= mortalidad), color= "#993366", linetype= "solid", size=1, alpha=0.8)+
  geom_line(data= Casossemana, aes(x=Fecha, y= incidencia), color= "#01A2AC", linetype= "solid", size=1, alpha=0.8)+
  geom_point( data = subset(Casossemana , Fecha == max(Fecha)), aes(x=Fecha, y= mortalidad), fill="#993366", size=2 , shape=21, color="white", stroke=1) +
  geom_point( data = subset(Casossemana , Fecha == max(Fecha)), aes(x=Fecha, y= incidencia), fill="#01A2AC", size=2 , shape=21, color="white", stroke=1) +
  geom_dl( data = subset(Casossemana, Fecha == max(Fecha)), aes(x=Fecha, y= mortalidad, label = paste0("\n", mortalidad, " decesos\npor millón de habs.", sep="")), color="#993366", 
           method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.8, fontfamily= "Lato Black")) +
  geom_dl( data = subset(Casossemana , Fecha == max(Fecha)), aes(x=Fecha, y= incidencia,  label = paste0(incidencia," casos\npor 100 mil habs.","\n",  sep="")), color="#01A2AC", 
           method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.8, fontfamily= "Lato Black")) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,175)) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-10-15"), (max(as.Date(Casossemana$Fecha)) + 100)), date_breaks = "1 month", date_labels = "%B") +
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_bw() +
  temaejes + theme(axis.title.y = element_markdown(family = "Lato", size =6)) +
  labs(y = "<span style = 'color:#01A2AC';>Incidencia (casos por 100 mil de habs.)</span><br><span style = 'color:#993366';>Mortalidad (decesos por millón de habs.)</span>", 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en SLRC:</span><br><span style = 'color:#01A2AC';>Incidencia</span> y <span style = 'color:#993366';>mortalidad</span> por semana", 
       subtitle= Fechahoy, caption =fuente)
Indicadores
ggsave("Gráficos semanales/SLRC/s18.png",Indicadores, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


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
  scale_y_continuous(expand = c(0, 0), limits = c(0,230)) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-10-15"), (max(as.Date(Casos2021$Fecha)+60))), date_breaks = "1 month", date_labels = "%B") +
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_bw() +
  temaejes + theme(axis.title.y = element_markdown(family = "Lato", size =6)) +
  labs(y = NULL, 
       x = NULL,legend= NULL, 
       title  = "<span style = 'font-size:14pt'>Covid-19 en SLRC:</span><br>Casos <span style = 'color:#2D6669';>recuperados</span> y <span style = 'color:#01A2AC';>confirmados</span> por semana", 
       subtitle= Fechasem, caption =fuente)
Recuperados
ggsave("Gráficos semanales/SLRC/s22.png",Recuperados, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


c14 <- ggplot(Casossemana) +
  geom_col(aes(x=Fecha, y= C_0_14, fill= C_0_14), color= "#005156", size=0.15) +
  scale_fill_gradient2(low = "#DEF2F2", mid= "#01A2AC", high = "#005156", midpoint = 3) +
  geom_text( aes(x=Fecha, y= C_0_14, label= C_0_14), family="Lato Black", size= 3, color="white", angle=90, hjust = 1.1) +
  scale_x_date(expand=c(0,5), breaks = Casossemana$Fecha, date_labels = "%d/%m") +
  # scale_x_date(expand=c(0,5), date_breaks = "1 month", date_labels = "%B") +
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_minimal() +
  temasinejes +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en SLRC:</span><br><span style = 'color:#01A2AC';>Casos de 0 a 14 años de edad por semana</span>", 
       subtitle= Fechasem, caption =fuente)
c14

ggsave("Gráficos semanales/SLRC/c0_14.png",c14, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)

Acumedad <- SLRCacum %>% select(C_0_14, C_15_29,C_30_59,C_60_mas) %>% 
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
  geom_line(aes(x= corte, y= casos, color= "Tendencia promedio móvil 7 días"), linetype= "solid", size=.4)+
  
  scale_color_manual(name="", values= c("Tendencia promedio móvil 7 días" = "#01787E", "Casos diarios" = "white")) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_date(expand=c(0,5), date_breaks = "2 month", date_labels = "%B \n %Y") +
  facet_wrap(~label, scales = "free")+
  theme_minimal() + temadiario  +
  theme(legend.text = element_text(family = "Lato", size = 7), legend.background = element_rect(fill="transparent"),
        legend.position = "none",  legend.justification="left", legend.margin=margin(t = 0, unit='cm'), axis.text = element_text(size=6), strip.text = element_text(size=9),
        panel.grid.minor = element_blank(),
        legend.key = element_rect(fill="transparent", color="transparent"), strip.background = element_blank()) +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en SLRC:</span><br><span style = 'color:#01A2AC';>Casos confirmados por semana</span>", 
       subtitle= Fechasem, caption =fuente)

CasosEdadG

ggsave("Gráficos semanales/SLRC/s07.png",CasosEdadG, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)

AcumedadD <- SLRCacum %>% select(D_0_14, D_15_29,D_30_59,D_60_mas) %>% 
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
  geom_line(aes(x= corte, y= decesos, color= "Tendencia promedio móvil 7 días"), linetype= "solid", size=.4)+
  scale_color_manual(name="", values= c("Tendencia promedio móvil 7 días" = "#73264D")) +
  scale_y_continuous(expand = c(0,0), breaks = int_breaks_rounded) +
  scale_x_date(expand=c(0,5), date_breaks = "2 month", date_labels = "%B \n %Y") +
  facet_wrap(~label, scales = "free")+
  theme_minimal() + temadiario  +
  theme(legend.text = element_text(family = "Lato", size = 7), legend.background = element_rect(fill="transparent"),
        legend.position = "none",  legend.justification="left", legend.margin=margin(t = 0, unit='cm'), axis.text = element_text(size=6), strip.text = element_text(size=9),
        panel.grid.minor = element_blank(),
        legend.key = element_rect(fill="transparent", color="transparent"), strip.background = element_blank()) +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en SLRC:</span><br><span style = 'color:#73264D';>Decesos confirmados por semana</span>", 
       subtitle= Fechasem, caption =fuenteedad)

DecesosEdadG

ggsave("Gráficos semanales/SLRC/s10.png",DecesosEdadG, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


SINTOMAS <- read_csv("Bases/COVIDSLRC_SINTOMAS.csv", 
                     col_types = cols(X1 = col_skip(), fecha_sintomas = col_date(format = "%Y-%m-%d")), 
                     locale = locale(encoding = "ISO-8859-1"))
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
                    plot.subtitle = element_text(family = "Lato Light", size = 10, color = "black"), legend.title = element_blank(),
                    strip.text = element_text(family = "Lato Black", size = 8, hjust=0),
                    axis.text = element_text(family = "Lato", size =4),
                    plot.background = element_rect(fill = "white", color = "white", size = 3),
                    axis.title.x = element_text(family = "Lato Light", size = 6, hjust=1),
                    axis.title.y = element_text(family = "Lato Light", size = 6, hjust=1), 
                    plot.caption = element_text(family = "Lato", size = 6),
                    legend.text = element_blank(), legend.background =  element_rect(fill="transparent", color="transparent"),
                    legend.box.background=  element_rect(fill="transparent", color="transparent" ), 
                    legend.key = element_rect(fill="transparent", color="transparent"),
                    legend.position = "none", plot.title.position = 'plot', plot.caption.position = 'plot')

Hospitalizados <- sintomassemana  %>%
  filter(fecha_sintomas<dia-14) %>%  ggplot(aes(x= fecha_sintomas, y= hospitalización)) +
  geom_line(color= "#F79646", linetype= "solid", size=1, alpha=0.8)+
  geom_area(color= "transparent", fill= "#F79646",alpha=0.1)+
  geom_point( data = subset(sintomassemana , fecha_sintomas == max(fecha_sintomas)-14), fill="white", size=1 , shape=21, color="#F79646", stroke=1) +
  geom_dl( data = subset(sintomassemana, fecha_sintomas == max(fecha_sintomas)-14), aes(label = paste0(hospitalización,"%")), color="#F79646", method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 1.5, fontfamily= "Lato Black")) +
  scale_y_continuous(expand = c(0, 0), limits= c(0,20), breaks=seq(0,20,5)) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-08-01"), max(as.Date(sintomassemana$fecha_sintomas)+60)), date_breaks = "1 month", date_labels = "%B") +
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_bw() +
  temadiario + theme(axis.text.y= element_text(size=10)) +
  labs(y = "Hospitalizados por cada 100 casos confirmados", 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en SLRC:</span><br><span style = 'color:#F79646';>Hospitalización<br>por semana de inicio de síntomas</span>", 
       subtitle= paste0("Casos confirmados que requirieron hospitalización respecto al total por semana de inicio de síntomas \n", Fechahoy, " con fecha de inicio de síntomas hasta el ",day(dia-14), "/", month(dia-14),"/", year(dia-14)), caption =fuentedes)  

Hospitalizados
ggsave("Gráficos semanales/SLRC/s20.png",Hospitalizados , width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


Letalidad <- sintomassemana %>% filter(fecha_sintomas<dia-21) %>%ggplot(aes(x= fecha_sintomas, y= letalidad)) +
  geom_line(color= "#73264D", linetype= "solid", size=1, alpha=0.8)+
  geom_area(color= "transparent", fill= "#D075A3",alpha=0.1)+
  geom_point( data = subset(sintomassemana , fecha_sintomas == max(fecha_sintomas)-21), fill="white", size=1 , shape=21, color="#73264D", stroke=1) +
  geom_dl( data = subset(sintomassemana, fecha_sintomas == max(fecha_sintomas)-21), aes(label = paste0(letalidad,"%")), color="#73264D", method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 1.5, fontfamily= "Lato Black")) +
  scale_y_continuous(expand = c(0, 0), limits= c(0,20), breaks=seq(0,20,5)) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-04-01"), max(as.Date(sintomassemana$fecha_sintomas)+60)), date_breaks = "1 month", date_labels = "%B") +
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_bw() +
  temadiario + theme(axis.text.y= element_text(size=10)) +
  labs(y = "Decesos por cada 100 casos confirmados", 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en SLRC:</span><br><span style = 'color:#73264D';>Letalidad<br>por semana de inicio de síntomas</span>", 
       subtitle= paste0("Casos confirmados fallecidos respecto al total por semana de inicio de síntomas \n", Fechahoy, " con fecha de inicio de síntomas hasta el ",day(dia-21), "/", month(dia-14),"/", year(dia-14)), caption =fuentedes)  

Letalidad
ggsave("Gráficos semanales/SLRC/s24.png",Letalidad , width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


Menores <- Casossemana %>% ggplot(aes(x= Fecha, y= pctj_menores)) +
  geom_line(color= "#01A2AC", linetype= "solid", size=1, alpha=0.8)+
  geom_area(color= "transparent", fill= "#01A2AC",alpha=0.1)+
  geom_point( data = subset(Casossemana , Fecha == max(Fecha)), fill="white", size=1 , shape=21, color="#01A2AC", stroke=1) +
  geom_dl( data = subset(Casossemana, Fecha == max(Fecha)), aes(label = paste0(pctj_menores,"%")), color="#01A2AC", method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 1.5, fontfamily= "Lato Black")) +
  scale_y_continuous(expand = c(0, 0), limits= c(0,12), breaks=seq(0,12,2)) +
  scale_x_date(expand=c(0,0), limits = c(min(as.Date(Casossemana$Fecha)), max(as.Date(Casossemana$Fecha)+60)), date_breaks = "1 month", date_labels = "%B") +
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_bw() +
  temadiario + theme(axis.text.y= element_text(size=8)) +
  labs(y = "Casos en menores de 15 años por cada 100 casos confirmados", 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en SLRC:</span><br><span style = 'color:#01A2AC';>Casos pediátricos confirmados<br>respecto al total reportado</span>", 
       subtitle= paste0("Casos confirmados en menores de 15 años respecto a los confirmados totales según semana de reporte\n", Fechahoy), caption =fuentedes)  

Menores
ggsave("Gráficos semanales/SLRC/s25.png",Menores , width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


