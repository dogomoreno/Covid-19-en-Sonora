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

Sonora.DF <- read_csv("Bases/ST_SonoraReporte_SSFED.csv", 
                      col_types = cols(fecha_reporte = col_date(format = "%Y-%m-%d")))

dia<-max(as.Date(Sonora.DF$fecha_reporte))

Fechahoy <- paste0("Al reporte del ", day(dia), " de ", months.Date(dia)," de ", year(dia))
fuente <- "Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud del Gobierno de la República\n*Por fecha de reporte | www.luisarmandomoreno.com"
subtitulo <- paste0("Casos confirmados en los últimos 7 días por 100 mil habitantes\nAl reporte del ",day(dia),"/",month(dia),"/",year(dia))
subtituloact <- paste0("Casos activos (que iniciaron síntimas dentro de los 14 días previos) por 100 mil habitantes.\nAl reporte del ",day(dia),"/",month(dia),"/",year(dia))

POBMUN <- read_csv("Bases/POBMUN.csv", col_types = cols(CVEGEO = col_character()), 
                   locale = locale(encoding = "ISO-8859-1"))

temaejes <- theme(plot.margin = margin(10, 25, 10, 25),
                  plot.title = element_markdown(family = "Lato Black", size = 25),  
                  plot.subtitle = element_text(family = "Lato Light", size = 10, color = "black"), legend.title = element_blank(),
                  strip.text = element_text(family = "Lato Black", size = 10),
                  axis.text = element_text(family = "Lato", size =6),
                  plot.background = element_rect(fill = "white", color = "white", size = 3),
                  axis.title.x = element_text(family = "Lato Light", size = 8, hjust=1),
                  axis.title.y = element_text(family = "Lato Light", size = 8, hjust=1), 
                  plot.caption = element_text(family = "Lato", size = 6),
                  legend.text = element_blank(), legend.background =  element_rect(fill="transparent", color="transparent"),
                  legend.box.background=  element_rect(fill="transparent", color="transparent" ), 
                  legend.key = element_rect(fill="transparent", color="transparent"),
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
                      legend.box.background=  element_rect(fill="transparent", color="transparent" ), 
                      legend.key = element_rect(fill="transparent", color="transparent"),
                      legend.position = "none",  legend.justification="left", plot.title.position = 'plot', plot.caption.position = 'plot')

temasmap <-  theme(axis.line = element_blank(),
                      plot.margin = margin(10, 10, 10, 10),
                      plot.title = element_markdown(family = "Lato Black", size = 20),  
                      plot.subtitle = element_text(family = "Lato Light", size = 8, color = "black"), legend.title = element_blank(),
                      axis.text= element_blank(),
                      plot.background = element_rect(fill = "white", color = "white", size = 3),
                      axis.title= element_blank(), 
                      plot.caption = element_text(family = "Lato", size = 5, color = "black"),
                      legend.text = element_text(family = "Lato", size = 6),
                      legend.position = "none",  legend.justification="left", plot.title.position = 'plot', plot.caption.position = 'plot')



# Carga base estatal
Sonora.DF <- read_csv("Bases/ST_SonoraReporte_SSFED.csv", 
                      col_types = cols(fecha_reporte = col_date(format = "%Y-%m-%d"))) %>% filter(fecha_reporte>as.Date("2020-04-01"))
Sonora.DF.hoy <- filter(Sonora.DF, fecha_reporte == dia)
Sonora.DF.hoy <- select(Sonora.DF.hoy, Hospitalizados.Activos, Ambulatorios.Activos, Decesos, Recuperados)
Sonora.DF.hoy <- rename(Sonora.DF.hoy, "Ambulatorios activos"= Ambulatorios.Activos, "Hospitalizados Activos"=Hospitalizados.Activos)
Sonora.DF.hoy <- gather(Sonora.DF.hoy, key= Estatus, value= Casos.confirmados) 

#Treemap

Sonora.DF.hoy <- filter(Sonora.DF, fecha_reporte == max(fecha_reporte))
Sonora.DF.hoy <- select(Sonora.DF.hoy, Hospitalizados.Activos, Ambulatorios.Activos, Decesos, Recuperados)
Sonora.DF.hoy <- rename(Sonora.DF.hoy, "Ambulatorios activos"= Ambulatorios.Activos, "Hospitalizados Activos"=Hospitalizados.Activos)
Sonora.DF.hoy <- gather(Sonora.DF.hoy, key= Estatus, value= Casos.confirmados) 
tituestatus <- paste0("<span style = 'font-size:14pt'>Covid-19 en Sonora:</span><br>Estatus de los <span style = 'color:#01A2AC';>", prettyNum(as.numeric(sum(Sonora.DF.hoy$Casos.confirmados)), big.mark=",", preserve.width="none"), "</span> casos confirmados")


Sonora.DF.hoy$label <- c(paste0(Sonora.DF.hoy$Estatus, "\n ",prettyNum(as.numeric(Sonora.DF.hoy$Casos.confirmados), big.mark=",", preserve.width="none"), "\n ", (round((Sonora.DF.hoy$Casos.confirmados/(sum(Sonora.DF.hoy$Casos.confirmados))*100), digits = 1)),"%"))
Hosp.hoy <- Sonora.DF.hoy %>% filter(Estatus=="Hospitalizados Activos")

hosplab <- Hosp.hoy$label

Estatus <- ggplot(Sonora.DF.hoy, aes(area = Casos.confirmados, fill= Estatus, label= Sonora.DF.hoy$label)) + geom_treemap( size=2, color= "white") +
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


ggsave("Gráficos diarios/Diarioestatus.png",Estatus , width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)

# Bases municipales
Casos <- read_csv("Bases/Casosdiarios_SSFED.csv", 
                  col_types = cols(CASOS = col_integer(), 
                                   CVEGEO = col_character(), Fecha = col_date(format = "%Y-%m-%d"), 
                                   MUNICIPIO = col_character(), NUEVOS = col_integer(), X1 = col_skip()), 
                  locale = locale(encoding = "ISO-8859-1")) %>% filter(Fecha>as.Date("2020-10-08"))
casosacumdia <- Casos %>% filter(Fecha==max(as.Date(Fecha))) %>% filter(NUEVOS!=0) %>% select (Fecha, MUNICIPIO, CASOS, NUEVOS) %>% arrange(desc(NUEVOS)) %>% 
write.csv('ResultadoCSV/casoshoy.csv')

Activosmun <- read_csv("Bases/Activosdiarios_SSFED.csv", 
                  col_types = cols(CASOS_ACTIVOS = col_integer(), 
                                   CVEGEO = col_character(), Fecha = col_date(format = "%Y-%m-%d"), 
                                   MUNICIPIO = col_character(), X1 = col_skip()), 
                  locale = locale(encoding = "ISO-8859-1"))
Activosmundia <- Activosmun %>% filter(Fecha==max(as.Date(Fecha))) %>% filter(CASOS_ACTIVOS!=0) %>% select (Fecha, MUNICIPIO, CASOS_ACTIVOS) %>% arrange(desc(CASOS_ACTIVOS)) %>% 
  write.csv('ResultadoCSV/Activosmunhoy.csv')



Decesos <- read_csv("Bases/Decesosdiarios_SSFED.csv", 
                    col_types = cols(DECESOS = col_integer(), 
                                     CVEGEO = col_character(), Fecha = col_date(format = "%Y-%m-%d"), 
                                     MUNICIPIO = col_character(), NUEVOS = col_integer(), X1 = col_skip()), 
                    locale = locale(encoding = "ISO-8859-1")) %>% filter(Fecha>as.Date("2020-10-08"))
decesosacumdia <- Decesos %>% filter(Fecha==max(as.Date(Fecha))) %>% filter(NUEVOS!=0) %>% select (Fecha, MUNICIPIO, DECESOS, NUEVOS) %>% arrange(desc(NUEVOS)) %>% 
  write.csv('ResultadoCSV/decesoshoy.csv')


#Casos trayectoria Promedio vs Acumulado

Casosprom <- Casos %>% group_by(MUNICIPIO) %>% mutate(Casos.media.7d=round(rollmeanr(x=NUEVOS, 7, fill = 0),1)) %>%  filter(CASOS>500) 

Casosd <- ggplot(subset(Casosprom, MUNICIPIO %in% c("Hermosillo", "Cajeme"))) +
  geom_line(mapping = aes(x = Fecha, y = Casos.media.7d, color= MUNICIPIO), size=0.75, alpha=0.8, arrow=arrow(type="open", length=unit(0.10,"cm"))) +
  coord_cartesian(expand = TRUE, clip = 'off') +
  scale_color_manual(values=c("#F79646", "#01A2AC")) + 
  theme_minimal() + temaejes + theme(
        legend.text = element_text(family = "Lato", size = 8),
        legend.position = c(0.85,0.85), legend.justification="left") +
  labs(y = "Casos diarios\n(promedio móvil 7 días)", 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Sonora:</span><br><span style = 'color:#01A2AC';>Trayectoria de casos en los municipios</span>", 
       subtitle= Fechahoy, caption =fuente) 
  #scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10")

ggsave("Gráficos diarios/diariocasostray.png",Casosd , width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)

#Decesos trayectoria Promedio vs Acumulado

Decesosprom <- Decesos %>% group_by(MUNICIPIO) %>% mutate(Decesos.media.7d=round(rollmeanr(x=NUEVOS, 7, fill = 0),1)) 

Decesosd <- ggplot(subset(Decesosprom, MUNICIPIO %in% c("Hermosillo", "Cajeme"))) +
  geom_line(mapping = aes(x = Fecha, y = Decesos.media.7d, color= MUNICIPIO), size=0.75, alpha=0.8, arrow=arrow(type="open", length=unit(0.10,"cm"))) +
  scale_color_locuszoom() + 
  coord_cartesian(expand = TRUE, clip = 'off') +
  theme_minimal() + temaejes + theme(
    legend.text = element_text(family = "Lato", size = 8),
    legend.position = "right", legend.justification="left") +
  labs(y = "Casos diarios\n(promedio móvil 7 días, log)", 
       x = "Casos acumulados (log)",legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Sonora:</span><br><span style = 'color:#993366';>Trayectoria de decesos en los municipios</span>", 
       subtitle= Fechahoy, caption =fuente) + 
  scale_y_continuous (expand = c(0, 0)) + 
  scale_x_continuous (expand = c(0, 0))

ggsave("Gráficos diarios/diariodecesostray.png",Decesosd , width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)




# Mapa incidencia

#discrete <- c("5" = "black", "4" = "#005155","3" = "#01787E","2" = "#01A2AC", "1" = "#58BCBC")


Casossemana <- Casos %>% group_by(MUNICIPIO) %>% 
  mutate(diasemana = weekdays(Fecha), Casossemana = rollsum(NUEVOS, 7, align="right", fill = 0)) %>% 
  filter(diasemana==weekdays(max(as.Date(Fecha)))) %>% 
  left_join(POBMUN, by = "CVEGEO") 
Casossemana <- Casossemana %>% mutate (INCIDENCIA= round((Casossemana*100000)/POB,1))
Casossemana$INCIDENCIA[Casossemana$INCIDENCIA==0] <- NA
# casossempob <- Casossemana %>% 
#   mutate(IS=if_else(INCIDENCIA>(round(quantile(casossempob$INCIDENCIA, 0.90, na.rm=TRUE),0)),5, 
#                     if_else(INCIDENCIA>(round(quantile(casossempob$INCIDENCIA, 0.75, na.rm=TRUE),0)),4, 
#                             if_else(INCIDENCIA>(round(quantile(casossempob$INCIDENCIA, 0.50, na.rm=TRUE),0)),3,
#                                     if_else(INCIDENCIA>(round(quantile(casossempob$INCIDENCIA, 0.25, na.rm=TRUE),0)),2,1))))) %>% 

niveles <- c("1", "2", "3", "4")
casossempob <- Casossemana %>% mutate(IS=if_else(INCIDENCIA>=99.9,4, 
                                                 if_else(INCIDENCIA>49.9,3,
                                                         if_else(INCIDENCIA>9.9,2,1)))) %>% 
  mutate(IS=as.character(IS)) %>% 
  filter(Fecha==max(as.Date(Fecha))) 
casossempob$IS[is.na(casossempob$IS)] <- "NA"

incidencianum <- casossempob %>% count(IS) %>% group_by(IS) %>% summarise(municipios=sum(n)) %>%  mutate(IS=as.character(IS)) %>% rename(romp=IS)
incidencianum[is.na(incidencianum)] <- "NA"


casossempob <- casossempob %>%  mutate(id=CVEGEO)

capa_munison <- readOGR("Shapes", layer="MUNSON")
capa_reg <- readOGR("Shapes", layer="REGSON")
capa_munison_df <- fortify(capa_munison, region="concat")
capa_munison_inci<- inner_join(capa_munison_df, casossempob, by="id")


#discrete <-  rev(carto_pal(5, "Temps"))
romp <- c("4", "3", "2", "1", "NA")
discrete <- c("#CE3F41","#FFA17B","#FECF7D", "#31859C","gray90")
marcas <- c( "Alta\n(100 o más)", "Substancial\n(50-99)", "Moderada\n(10-49)","Baja\n(>0-9)", "Nula\n(0)")


incidenciamarcas <- data.frame(romp, marcas, discrete) %>% left_join(incidencianum, by="romp")
incidenciamarcas[is.na(incidenciamarcas)] <- 0
incidenciamarcas <- incidenciamarcas %>% mutate(label=paste0(marcas, "\n", municipios," municipios")) 

Mapa_incidencia<- ggplot(capa_munison_inci, aes(map_id = id)) +
  geom_polygon(data=capa_munison, aes(x=long, y=lat, group=group), 
               fill="gray90", color="white", size=0.12) +
  geom_map(aes(fill = factor(IS, romp)),color = "white",size=0.22, map = capa_munison_df) + 
  scale_fill_manual(values = incidenciamarcas$discrete, 
                    drop = F,
                    breaks= incidenciamarcas$romp, 
                    labels = incidenciamarcas$label) +
  theme_void() + temasmap + theme(legend.position = "right",
        legend.key.height = unit (1.1, "cm"), legend.key.width = unit (0.3, "cm"),
        legend.text = element_text(family = "Lato", size = 6, color = "black"),
        legend.title = element_text(family = "Lato Black", size = 5, color = "black"),
        axis.title = element_blank()) +
  labs(y = NULL, x = NULL, title  = "<span style = 'font-size:12pt'>Covid-19 en Sonora:</span><br><span style = 'color:#01A2AC';>Incidencia semanal</span>", 
       subtitle = subtitulo,  fill = NULL, 
       caption =fuente)+
       geom_polygon(data=capa_reg, aes(x=long, y=lat, group=group), 
             fill="transparent", color="black", size=0.2)
Mapa_incidencia
ggsave("Gráficos semanales/s08.png",Mapa_incidencia, width = 5/2 * (16/9), height = 5, type = "cairo", dpi = 400)


Mapa_incivoid<- ggplot(capa_munison_inci, aes(map_id = id)) +
  geom_polygon(data=capa_munison, aes(x=long, y=lat, group=group), 
               fill="gray90", color="white", size=0.12) +
  geom_map(aes(fill = factor(IS)),color = "white",size=0.22, map = capa_munison_df) + 
  scale_fill_manual(values = discrete, 
                    breaks= romp, 
                    labels = marcas) +
  theme_void() +
  theme(plot.title = (element_text(family = "Lato Black", size = 20, color = "black")),
        plot.subtitle = (element_text(family = "Lato Light", size = 8, color = "#01787E")),
        plot.margin = margin(0.5, 0.5, 0.25, 0.4, "cm"),
        legend.position = "none",
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.key.height = unit (0.3, "cm"), legend.key.width = unit (0.3, "cm"), axis.text = element_blank(),
        legend.text = element_text(family = "Lato", size = 6, color = "black"),
        legend.title = element_text(family = "Lato Black", size = 5, color = "black"),
        plot.caption = element_text(family = "Lato Light", size = 6, color = "gray40"),
        axis.title = element_blank()) +
  labs(y = NULL, x = NULL, title  = NULL, 
       subtitle = NULL,  fill = NULL, 
       caption = NULL)+
  geom_polygon(data=capa_reg, aes(x=long, y=lat, group=group), 
               fill="transparent", color="black", size=0.2)
ggsave("Gráficos semanales/smincidenciav.png",Mapa_incivoid, bg = "transparent", height = 12, width = 12, units = "cm", dpi = 800, type = 'cairo')



Activossemana <- Activosmun%>% group_by(MUNICIPIO) %>% 
  mutate(diasemana = weekdays(Fecha)) %>% 
  filter(diasemana==weekdays(max(as.Date(Fecha)))) %>% 
  left_join(POBMUN, by = "CVEGEO") 
Activossemana <- Activossemana %>% mutate (INCIDENCIA= round((CASOS_ACTIVOS*100000)/POB,1))
Activossemana$INCIDENCIA[Activossemana$INCIDENCIA==0] <- NA
# casossempob <- Casossemana %>% 
#   mutate(IS=if_else(INCIDENCIA>(round(quantile(casossempob$INCIDENCIA, 0.90, na.rm=TRUE),0)),5, 
#                     if_else(INCIDENCIA>(round(quantile(casossempob$INCIDENCIA, 0.75, na.rm=TRUE),0)),4, 
#                             if_else(INCIDENCIA>(round(quantile(casossempob$INCIDENCIA, 0.50, na.rm=TRUE),0)),3,
#                                     if_else(INCIDENCIA>(round(quantile(casossempob$INCIDENCIA, 0.25, na.rm=TRUE),0)),2,1))))) %>% 

niveles <- c("1", "2", "3", "4")
Activossempob <- Activossemana %>% mutate(IS=if_else(INCIDENCIA>=99.9,4, 
                                                     if_else(INCIDENCIA>49.9,3,
                                                             if_else(INCIDENCIA>9.9,2,1)))) %>%  
  mutate(IS=as.character(IS)) %>% 
  filter(Fecha==max(as.Date(Fecha))) 
Activossempob$IS[is.na(Activossempob$IS)] <- "NA"

incidenciaAct <- Activossempob %>% count(IS) %>% group_by(IS) %>% summarise(municipios=sum(n)) %>%  mutate(IS=as.character(IS)) %>% rename(romp=IS)
incidenciaAct[is.na(incidenciaAct)] <- "NA"


Activossempob <- Activossempob %>%  mutate(id=CVEGEO)

capa_munison_act<- inner_join(capa_munison_df, Activossempob, by="id")


#discrete <-  rev(carto_pal(5, "Temps"))
romp <- c("4", "3", "2", "1", "NA")
discrete <- c("#CE3F41","#FFA17B","#FECF7D", "#31859C","gray90")
marcas <- c( "Alta\n(100 o más)", "Substancial\n(50-99)", "Moderada\n(10-49)","Baja\n(>0-9)", "Nula\n(0)")


Actmarcas <- data.frame(romp, marcas, discrete) %>% left_join(incidenciaAct, by="romp")
Actmarcas[is.na(Actmarcas)] <- 0
Actmarcas <- Actmarcas %>% mutate(label=paste0(marcas, "\n", municipios," municipios")) 

Mapa_act<- ggplot(capa_munison_act, aes(map_id = id)) +
  geom_polygon(data=capa_munison, aes(x=long, y=lat, group=group), 
               fill="gray90", color="white", size=0.12) +
  geom_map(aes(fill = factor(IS, romp)),color = "white",size=0.22, map = capa_munison_df) + 
  scale_fill_manual(values = Actmarcas$discrete, 
                    drop = F,
                    breaks= Actmarcas$romp, 
                    labels = Actmarcas$label) +
  theme_void() + temasmap + theme(legend.position = "right",
                                  legend.key.height = unit (1.1, "cm"), legend.key.width = unit (0.3, "cm"),
                                  legend.text = element_text(family = "Lato", size = 6, color = "black"),
                                  legend.title = element_text(family = "Lato Black", size = 5, color = "black"),
                                  axis.title = element_blank()) +
  labs(y = NULL, x = NULL, title  = "<span style = 'font-size:12pt'>Covid-19 en Sonora:</span><br><span style = 'color:#01A2AC';>Incidencia de casos activos</span>", 
       subtitle = subtituloact,  fill = NULL, 
       caption =fuente)+
  geom_polygon(data=capa_reg, aes(x=long, y=lat, group=group), 
               fill="transparent", color="black", size=0.2)
Mapa_act
ggsave("Gráficos diarios/diariomapinci.png",Mapa_act, width = 5/2 * (16/9), height = 5, type = "cairo", dpi = 400)


Mapa_incivoid<- ggplot(capa_munison_inci, aes(map_id = id)) +
  geom_polygon(data=capa_munison, aes(x=long, y=lat, group=group), 
               fill="gray90", color="white", size=0.12) +
  geom_map(aes(fill = factor(IS)),color = "white",size=0.22, map = capa_munison_df) + 
  scale_fill_manual(values = discrete, 
                    breaks= romp, 
                    labels = marcas) +
  theme_void() +
  theme(plot.title = (element_text(family = "Lato Black", size = 20, color = "black")),
        plot.subtitle = (element_text(family = "Lato Light", size = 8, color = "#01787E")),
        plot.margin = margin(0.5, 0.5, 0.25, 0.4, "cm"),
        legend.position = "none",
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.key.height = unit (0.3, "cm"), legend.key.width = unit (0.3, "cm"), axis.text = element_blank(),
        legend.text = element_text(family = "Lato", size = 6, color = "black"),
        legend.title = element_text(family = "Lato Black", size = 5, color = "black"),
        plot.caption = element_text(family = "Lato Light", size = 6, color = "gray40"),
        axis.title = element_blank()) +
  labs(y = NULL, x = NULL, title  = NULL, 
       subtitle = NULL,  fill = NULL, 
       caption = NULL)+
  geom_polygon(data=capa_reg, aes(x=long, y=lat, group=group), 
               fill="transparent", color="black", size=0.2)
ggsave("Gráficos diarios/mincidenciav.png",Mapa_incivoid, bg = "transparent", height = 12, width = 12, units = "cm", dpi = 800, type = 'cairo')







# Gráfico estatal


Sonora.DF <- rename(Sonora.DF, Fecha=fecha_reporte)
Sonora.DF <-  filter(Sonora.DF, Fecha>as.Date("2020-04-01"))
Sonora.DF <- mutate(Sonora.DF, Casos.diarios= Casos - lag(Casos, default = Casos[1], order_by=Fecha))
Sonora.DF <- mutate(Sonora.DF, Confirmados.diarios= Confirmados_lab - lag(Confirmados_lab, default = Confirmados_lab[1], order_by=Fecha))
Sonora.DF <- mutate(Sonora.DF, Decesos.diarios= Decesos - lag(Decesos, default = Decesos[1], order_by=Fecha))
Sonora.DF <- mutate(Sonora.DF, Casos.media.7d=round(rollmeanr(x=Casos.diarios, 7, fill = NA),1))
Sonora.DF <- mutate(Sonora.DF, Decesos.media.7d=round(rollmeanr(x=Decesos.diarios,7, fill=NA),1))
Sonora.DF <- mutate(Sonora.DF, Resultados.diarios= Resultados_lab - lag(Resultados_lab, default = Resultados_lab [1]))
Sonora.DF <- mutate(Sonora.DF, Resultados.media.7d=round(rollmeanr(x=Resultados.diarios,7, fill=NA),1))
Sonora.DF <- mutate(Sonora.DF,Ambulatorios.Activos.7d=round(rollmeanr(x=Ambulatorios.Activos,7, fill=NA),1))
Sonora.DF <- mutate(Sonora.DF,Hospitalizados.Activos.7d=round(rollmeanr(x=Hospitalizados.Activos,7, fill=NA),1))
Sonora.DF <- mutate(Sonora.DF, Incidencia= round((Casos / 30.74745),2))
Sonora.DF <- mutate(Sonora.DF, Letalidad= round((Decesos / Casos)*100,1))
Sonora.DF <- mutate(Sonora.DF, Mortalidad= round((Decesos / 30.74745)*100,2))
Sonora.DF <- mutate(Sonora.DF, Positividad.diaria= round((Confirmados.diarios / Resultados.diarios)*100,2))
Sonora.DF <- mutate(Sonora.DF, Positividad= round((Confirmados_lab / Resultados_lab )*100,2))
Sonora.DF <- mutate(Sonora.DF, Analizados.diarios= Analizados - lag(Analizados, default = Analizados[1], order_by=Fecha))
Sonora.DF <- mutate(Sonora.DF, Analizados.media.7d=round(rollmeanr(x=Analizados.diarios, 7, fill = NA),1))

estata.hoy <- Sonora.DF %>% filter(Fecha==max(as.Date(Fecha)))
estata.semana <- Sonora.DF %>% filter(Fecha==(max(as.Date(Fecha))-7))


# Casos diarios Estatal
CasosSon <- ggplot(Sonora.DF) +
  geom_hline(yintercept=estata.semana$Casos.diarios, linetype="dashed", color = "gray80") +
  geom_text(aes(x = as.Date("2020-10-20"), y = (estata.semana$Casos.diarios + 14),
                label = paste0("Semana anterior ", estata.semana$Casos.diarios, " casos")), stat = "unique", family = "Lato Black",
            size = 2, color = "gray80", hjust=0)+
  geom_hline(yintercept=estata.hoy$Casos.diarios, linetype="dashed", color = "red") +
  geom_area(aes(x= Fecha, y= Casos.media.7d), fill= "#58BCBC", alpha=0.3)+
  geom_line(aes(x= Fecha, y= Casos.media.7d, color= "Tendencia promedio móvil 7 días"), linetype= "solid", size=.75, arrow=arrow(type="open", length=unit(0.10,"cm")))+
  geom_point(aes(x= Fecha, y= Casos.diarios), color = "white", fill= "#01787E", size = 0.9, stroke=0.4, alpha=0.65, shape = 21) +
  scale_fill_manual(name="", values= c("Casos diarios" = "#01787E")) + 
  scale_color_manual(name="", values= c("Tendencia promedio móvil 7 días" = "#01787E", "Casos diarios" = "white")) +
  scale_y_continuous(expand = c(0, 15), limits=c(0, max(Sonora.DF$Casos.diarios)+50)) +
  scale_x_date(expand=c(0,5), date_breaks = "1 month", date_labels = "%B") +
  geom_text(aes(x = as.Date("2020-10-20"), y = (estata.hoy$Casos.diarios + 14),
                   label = paste0("Nuevos ", estata.hoy$Casos.diarios, " casos")), stat = "unique", family = "Lato Black",
               size = 3, color = "red",  hjust=0)+
  theme_bw() + temaejes +
  theme(legend.text = element_text(family = "Lato", size = 7), legend.background = element_rect(fill="transparent"),
        legend.position = c(0.02,0.90),  legend.justification="left", legend.margin=margin(t = 0, unit='cm'),
        legend.key = element_rect(fill="transparent", color="transparent")) +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Sonora:</span><br><span style = 'color:#01A2AC';>Casos confirmados diariamente</span>", 
       subtitle= Fechahoy, caption =fuente)

CasosSon

ggsave("Gráficos diarios/diariocasos.png",CasosSon, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)

  
  DecesosSon <- ggplot(Sonora.DF) +
  
  geom_hline(yintercept=estata.semana$Decesos.diarios, linetype="dashed", color = "gray80") +
  geom_text(aes(x = as.Date("2020-10-20"), y = (estata.semana$Decesos.diarios + 1.5),
                  label = paste0("Semana anterior ", estata.semana$Decesos.diarios, " decesos")), stat = "unique", family = "Lato Black",
              size = 2, color = "gray80", hjust=0)+
  geom_hline(yintercept=estata.hoy$Decesos.diarios, linetype="dashed", color = "red") +
  geom_area(aes(x= Fecha, y= Decesos.media.7d), fill= "#D075A3", alpha=0.3)+  
  geom_line(aes(x= Fecha, y= Decesos.media.7d, color= "Tendencia promedio móvil 7 días"), linetype= "solid", size=.75, arrow=arrow(type="open", length=unit(0.10,"cm")))+
  geom_point(aes(x= Fecha, y= Decesos.diarios), color = "white", fill= "#73264D", size = 0.9, stroke=0.4, alpha=0.65, shape = 21) +
  scale_fill_manual(name="", values= c("Decesos diarios" = "#73264D")) + 
  scale_color_manual(name="", values= c("Decesos diarios" = "white","Tendencia promedio móvil 7 días" = "#73264D")) +
  scale_y_continuous(expand = c(0, 0), limits= c(0,80)) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-10-08"), max(as.Date(Sonora.DF$Fecha))+5), date_breaks = "1 month", date_labels = "%B") +
  geom_text(aes(x = as.Date("2020-10-20"), y = estata.hoy$Decesos.diarios + 1.5,
                label = paste0("Nuevos ", estata.hoy$Decesos.diarios, " decesos")), stat = "unique", family = "Lato Black",
            size = 3, color = "red", hjust=0)+
  theme_bw() + temaejes +
  theme(legend.text = element_text(family = "Lato", size = 7), legend.background = element_rect(fill="transparent"),
        legend.position = c(0.02,0.95),  legend.justification="left", legend.margin=margin(t = 0, unit='cm'),
        legend.key = element_rect(fill="transparent", color="transparent")) +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Sonora:</span><br><span style = 'color:#993366';>Decesos confirmados diariamente</span>", 
       subtitle= Fechahoy, caption =fuente)

DecesosSon

ggsave("Gráficos diarios/diariodecesos.png",DecesosSon,  width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)

PruebasSon <- ggplot(Sonora.DF) +
  geom_area(aes(x= Fecha, y= Resultados.media.7d), fill= "#4BACC6", alpha=0.3)+
  geom_line(aes(x= Fecha, y= Resultados.media.7d, color= "Tendencia promedio móvil 7 días"), linetype= "solid", size=.75, arrow=arrow(type="open", length=unit(0.10,"cm")))+
  scale_fill_manual(name="", values= c("Resultados diarios" = "#31859C")) + 
  scale_color_manual(name="", values= c("Tendencia promedio móvil 7 días" = "#31859C", "Resultados diarios" = "white")) +
  scale_y_continuous(expand=c(0,0), limits=c(0, max(Sonora.DF$Resultados.diarios,na.rm=TRUE)+100)) +
  scale_x_date(expand=c(0,10), date_breaks = "1 month", date_labels = "%B") +
  geom_hline(yintercept=estata.hoy$Resultados.diarios, linetype="dashed", color = "red") +
  geom_point(aes(x= Fecha, y= Resultados.diarios), color = "white", fill= "#31859C", size = 0.9, stroke=0.4, alpha=0.65, shape = 21) +
  geom_text(aes(x = as.Date("2020-04-01"), y = estata.hoy$Resultados.diarios+100,
               label = paste0(estata.hoy$Resultados.diarios, " nuevos resultados hoy\n", round(estata.hoy$Positividad.diaria,0), "% positivos")), stat = "unique", family = "Lato Black",
          size = 3, color = "red", hjust=0)+
  # geom_segment(aes(x = as.Date("2021-02-22"), y = 490, xend = as.Date("2021-03-11"), yend = 250),
  #              size = 1.5, color = "black",
  #              arrow = arrow(length = unit(0.02, "npc"))) +
  # geom_text(aes(x = as.Date("2021-02-22"), y = 520,
  #               label = "11/03/2021\n248 resultados"), stat = "unique", family = "Lato Black",
  #           size = 5, color = "black")+
  theme_bw() + temaejes +
  theme(legend.text = element_text(family = "Lato", size = 7), legend.background = element_rect(fill="transparent"),
        legend.position = c(0.02,0.85),  legend.justification="left", legend.margin=margin(t = 0, unit='cm'),
        legend.key = element_rect(fill="transparent", color="transparent")) +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Sonora:</span><br><span style = 'color:#4BACC6';>Resultados informados diariamente</span>", 
       subtitle= Fechahoy, caption =fuente)

PruebasSon

ggsave("Gráficos diarios/diariopruebas.png",PruebasSon, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


Sonora.A <- filter(Sonora.DF, Fecha>=as.Date("2020-11-01"))
AnalizadosSon <- ggplot(Sonora.A) +
  geom_area(aes(x= Fecha, y= Analizados.media.7d), fill= "#4BACC6", alpha=0.3)+
  geom_line(aes(x= Fecha, y= Analizados.media.7d, color= "Tendencia promedio móvil 7 días"), linetype= "solid", size=.75, arrow=arrow(type="open", length=unit(0.10,"cm")))+
  scale_fill_manual(name="", values= c("Analizados nuevos diarios" = "#31859C")) + 
  scale_color_manual(name="", values= c("Tendencia promedio móvil 7 días" = "#31859C")) +
  scale_y_continuous(expand=c(0,0), limits=c(0, max(Sonora.A$Analizados.diarios,na.rm=TRUE)+100)) +
  scale_x_date(expand=c(0,10), date_breaks = "1 month", date_labels = "%B") +
  geom_hline(yintercept=estata.hoy$Analizados.diarios, linetype="dashed", color = "red") +
  geom_point(aes(x= Fecha, y= Analizados.diarios,  fill= "Analizados nuevos diarios"), color = "white", size = 0.9, stroke=0.4, alpha=0.65, shape = 21) +
  geom_text(aes(x = as.Date("2020-11-01"), y = estata.hoy$Analizados.diarios+100,
                label = paste0(estata.hoy$Analizados.diarios, " nuevas personas\nanalizadas hoy")), stat = "unique", family = "Lato Black",
            size = 3, color = "red", hjust=0)+
  # geom_segment(aes(x = as.Date("2021-02-22"), y = 490, xend = as.Date("2021-03-11"), yend = 250),
  #              size = 1.5, color = "black",
  #              arrow = arrow(length = unit(0.02, "npc"))) +
  # geom_text(aes(x = as.Date("2021-02-22"), y = 520,
  #               label = "11/03/2021\n248 resultados"), stat = "unique", family = "Lato Black",
  #           size = 5, color = "black")+
  theme_bw() + temaejes +
  theme(legend.text = element_text(family = "Lato", size = 7), legend.background = element_rect(fill="transparent"),
        legend.position = c(0.02,0.85),  legend.justification="left", legend.margin=margin(t = 0, unit='cm'),
        legend.key = element_rect(fill="transparent", color="transparent")) +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  =paste0("<span style = 'font-size:14pt'>Covid-19 en Sonora:</span><br><span style = 'color:#4BACC6';>", estata.hoy$Analizados," personas analizadas</span>"), 
       subtitle= Fechahoy, caption =fuente)

AnalizadosSon

ggsave("Gráficos diarios/diariopruebas.png",AnalizadosSon, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)




library(directlabels)
Letalidad <- Sonora.DF %>% ggplot(aes(x= Fecha, y= Letalidad)) +
  geom_line(color= "#993366", linetype= "solid", size=1, alpha=0.6)+
  geom_point( data = subset(Sonora.DF , Fecha == max(Fecha)), fill="#993366", size=2 , shape=21, color="white", stroke=1) +
  geom_dl( data = subset(Sonora.DF , Fecha == max(Fecha)), aes(label = Letalidad), color="#993366", method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 1.5, fontfamily= "Lato Black")) +
  scale_y_continuous(expand = c(0, 0), limits= c(0,15), breaks=seq(0,15,2)) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-04-01"), max(Sonora.DF$Fecha)+40), date_breaks = "1 month", date_labels = "%B") +
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_bw() +
  temaejes +
  labs(y = "Decesos por cada 100 casos", 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Sonora:</span><br><span style = 'color:#993366';>Letalidad acumulada</span>", 
       subtitle= Fechahoy, caption =fuente)  

Letalidad

ggsave("Gráficos semanales/s13.png",Letalidad,  width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


#Hospitalizados

Hospitalizados <- Sonora.DF %>% ggplot() +
  geom_point(aes(x= Fecha, y= Hospitalizados.Activos),fill= "#F79646", color="white", size = 0.9, stroke=0.4, alpha=0.65, shape = 21)+
  geom_line(aes(x= Fecha, y= Hospitalizados.Activos.7d), color= "#F79646", linetype= "solid", size=1, alpha=0.8)+
  geom_area(aes(x= Fecha, y= Hospitalizados.Activos.7d),color= "transparent", fill= "#F79646",alpha=0.1)+
  geom_point(aes(x= Fecha, y= Hospitalizados.Activos), data = subset(Sonora.DF , Fecha == max(Fecha)), fill="white", size=1 , shape=21, color="#F79646", stroke=1) +
  geom_dl( data = subset(Sonora.DF , Fecha == max(Fecha)), aes(x= Fecha, y= Hospitalizados.Activos, label = Hospitalizados.Activos), color="#F79646", method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 1.5, fontfamily= "Lato Black")) +
  scale_y_continuous(expand = c(0, 0), limits= c(0,300), breaks=seq(0,300,50)) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-04-01"), max(as.Date(Sonora.DF$Fecha))+50), date_breaks = "1 month", date_labels = "%B") +
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_bw() +
  temaejes +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Sonora:</span><br><span style = 'color:#F79646';>Casos activos hospitalizados al corte</span>", 
       subtitle= Fechahoy, caption =fuente)  

Hospitalizados

ggsave("Gráficos diarios/Hospitalizados 2.png",Hospitalizados,  width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)



Activos <- Sonora.DF %>% ggplot() +
  geom_point(aes(x= Fecha, y= Ambulatorios.Activos), fill= "#58BCBC", color= "white", size = 0.9, stroke=0.4, alpha=0.65, shape = 21)+
  geom_line(aes(x=Fecha, y=Ambulatorios.Activos.7d, color = "Tendencia promedio móvil 7 días"), linetype= "solid", size=.75, arrow=arrow(type="open", length=unit(0.10,"cm")))+
  geom_area(aes(x=Fecha, y=Ambulatorios.Activos.7d),color= "transparent", fill= "#58BCBC",alpha=0.1)+
  geom_point( data = subset(Sonora.DF , Fecha == max(Fecha)),aes(x= Fecha, y= Ambulatorios.Activos), fill="white", size=1 , shape=21, color="#58BCBC", stroke=1) +
  geom_dl( data = subset(Sonora.DF , Fecha == max(Fecha)), aes(x= Fecha, y= Ambulatorios.Activos, label = Ambulatorios.Activos), color="#58BCBC", method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 1.5, fontfamily= "Lato Black")) +
  scale_color_manual(values= c("Tendencia promedio móvil 7 días"= "#58BCBC"))+
  scale_y_continuous(expand = c(0, 0), limits= c(0,3000), breaks=seq(0,3000,500)) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-04-01"), max(as.Date(Sonora.DF$Fecha))+50), date_breaks = "1 month", date_labels = "%B") +
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_bw() +
  temaejes + 
  theme(legend.text = element_text(family = "Lato", size = 7), legend.background = element_rect(fill="transparent"),
        legend.position = c(0.02,0.9),  legend.justification="left", legend.margin=margin(t = 0, unit='cm'),
        legend.key = element_rect(fill="transparent", color="transparent")) +
  labs(y = NULL, 
       x = NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Sonora:</span><br><span style = 'color:#58BCBC';>Pacientes activos con síntomas leves al corte</span>", 
       subtitle= Fechahoy, caption =fuente)  

Activos
ggsave("Gráficos diarios/diariosActivos.png",Activos,  width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


   
                                                                                                         
write.csv(Sonora.DF, "ResultadoCSV/DFSonora.csv")

# O2A <- Sonora.DF %>% filter(Etapa=="Ascenso", OLA=="B") 
# O2A <- O2A %>% mutate(día=rownames(O2A)) %>% select(día, Fecha, Pruebas, Pruebas.diarias,Pruebas.media.7d, Confirmados, Casos.diarios, Casos.media.7d, Decesos, Decesos.diarios, Decesos.media.7d, Hospitalizados) %>% 
#   mutate(Casos.acumulados=cumsum(Casos.diarios))
# 
# O3A <- Sonora.DF %>% filter(Etapa=="Ascenso", OLA=="C") 
# O3A <- O3A %>% mutate(día=rownames(O3A)) %>% select(día, Fecha, Pruebas, Pruebas.diarias,Pruebas.media.7d, Confirmados, Casos.diarios, Casos.media.7d, Decesos, Decesos.diarios, Decesos.media.7d, Hospitalizados) %>% 
# mutate(Casos.acumulados=cumsum(Casos.diarios))
# 
# OLAS <- O2A %>% left_join(O3A, by="día") %>% mutate(día=as.numeric(día))





# OLASG <- ggplot(OLAS) +
#   geom_line(aes(x= día, y= Casos.acumulados.x, color= "OLA 2", group=1), linetype= "solid", size=.75, arrow=arrow(type="open", length=unit(0.10,"cm")))+
#   geom_line(aes(x= día, y= Casos.acumulados.y, color= "OLA 3", group=1), linetype= "solid", size=.75, arrow=arrow(type="open", length=unit(0.10,"cm")))+
#   scale_color_manual(name="", values= c("OLA 2" = "#01787E", "OLA 3" = "#F79646")) +
#   #scale_x_date(expand=c(0,5), date_breaks = "1 month", date_labels = "%B") +
# theme_bw() + temaejes +
#   theme(legend.text = element_text(family = "Lato", size = 7), legend.background = element_rect(fill="transparent"),
#         legend.position = "none",  legend.justification="left", legend.margin=margin(t = 0, unit='cm'),
#         legend.key = element_rect(fill="transparent", color="transparent")) +
#   labs(y = NULL, 
#        x = "Días desde el inicio de la ola", legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Sonora:</span><br>Casos acumulados confirmados en las etapas de<br>ascenso de la <span style = 'color:#01787E';>primera</span> y <span style = 'color:#F79646';>segunda</span> ola</span>", 
#        subtitle= Fechahoy, caption =fuente)
# 
# OLASG
# 
# ggsave("Gráficos diarios/OLASG.png",OLASG, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)
# 

