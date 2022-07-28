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
                      col_types = cols(fecha_reporte = col_date(format = "%Y-%m-%d")))

dia<-max(as.Date(Hermosillo.DF$fecha_reporte))

Fechahoy <- paste0("Al reporte del ", day(dia), " de ", months.Date(dia)," de ", year(dia),".")
fuente <- "Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud del Gobierno de la República\n*Por fecha de reporte | www.luisarmandomoreno.com"
fuenteimp <- "Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud del Gobierno de la República y Secretaría de Salud del estado de Hermosillo.\nwww.luisarmandomoreno.com"
fuenteprueba <- "Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud del Gobierno de la República\n*Incluye resultados válidos de pruebas PCR y antigénicas. | www.luisarmandomoreno.com"
fuenteact <- "Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud del Gobierno de la República\n*Se consideran casos activos aquellos que iniciaron síntomas dentro de los 14 días previos al corte. | www.luisarmandomoreno.com"
subtitulo <- paste0("Casos confirmados en los últimos 7 días por 100 mil habitantes\nAl reporte del ",day(dia),"/",month(dia),"/",year(dia))
subtituloact <- paste0("Casos activos (que iniciaron síntimas dentro de los 14 días previos) por 100 mil habitantes.\nAl reporte del ",day(dia),"/",month(dia),"/",year(dia))

fuentefech <- "Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud del Gobierno de la República\n*El registro de los últimos 14 días aún se están alimentando en el sistema | www.luisarmandomoreno.com"
POBMUN <- read_csv("Bases/POBMUN.csv", col_types = cols(CVEGEO = col_character()), 
                   locale = locale(encoding = "ISO-8859-1"))

temaejes <- theme(plot.margin = margin(10, 25, 10, 25),panel.grid=element_blank(), panel.border=element_blank(), axis.line= element_line(color = "black", size = 0.3),
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
Hermosillo.DF <- read_csv("Bases/ST_HermosilloReporte_SSFED.csv", 
                      col_types = cols(fecha_reporte = col_date(format = "%Y-%m-%d"))) %>% filter(fecha_reporte>as.Date("2020-10-08"))
Hermosillo.DF.hoy <- filter(Hermosillo.DF, fecha_reporte == dia)
Hermosillo.DF.hoy <- select(Hermosillo.DF.hoy, Hospitalizados.Activos, Ambulatorios.Activos, Decesos, Recuperados)
Hermosillo.DF.hoy <- rename(Hermosillo.DF.hoy, "Ambulatorios activos"= Ambulatorios.Activos, "Hospitalizados Activos"=Hospitalizados.Activos)
Hermosillo.DF.hoy <- gather(Hermosillo.DF.hoy, key= Estatus, value= Casos.confirmados) 

#Treemap

Hermosillo.DF.hoy <- filter(Hermosillo.DF, fecha_reporte == max(fecha_reporte))
Hermosillo.DF.hoy <- select(Hermosillo.DF.hoy, Hospitalizados.Activos, Ambulatorios.Activos, Decesos, Recuperados)
Hermosillo.DF.hoy <- rename(Hermosillo.DF.hoy, "Ambulatorios\nactivos"= Ambulatorios.Activos, "Hospitalizados activos"=Hospitalizados.Activos)
Hermosillo.DF.hoy <- gather(Hermosillo.DF.hoy, key= Estatus, value= Casos.confirmados) 
tituestatus <- paste0("<span style = 'font-size:14pt'>Covid-19 en Hermosillo</span><br>Estatus de los <span style = 'color:#01A2AC';>", prettyNum(as.numeric(sum(Hermosillo.DF.hoy$Casos.confirmados)), big.mark=",", preserve.width="none"), "</span> casos confirmados")


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


ggsave("Gráficos diarios/Hermosillo/Diarioestatus.png",Estatus , width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)

# Bases municipales
Casos <- read_csv("Bases/Casosdiarios_SSFED.csv", 
                  col_types = cols(CASOS = col_integer(), 
                                   CVEGEO = col_character(), Fecha = col_date(format = "%Y-%m-%d"), 
                                   MUNICIPIO = col_character(), NUEVOS = col_integer(), X1 = col_skip()), 
                  locale = locale(encoding = "ISO-8859-1")) %>% filter(Fecha>as.Date("2020-10-08")) %>% filter(CVEGEO!=26999)
casosacumdia <- Casos %>% filter(Fecha==max(as.Date(Fecha))) %>% filter(NUEVOS!=0) %>% select (Fecha, MUNICIPIO, CASOS, NUEVOS) %>% arrange(desc(NUEVOS)) %>% 
#write.csv('ResultadoCSV/casoshoy.csv')


Decesos <- read_csv("Bases/Decesosdiarios_SSFED.csv", 
                    col_types = cols(DECESOS = col_integer(), 
                                     CVEGEO = col_character(), Fecha = col_date(format = "%Y-%m-%d"), 
                                     MUNICIPIO = col_character(), NUEVOS = col_integer(), X1 = col_skip()), 
                    locale = locale(encoding = "ISO-8859-1")) %>% filter(Fecha>as.Date("2020-10-08")) %>% filter(CVEGEO!=26999)
decesosacumdia <- Decesos %>% filter(Fecha==max(as.Date(Fecha))) %>% filter(NUEVOS!=0) %>% select (Fecha, MUNICIPIO, DECESOS, NUEVOS) %>% arrange(desc(NUEVOS)) %>% 
  #write.csv('ResultadoCSV/decesoshoy.csv')


#Casos trayectoria Promedio vs Acumulado

# Casosprom <- Casos %>% group_by(MUNICIPIO) %>% mutate(Casos.media.7d=round(rollmeanr(x=NUEVOS, 7, fill = 0),1)) %>%  filter(CASOS>500) 
# 
# Casosd <- ggplot(subset(Casosprom, MUNICIPIO %in% c("Hermosillo", "Cajeme"))) +
#   geom_line(mapping = aes(x = Fecha, y = Casos.media.7d, color= MUNICIPIO), size=0.75, alpha=0.8, arrow=arrow(type="open", length=unit(0.10,"cm"))) +
#   coord_cartesian(expand = TRUE, clip = 'off') +
#   scale_color_manual(values=c("#F79646", "#01A2AC")) + 
#   theme_minimal() + temaejes + theme(
#         legend.text = element_text(family = "Lato", size = 8),
#         legend.position = c(0.85,0.85), legend.justification="left") +
#   labs(y = "Casos diarios\n(promedio móvil 7 días)", 
#        x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo</span><br><span style = 'color:#01A2AC';>Trayectoria de casos en los municipios</span>", 
#        subtitle= Fechahoy, caption =fuente) 
#   #scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10")
# 
# ggsave("Gráficos diarios/Hermosillo/diariocasostray.png",Casosd , width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)
# 
# #Decesos trayectoria Promedio vs Acumulado
# 
# Decesosprom <- Decesos %>% group_by(MUNICIPIO) %>% mutate(Decesos.media.7d=round(rollmeanr(x=NUEVOS, 7, fill = 0),1)) 
# 
# Decesosd <- ggplot(subset(Decesosprom, MUNICIPIO %in% c("Hermosillo", "Cajeme"))) +
#   geom_line(mapping = aes(x = Fecha, y = Decesos.media.7d, color= MUNICIPIO), size=0.75, alpha=0.8, arrow=arrow(type="open", length=unit(0.10,"cm"))) +
#   scale_color_locuszoom() + 
#   coord_cartesian(expand = TRUE, clip = 'off') +
#   theme_minimal() + temaejes + theme(
#     legend.text = element_text(family = "Lato", size = 8),
#     legend.position = "right", legend.justification="left") +
#   labs(y = "Casos diarios\n(promedio móvil 7 días, log)", 
#        x = "Casos acumulados (log)",legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo</span><br><span style = 'color:#993366';>Trayectoria de decesos en los municipios</span>", 
#        subtitle= Fechahoy, caption =fuente) + 
#   scale_y_continuous (expand = c(0, 0)) + 
#   scale_x_continuous (expand = c(0, 0))
# 
# ggsave("Gráficos diarios/Hermosillo/diariodecesostray.png",Decesosd , width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)




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
casossempob <- Casossemana %>% mutate(IS=if_else(INCIDENCIA>=99.9,"4", 
                                                 if_else(INCIDENCIA>49.9,"3",
                                                         if_else(INCIDENCIA>9.9,"2",
                                                                 if_else(INCIDENCIA>0,"1","NA"))))) %>% 
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
  labs(y = NULL, x = NULL, title  = "<span style = 'font-size:12pt'>Covid-19 en Hermosillo</span><br><span style = 'color:#01A2AC';>Incidencia semanal</span>", 
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
ggsave("Gráficos semanales/sincidencia.png",Mapa_incivoid, bg = "transparent", height = 12, width = 12, units = "cm", dpi = 800, type = 'cairo')


Activosmun <- read_csv("Bases/Activosdiarios_SSFED.csv", 
                       col_types = cols(CASOS_ACTIVOS = col_integer(), 
                                        CVEGEO = col_character(), Fecha = col_date(format = "%Y-%m-%d"), 
                                        MUNICIPIO = col_character(), X1 = col_skip()), 
                       locale = locale(encoding = "ISO-8859-1")) %>% filter(CVEGEO!=26999)
Activosmundia <- Activosmun %>% filter(Fecha==max(as.Date(Fecha))) %>% filter(CASOS_ACTIVOS!=0) %>% select (Fecha, MUNICIPIO, CASOS_ACTIVOS) %>% arrange(desc(CASOS_ACTIVOS)) %>% 
  #write.csv('ResultadoCSV/Activosmunhoy.csv')


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
  labs(y = NULL, x = NULL, title  = "<span style = 'font-size:12pt'>Covid-19 en Hermosillo:</span><br><span style = 'color:#01A2AC';>Incidencia de casos activos</span>", 
       subtitle = subtituloact,  fill = NULL, 
       caption =fuente)+
  geom_polygon(data=capa_reg, aes(x=long, y=lat, group=group), 
               fill="transparent", color="black", size=0.2)
Mapa_act
ggsave("Gráficos diarios/Hermosillo/diariomapinci.png",Mapa_act, width = 5/2 * (16/9), height = 5, type = "cairo", dpi = 400)

# Gráfico estatal
Hermosillo.DF <- read_csv("Bases/ST_HermosilloReporte_SSFED.csv", 
                      col_types = cols(fecha_reporte = col_date(format = "%Y-%m-%d")))
Hermosillo.DF <- rename(Hermosillo.DF, Fecha=fecha_reporte)
Hermosillo.DF <- mutate(Hermosillo.DF, Casos.diarios= Casos - lag(Casos, default = Casos[1], order_by=Fecha))
Hermosillo.DF <- mutate(Hermosillo.DF, Confirmados.diarios= Confirmados_lab - lag(Confirmados_lab, default = Confirmados_lab[1], order_by=Fecha))
Hermosillo.DF <- mutate(Hermosillo.DF, Hospitalizados.diarios= Hospitalizados - lag(Hospitalizados, default = Casos[1], order_by=Fecha))
Hermosillo.DF <- mutate(Hermosillo.DF, Decesos.diarios= Decesos - lag(Decesos, default = Decesos[1], order_by=Fecha))
Hermosillo.DF <- mutate(Hermosillo.DF, Casos.media.7d=round(rollmeanr(x=Casos.diarios, 7, fill = NA),1))
Hermosillo.DF <- mutate(Hermosillo.DF, Decesos.media.7d=round(rollmeanr(x=Decesos.diarios,7, fill=NA),1))
Hermosillo.DF <- mutate(Hermosillo.DF, Hospitalizados.media.7d=round(rollmeanr(x=Hospitalizados.diarios, 7, fill = NA),1))
Hermosillo.DF <- mutate(Hermosillo.DF, Resultados.diarios= Resultados_lab - lag(Resultados_lab, default = Resultados_lab [1]))
Hermosillo.DF <- mutate(Hermosillo.DF, Resultados.media.7d=round(rollmeanr(x=Resultados.diarios,7, fill=NA),1))
Hermosillo.DF <- mutate(Hermosillo.DF, Ambulatorios.Activos.7d=round(rollmeanr(x=Ambulatorios.Activos,7, fill=NA),1))
Hermosillo.DF <- mutate(Hermosillo.DF, Sospechosos.7d=round(rollmeanr(x=Sospechosos,7, fill=NA),1))
Hermosillo.DF <- mutate(Hermosillo.DF, Sospechosos.diarios= Sospechosos - lag(Sospechosos, default = Sospechosos[1], order_by=Fecha))
Hermosillo.DF <- mutate(Hermosillo.DF, Sospechosos.diarios.7d=round(rollmeanr(x=Sospechosos.diarios,7, fill=NA),1))
Hermosillo.DF <- mutate(Hermosillo.DF, Hospitalizados.Activos.7d=round(rollmeanr(x=Hospitalizados.Activos,7, fill=NA),1))
Hermosillo.DF <- mutate(Hermosillo.DF, Incidencia= round((Casos / 30.74745),2))
Hermosillo.DF <- mutate(Hermosillo.DF, Letalidad= round((Decesos / Casos)*100,1))
Hermosillo.DF <- mutate(Hermosillo.DF, Mortalidad= round((Decesos / 30.74745)*100,2))
Hermosillo.DF <- mutate(Hermosillo.DF, Positividad.diaria= round((Confirmados.diarios / Resultados.diarios)*100,2))
Hermosillo.DF <- mutate(Hermosillo.DF, Positividad= round((Confirmados_lab / Resultados_lab )*100,2))
Hermosillo.DF <- mutate(Hermosillo.DF, Analizados.diarios= Analizados - lag(Analizados, default = Analizados[1], order_by=Fecha))
Hermosillo.DF <- mutate(Hermosillo.DF, Analizados.media.7d=round(rollmeanr(x=Analizados.diarios, 7, fill = NA),1))

estata.hoy <- Hermosillo.DF %>% filter(Fecha==max(as.Date(Fecha)))
estata.semana <- Hermosillo.DF %>% filter(Fecha==(max(as.Date(Fecha))-7))

write.csv(Hermosillo.DF, "ResultadoCSV/DFHermosillo.csv")

#Casos diarios Estatal

Hermosillo.ED <- read_csv("Bases/ST_HermosilloInformesCOVID_SSEDO.csv", 
                      col_types = cols(fecha_corte = col_date(format = "%d/%m/%Y")))
Hermosillo.ED <- rename(Hermosillo.ED, Fecha_reporte= Fecha)
Hermosillo.ED <- rename(Hermosillo.ED, Fecha=fecha_corte)
Hermosillo.ED <- mutate(Hermosillo.ED, Casos.diarios= Casos - lag(Casos, default = Casos[1], order_by=Fecha))
Hermosillo.ED <- mutate(Hermosillo.ED, Decesos.diarios= Decesos_imp - lag(Decesos_imp, default = Decesos_imp[1], order_by=Fecha))
Hermosillo.ED <- select(Hermosillo.ED, Fecha, Casos.diarios, Decesos.diarios)

CasosPos <- Hermosillo.DF %>% filter(Fecha>as.Date("2021-09-19")) %>% 
  select(Fecha, Casos.diarios, Decesos.diarios) 

Hermosillo.ED <- Hermosillo.ED %>% bind_rows(CasosPos)
Hermosillo.ED <- mutate(Hermosillo.ED, Casos.media.7d=round(rollmeanr(x=Casos.diarios, 7, fill = NA),1),
                    Decesos.media.7d=round(rollmeanr(x=Decesos.diarios, 7, fill = NA),1))
Hermosillo.ED[is.na(Hermosillo.ED)] <- 0

CasosSon <- ggplot(Hermosillo.ED) +
  geom_hline(yintercept=estata.semana$Casos.diarios, linetype="dashed", color = "gray65") +
  geom_text(aes(x = as.Date("2020-03-20"), y = (estata.semana$Casos.diarios + 30),
                label = paste0("+", estata.semana$Casos.diarios, " casos el mismo día de la semana anterior")), stat = "unique", family = "Lato Black",
            size = 2, color = "gray65", hjust=0)+
  geom_hline(yintercept=estata.hoy$Casos.diarios, linetype="dashed", color = "red") +
  geom_area(aes(x= Fecha, y= Casos.media.7d), fill= "#58BCBC", alpha=0.3)+
  geom_line(aes(x= Fecha, y= Casos.media.7d, color= "Tendencia promedio móvil 7 días"), linetype= "solid", size=.7, arrow=arrow(type="open", length=unit(0.10,"cm")))+
  geom_point(aes(x= Fecha, y= Casos.diarios), color = "white", fill= "#01787E", size = 0.8, stroke=0.4, alpha=0.65, shape = 21) +
  scale_color_manual(name="", values= c("Tendencia promedio móvil 7 días" = "#01787E")) +
  scale_y_continuous(expand = c(0, 15), limits=c(0, 1600)) +
  scale_x_date(expand=c(0,5), date_breaks = "1 month", labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                                                                    paste(month(x, label = TRUE), "\n", year(x)), 
                                                                                    paste(month(x, label = TRUE)))) +
  geom_text(aes(x = as.Date("2020-03-20"), y = (estata.hoy$Casos.diarios + 30),
                   label = paste0("+", estata.hoy$Casos.diarios, " casos hoy")), stat = "unique", family = "Lato Black",
               size = 3, color = "red",  hjust=0)+
  theme_bw() + temaejes +
  theme(legend.text = element_text(family = "Lato", size = 7), legend.background = element_rect(fill="transparent"),
        legend.position = c(0.02,0.95),  legend.justification="left", legend.margin=margin(t = 0, unit='cm'), legend.box = "horizontal",
        legend.key = element_rect(fill="transparent", color="transparent")) +
  labs(y = NULL,
       x = NULL,legend= NULL, title  = paste0("<span style = 'font-size:14pt'>Covid-19 en Hermosillo</span><br><span style = 'color:#01A2AC';>Casos confirmados acumulados: ",prettyNum(as.numeric(sum(Hermosillo.DF.hoy$Casos.confirmados)), big.mark=",", preserve.width="none"),"</span>"),
       subtitle= paste0("Variación en el acumulado de casos confirmados por fecha de reporte respecto al día anterior.\n", Fechahoy), caption =fuenteimp)

CasosSon

ggsave("Gráficos diarios/Hermosillo/diariocasosreporte.png",CasosSon, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


dias <- c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo")
Hermosillo.semana <- Hermosillo.ED %>% mutate(semana=isoweek(Fecha), diasemana=weekdays(Fecha)) %>% mutate(diasemana=factor(diasemana, dias)) %>% filter(Fecha>as.Date("2020-03-16"))
Hermosillo.semana %>% ggplot()+
  geom_line(aes(x=Fecha, y=Casos.diarios), color="#01A2AC") +
  scale_y_continuous(expand = c(0,0), limits = c(0, max(Hermosillo.semana$Casos.diarios))) +
  scale_x_date(expand=c(0,5),
               date_breaks = "3 month", labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                                                     paste(month(x, label = TRUE), "\n", year(x)),
                                                                     paste(month(x, label = TRUE)))) +
  facet_wrap(~diasemana)+
  theme_bw() + temaejes +
  theme(legend.text = element_text(family = "Lato", size = 7), legend.background = element_rect(fill="transparent"),
        legend.position = "none",  legend.justification="left", legend.margin=margin(t = 0, unit='cm'), axis.text = element_text(size=6), strip.text = element_text(size=9),
        panel.grid.minor = element_blank(),
        legend.key = element_rect(fill="transparent", color="transparent"), strip.background = element_blank()) +
  labs(y = NULL,
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo</span><br><span style = 'color:#01A2AC';>Casos confirmados por día de la semana</span>",
       subtitle= paste0(Fechahoy), caption =fuenteimp)
ggsave("Gráficos diarios/Hermosillo/diariocasossemanarep.png", width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)

HospSon <- Hermosillo.DF %>% filter(Fecha>=as.Date("2021-07-01")) %>% ggplot() +
  geom_hline(yintercept=estata.semana$Hospitalizados.diarios, linetype="dashed", color = "gray65") +
  geom_text(aes(x = as.Date("2021-07-05"), y = (estata.semana$Hospitalizados.diarios + 1.8),
                label = paste0("+", estata.semana$Hospitalizados.diarios, " casos hospitalizados mismo día de la semana anterior")), stat = "unique", family = "Lato Black",
            size = 2, color = "gray65", hjust=0)+
  geom_hline(yintercept=estata.hoy$Hospitalizados.diarios, linetype="dashed", color = "red") +
  geom_area(aes(x= Fecha, y= Hospitalizados.media.7d), fill= "#F79646", alpha=0.1)+
  geom_line(aes(x= Fecha, y= Hospitalizados.media.7d, color= "Tendencia promedio móvil 7 días"), linetype= "solid", size=.7, arrow=arrow(type="open", length=unit(0.10,"cm")))+
  geom_point(aes(x= Fecha, y= Hospitalizados.diarios), color = "white", fill= "#F79646", size = 0.8, stroke=0.4, alpha=0.65, shape = 21) +
  scale_color_manual(name="", values= c("Tendencia promedio móvil 7 días" = "#F79646")) +
  scale_y_continuous(expand = c(0, 1), limits=c(0, 100)) +
  scale_x_date(expand=c(0,2), date_breaks = "1 month", labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                                                                    paste(month(x, label = TRUE), "\n", year(x)), 
                                                                                    paste(month(x, label = TRUE)))) +
  geom_text(aes(x = as.Date("2021-07-05"), y = (estata.hoy$Hospitalizados.diarios + 2),
                label = paste0("+", estata.hoy$Hospitalizados.diarios, " casos hospitalizados hoy")), stat = "unique", family = "Lato Black",
            size = 3, color = "red",  hjust=0)+
  theme_bw() + temaejes +
  theme(legend.text = element_text(family = "Lato", size = 7), legend.background = element_rect(fill="transparent"),
        legend.position = c(0.02,0.95),  legend.justification="left", legend.margin=margin(t = 0, unit='cm'), legend.box = "horizontal",
        legend.key = element_rect(fill="transparent", color="transparent")) +
  labs(y = NULL,
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo</span><br><span style = 'color:#F79646';>Casos confirmados hospitalizados por reporte</span>",
       subtitle= paste0("Variación en el acumulado de casos confirmados hospitalizados respecto al día anterior.\n", Fechahoy), caption =fuenteimp)

HospSon

ggsave("Gráficos diarios/Hermosillo/Hospitalizados3.png",HospSon, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


  DecesosSon <- ggplot(Hermosillo.ED) +
  geom_hline(yintercept=estata.semana$Decesos.diarios, linetype="dashed", color = "gray65") +
  geom_text(aes(x = as.Date("2020-03-20"), y = (estata.semana$Decesos.diarios + 3),
                  label = paste0("+", estata.semana$Decesos.diarios, " decesos el mismo día de la semana anterior")), stat = "unique", family = "Lato Black",
              size = 2, color = "gray65", hjust=0)+
  geom_hline(yintercept=estata.hoy$Decesos.diarios, linetype="dashed", color = "red") +
  geom_area(aes(x= Fecha, y= Decesos.media.7d), fill= "#D075A3", alpha=0.3)+
  geom_line(aes(x= Fecha, y= Decesos.media.7d, color= "Tendencia promedio móvil 7 días"), linetype= "solid", size=.75, arrow=arrow(type="open", length=unit(0.10,"cm")))+
  geom_point(aes(x= Fecha, y= Decesos.diarios), color = "white", fill= "#73264D", size = 0.9, stroke=0.4, alpha=0.65, shape = 21) +
  scale_fill_manual(name="", values= c("Decesos diarios" = "#73264D", "Tendencia promedio móvil 7 días" = "#D075A3")) +
  scale_color_manual(name="", values= c("Tendencia promedio móvil 7 días" = "#73264D")) +
  scale_y_continuous(expand = c(0, 0), limits= c(0,160)) +
  scale_x_date(expand=c(0,10), date_breaks = "1 month", labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                                                                       paste(month(x, label = TRUE), "\n", year(x)), 
                                                                                       paste(month(x, label = TRUE)))) +
  geom_text(aes(x = as.Date("2020-03-20"), y = estata.hoy$Decesos.diarios + 3,
                label = paste0("+", estata.hoy$Decesos.diarios, " decesos hoy")), stat = "unique", family = "Lato Black",
            size = 3, color = "red", hjust=0)+
  theme_bw() + temaejes +
  theme(legend.text = element_text(family = "Lato", size = 7), legend.background = element_rect(fill="transparent"),
        legend.position = c(0.02,0.95),  legend.justification="left", legend.margin=margin(t = 0, unit='cm'),
        legend.key = element_rect(fill="transparent", color="transparent")) +
  labs(y = NULL,
       x = NULL,legend= NULL, title  = paste0("<span style = 'font-size:14pt'>Covid-19 en Hermosillo</span><br><span style = 'color:#993366';>Decesos confirmados acumulados: ",prettyNum(as.numeric(estata.hoy$Decesos), big.mark=",", preserve.width="none"),"</span>"),
       subtitle= paste0("Variación en el acumulado de decesos confirmados por fecha de reporte respecto al día anterior.\n", Fechahoy), caption =fuenteimp)

DecesosSon

ggsave("Gráficos diarios/Hermosillo/diariodecesosrep.png",DecesosSon,  width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)
Hermosillo.DF <-  filter(Hermosillo.DF, Fecha>as.Date("2020-05-01"))
PruebasSon <- ggplot(Hermosillo.DF) +
  geom_area(aes(x= Fecha, y= Resultados.media.7d), fill= "#4BACC6", alpha=0.3)+
  geom_line(aes(x= Fecha, y= Resultados.media.7d, color= "Tendencia promedio móvil 7 días"), linetype= "solid", size=.7, arrow=arrow(type="open", length=unit(0.10,"cm")))+
  scale_fill_manual(name="", values= c("Resultados diarios" = "#31859C")) + 
  scale_color_manual(name="", values= c("Tendencia promedio móvil 7 días" = "#31859C")) +
  scale_y_continuous(expand=c(0,0), limits=c(0, max(Hermosillo.DF$Resultados.diarios,na.rm=TRUE)+500)) +
  scale_x_date(expand=c(0,10), date_breaks = "1 month", labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                                                                     paste(month(x, label = TRUE), "\n", year(x)), 
                                                                                     paste(month(x, label = TRUE)))) +
  geom_hline(yintercept=estata.hoy$Resultados.diarios, linetype="dashed", color = "red") +
  geom_point(aes(x= Fecha, y= Resultados.diarios,  fill= "Resultados diarios"), color = "white", size = 0.8, stroke=0.4, alpha=0.65, shape = 21) +
  geom_text(aes(x = as.Date("2020-05-12"), y = estata.hoy$Resultados.diarios+150,
               label = paste0("+", estata.hoy$Resultados.diarios, " resultados válidos hoy\n", round(estata.hoy$Positividad.diaria,0), "% positivos")), stat = "unique", family = "Lato Black",
          size = 3, color = "red", hjust=0)+
  # geom_segment(aes(x = as.Date("2021-02-22"), y = 490, xend = as.Date("2021-03-11"), yend = 250),
  #              size = 1.5, color = "black",
  #              arrow = arrow(length = unit(0.02, "npc"))) +
  # geom_text(aes(x = as.Date("2021-02-22"), y = 520,
  #               label = "11/03/2021\n248 resultados"), stat = "unique", family = "Lato Black",
  #           size = 5, color = "black")+
  theme_bw() + temaejes +
  theme(legend.text = element_text(family = "Lato", size = 7), legend.background = element_rect(fill="transparent"),
        legend.position = c(0.02,0.95),  legend.justification="left", legend.margin=margin(t = 0, unit='cm'), legend.box = "horizontal",
        legend.key = element_rect(fill="transparent", color="transparent")) +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo</span><br><span style = 'color:#4BACC6';>Resultados válidos informados diariamente</span>", 
       subtitle=Fechahoy, caption =fuenteprueba)

PruebasSon

ggsave("Gráficos diarios/Hermosillo/diariopruebas.png",PruebasSon, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


Hermosillo.A <- filter(Hermosillo.DF, Fecha>=as.Date("2020-11-01"))
AnalizadosSon <- ggplot(Hermosillo.A) +
  geom_area(aes(x= Fecha, y= Analizados.media.7d), fill= "#4BACC6", alpha=0.3)+
  geom_line(aes(x= Fecha, y= Analizados.media.7d, color= "Tendencia promedio móvil 7 días"), linetype= "solid", size=.7, arrow=arrow(type="open", length=unit(0.10,"cm")))+
  scale_fill_manual(name="", values= c("Analizados nuevos diarios" = "#31859C")) + 
  scale_color_manual(name="", values= c("Tendencia promedio móvil 7 días" = "#31859C")) +
  scale_y_continuous(expand=c(0,0), limits=c(0, max(Hermosillo.A$Analizados.diarios,na.rm=TRUE)+300)) +
  scale_x_date(expand=c(0,10), date_breaks = "1 month", date_labels = "%B") +
  geom_hline(yintercept=estata.hoy$Analizados.diarios, linetype="dashed", color = "red") +
  geom_point(aes(x= Fecha, y= Analizados.diarios,  fill= "Analizados nuevos diarios"), color = "white", size = 0.8, stroke=0.4, alpha=0.65, shape = 21) +
  geom_text(aes(x = as.Date("2020-11-01"), y = estata.hoy$Analizados.diarios+50,
                label = paste0("+", estata.hoy$Analizados.diarios, " nuevos registros hoy")), stat = "unique", family = "Lato Black",
            size = 3, color = "red", hjust=0)+
  # geom_segment(aes(x = as.Date("2021-02-22"), y = 490, xend = as.Date("2021-03-11"), yend = 250),
  #              size = 1.5, color = "black",
  #              arrow = arrow(length = unit(0.02, "npc"))) +
  # geom_text(aes(x = as.Date("2021-02-22"), y = 520,
  #               label = "11/03/2021\n248 resultados"), stat = "unique", family = "Lato Black",
  #           size = 5, color = "black")+
  theme_bw() + temaejes +
  theme(legend.text = element_text(family = "Lato", size = 7), legend.background = element_rect(fill="transparent"), legend.box = "horizontal",
        legend.position = c(0.02,0.95),  legend.justification="left", legend.margin=margin(t = 0, unit='cm'),
        legend.key = element_rect(fill="transparent", color="transparent")) +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  =paste0("<span style = 'font-size:14pt'>Covid-19 en Hermosillo:</span><br><span style = 'color:#4BACC6';>Personas analizadas: ", prettyNum(as.numeric(estata.hoy$Analizados), big.mark=",", preserve.width="none"),"</span>"), 
       subtitle= Fechahoy, caption =fuente)

AnalizadosSon

ggsave("Gráficos diarios/Hermosillo/diarioAnalizados.png",AnalizadosSon, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)



library(directlabels)
Letalidad <- Hermosillo.DF %>% ggplot(aes(x= Fecha, y= Letalidad)) +
  geom_line(color= "#993366", linetype= "solid", size=1, alpha=0.6)+
  geom_point( data = subset(Hermosillo.DF , Fecha == max(Fecha)), fill="#993366", size=2 , shape=21, color="white", stroke=1) +
  geom_dl( data = subset(Hermosillo.DF , Fecha == max(Fecha)), aes(label = Letalidad), color="#993366", method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 1.5, fontfamily= "Lato Black")) +
  scale_y_continuous(expand = c(0, 0), limits= c(0,15), breaks=seq(0,15,2)) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-04-01"), max(Hermosillo.DF$Fecha)+40), date_breaks = "1 month", 
               labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                            paste(month(x, label = TRUE), "\n", year(x)), 
                                            paste(month(x, label = TRUE)))) +
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_bw() +
  temaejes +
  labs(y = "Decesos por cada 100 casos", 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo</span><br><span style = 'color:#993366';>Letalidad acumulada</span>", 
       subtitle= Fechahoy, caption =fuente)  

Letalidad

ggsave("Gráficos semanales/s13.png",Letalidad,  width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


#Hospitalizados

Hospitalizados <- Hermosillo.DF %>% ggplot() +
  geom_point(aes(x= Fecha, y= Hospitalizados.Activos),fill= "#F79646", color="white", size = 0.9, stroke=0.4, alpha=0.65, shape = 21)+
  geom_line(aes(x= Fecha, y= Hospitalizados.Activos.7d), color= "#F79646", linetype= "solid", size=1, alpha=0.8,arrow=arrow(type="open", length=unit(0.10,"cm")))+
  geom_area(aes(x= Fecha, y= Hospitalizados.Activos.7d),color= "transparent", fill= "#F79646",alpha=0.1)+
  geom_point(aes(x= Fecha, y= Hospitalizados.Activos), data = subset(Hermosillo.DF , Fecha == max(Fecha)), fill="white", size=1 , shape=21, color="#F79646", stroke=1) +
  geom_dl( data = subset(Hermosillo.DF , Fecha == max(Fecha)), aes(x= Fecha, y= Hospitalizados.Activos, label = Hospitalizados.Activos), color="#F79646", method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 1.5, fontfamily= "Lato Black")) +
  scale_y_continuous(expand = c(0, 0), limits= c(0,250), breaks=seq(0,250,50)) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-05-01"), max(Hermosillo.DF$Fecha)+50), date_breaks = "1 month", 
               labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                            paste(month(x, label = TRUE), "\n", year(x)), 
                                            paste(month(x, label = TRUE)))) +
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_bw() +
  temaejes +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo:</span><br><span style = 'color:#F79646';>Casos activos hospitalizados al corte</span>", 
       subtitle= Fechahoy, caption =fuenteact)  

Hospitalizados

ggsave("Gráficos diarios/Hermosillo/Hospitalizados 2.png",Hospitalizados,  width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


Activos <- Hermosillo.DF %>% ggplot() +
  geom_point(aes(x= Fecha, y= Ambulatorios.Activos), fill= "#58BCBC", color= "white", size = 0.9, stroke=0.4, alpha=0.65, shape = 21)+
  geom_line(aes(x=Fecha, y=Ambulatorios.Activos.7d, color = "Tendencia promedio móvil 7 días"), linetype= "solid", size=.75, arrow=arrow(type="open", length=unit(0.10,"cm")))+
  geom_area(aes(x=Fecha, y=Ambulatorios.Activos.7d),color= "transparent", fill= "#58BCBC",alpha=0.1)+
  geom_point( data = subset(Hermosillo.DF , Fecha == max(Fecha)),aes(x= Fecha, y= Ambulatorios.Activos), fill="white", size=1 , shape=21, color="#58BCBC", stroke=1) +
  geom_dl( data = subset(Hermosillo.DF , Fecha == max(Fecha)), aes(x= Fecha, y= Ambulatorios.Activos, label = Ambulatorios.Activos), color="#58BCBC", method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 1.5, fontfamily= "Lato Black")) +
  scale_color_manual(values= c("Tendencia promedio móvil 7 días"= "#58BCBC"))+
  scale_y_continuous(expand = c(0, 0), limits= c(0,max(Hermosillo.DF$Ambulatorios.Activos)+500), breaks=seq(0,max(Hermosillo.DF$Ambulatorios.Activos)+500,500)) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-05-01"), max(Hermosillo.DF$Fecha)+60), date_breaks = "1 month", 
                labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                             paste(month(x, label = TRUE), "\n", year(x)), 
                                             paste(month(x, label = TRUE)))) +
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_bw() +
  temaejes + 
  theme(legend.text = element_text(family = "Lato", size = 7), legend.background = element_rect(fill="transparent"),
        legend.position = c(0.02,0.95),  legend.justification="left", legend.margin=margin(t = 0, unit='cm'),
        legend.key = element_rect(fill="transparent", color="transparent")) +
  labs(y = NULL, 
       x = NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo</span><br><span style = 'color:#58BCBC';>Pacientes activos con síntomas leves al corte</span>", 
       subtitle= Fechahoy, caption =fuenteact)  

Activos
ggsave("Gráficos diarios/Hermosillo/diariosActivos.png",Activos,  width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)

Sospechosos <- Hermosillo.DF %>% ggplot() +
  geom_point(aes(x= Fecha, y= Sospechosos), fill= "#A96AC2", color= "white", size = 0.9, stroke=0.4, alpha=0.65, shape = 21)+
  geom_line(aes(x=Fecha, y=Sospechosos.7d, color = "Tendencia promedio móvil 7 días"), linetype= "solid", size=.75, arrow=arrow(type="open", length=unit(0.10,"cm")))+
  geom_area(aes(x=Fecha, y=Sospechosos.7d),color= "transparent", fill= "#A96AC2",alpha=0.1)+
  geom_point( data = subset(Hermosillo.DF , Fecha == max(Fecha)),aes(x= Fecha, y= Sospechosos), fill="white", size=1 , shape=21, color="#A96AC2", stroke=1) +
  geom_dl( data = subset(Hermosillo.DF , Fecha == max(Fecha)), aes(x= Fecha, y= Sospechosos, label = Sospechosos), color="#A96AC2", method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 1.5, fontfamily= "Lato Black")) +
  scale_color_manual(values= c("Tendencia promedio móvil 7 días"= "#A96AC2"))+
  scale_y_continuous(expand = c(0, 0), limits= c(0,7500), breaks=seq(0,7500,500)) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-05-01"), max(as.Date(Hermosillo.DF$Fecha))+60), date_breaks = "1 month", 
               labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                            paste(month(x, label = TRUE), "\n", year(x)), 
                                            paste(month(x, label = TRUE)))) +
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_bw() +
  temaejes + 
  theme(legend.text = element_text(family = "Lato", size = 7), legend.background = element_rect(fill="transparent"),
        legend.position = c(0.02,0.9),  legend.justification="left", legend.margin=margin(t = 0, unit='cm'),
        legend.key = element_rect(fill="transparent", color="transparent")) +
  labs(y = NULL, 
       x = NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo</span><br><span style = 'color:#A96AC2';>Pacientes con sospecha al corte</span>", 
       subtitle= Fechahoy, caption =fuente)  

Sospechosos
ggsave("Gráficos diarios/Hermosillo/diariosSospechosos.png",Sospechosos,  width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)

SospechososD <- Hermosillo.DF %>% ggplot() +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_point(aes(x= Fecha, y= Sospechosos.diarios), fill= "#A96AC2", color= "white", size = 0.9, stroke=0.4, alpha=0.65, shape = 21)+
  geom_line(aes(x=Fecha, y=Sospechosos.diarios.7d, color = "Tendencia promedio móvil 7 días"), linetype= "solid", size=.75, arrow=arrow(type="open", length=unit(0.10,"cm")))+
  #geom_area(aes(x=Fecha, y=Sospechosos.7d),color= "transparent", fill= "#A96AC2",alpha=0.1)+
  geom_point( data = subset(Hermosillo.DF , Fecha == max(Fecha)),aes(x= Fecha, y= Sospechosos.diarios), fill="white", size=1 , shape=21, color="#A96AC2", stroke=1) +
  geom_dl( data = subset(Hermosillo.DF , Fecha == max(Fecha)), aes(x= Fecha, y= Sospechosos.diarios, label = Sospechosos.diarios), color="#A96AC2", method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 1.5, fontfamily= "Lato Black")) +
  scale_color_manual(values= c("Tendencia promedio móvil 7 días"= "#A96AC2"))+
  scale_y_continuous(expand = c(0, 0), limits= c(-200,200), breaks=seq(-200,200,50)) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2021-09-01"), max(as.Date(Hermosillo.DF$Fecha))+10), date_breaks = "1 month", date_labels = "%B") +
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_bw() +
  temaejes + 
  theme(legend.text = element_text(family = "Lato", size = 7), legend.background = element_rect(fill="transparent"),
        legend.position = c(0.02,0.9),  legend.justification="left", legend.margin=margin(t = 0, unit='cm'),
        legend.key = element_rect(fill="transparent", color="transparent")) +
  labs(y = NULL, 
       x = NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo</span><br><span style = 'color:#A96AC2';>Variación de pacientes sospechosos al corte</span>", 
       subtitle= Fechahoy, caption =fuente)  

SospechososD
ggsave("Gráficos diarios/Hermosillo/diariosvarSospechosos.png",SospechososD,  width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)

   
                                                                                                         
#write.csv(Hermosillo.DF, "ResultadoCSV/DFHermosillo.csv")


Ocupacion <- read_csv("http://archivos.serendipiadata.com/ocupacion/concentrado/ocupacion_hospitalaria_Entidad_sin_ceros.csv",
                      col_types = cols(FECHA = col_date(format = "%Y-%m-%d")),
                      locale = locale())
Ocupacion <- Ocupacion %>% filter(Estado=="Hermosillo") %>% rename(Fecha=FECHA,Ocupacion= "% Ocupación") %>% filter(Tipo=="General") %>% mutate(Ocupacion=round(Ocupacion/100,3))

Ocupacion <- mutate(Ocupacion, General.media.7d=round(rollmeanr(x=Ocupacion,7, fill=NA),3))
diaocup <- max(Ocupacion$Fecha)
Fechahoyocup <- paste0("Al reporte del ", day(diaocup), " de ", months.Date(diaocup)," de ", year(diaocup),".")
OcupH <- Ocupacion %>% ggplot() +
  geom_point(aes(x= Fecha, y=Ocupacion),fill= "#F79646", color="white", size = 0.9, stroke=0.4, alpha=0.65, shape = 21)+
  geom_line(aes(x= Fecha, y= General.media.7d), color= "#F79646", linetype= "solid", size=1, alpha=0.8,arrow=arrow(type="open", length=unit(0.10,"cm")))+
  geom_area(aes(x= Fecha, y= General.media.7d),color= "transparent", fill= "#F79646",alpha=0.1)+
  geom_point(aes(x= Fecha, y= Ocupacion), data = subset(Ocupacion , Fecha == max(Fecha)), fill="white", size=1 , shape=21, color="#F79646", stroke=1) +
  geom_dl( data = subset(Ocupacion , Fecha == max(Fecha)), aes(x= Fecha, y= Ocupacion, label = paste0(Ocupacion*100, "%")), color="#F79646", method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 1.3, fontfamily= "Lato Black")) +
  scale_y_continuous(expand = c(0, 0), limits= c(0,.70), breaks=seq(0,.70,.1), labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-05-01"), max(Hermosillo.DF$Fecha)+70), date_breaks = "1 month",
               labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x),
                                            paste(month(x, label = TRUE), "\n", year(x)),
                                            paste(month(x, label = TRUE)))) +
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_bw() +
  temaejes +
  labs(y = NULL,
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo:</span><br><span style = 'color:#F79646';>Ocupación hospitalaria general al corte</span>",
       subtitle= Fechahoyocup, caption =fuente)

OcupH
"04-11-2021 32.05"

ggsave("Gráficos diarios/Hermosillo/Hospitalizados 1.png",OcupH,  width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)




Hermosillo.Sintomas <- read_csv("Bases/ST_HermosilloSintomas_SSFED.csv", 
                      col_types = cols(fecha_sintomas = col_date(format = "%Y-%m-%d")))

Hermosillo.Sintomas <- rename(Hermosillo.Sintomas, Fecha=fecha_sintomas)
Hermosillo.Sintomas <- mutate(Hermosillo.Sintomas, Casos.media.7d=round(rollmeanr(x=Casos, 7, fill = NA),1))
Hermosillo.Sintomas <- mutate(Hermosillo.Sintomas, Sospechosos.7d=round(rollmeanr(x=Sospechosos, 7, fill = NA),1))
Hermosillo.Sintomas <- mutate(Hermosillo.Sintomas, Sospechosos.Act=round(rollsum(Sospechosos, 13, align="right", fill = 0)))
Hermosillo.Sintomas <- filter(Hermosillo.Sintomas, Fecha>as.Date("2020-03-03"))

Dias <- Hermosillo.Sintomas %>% select(Fecha, Analizados)

sintomas.hoy <- Hermosillo.Sintomas %>% filter(Fecha==max(as.Date(Fecha)))

# estata.sint.hoy <- Hermosillo.DF %>% filter(Fecha==max(as.Date(Fecha)))
dia.act <- max(as.Date(Hermosillo.DF$Fecha))-14


# Casos diarios Estatal
CasosSon <- ggplot(Hermosillo.Sintomas) +
  geom_area(data=subset(Hermosillo.Sintomas, Fecha <=dia.act), aes( x= Fecha, y= Casos.media.7d), fill= "#58BCBC", alpha=0.3)+
  # geom_hline(yintercept=estata.semana$Casos.diarios, linetype="dashed", color = "gray45") +
  # geom_hline(yintercept=estata.hoy$Casos.diarios, linetype="dashed", color = "red") +
  geom_line(data=subset(Hermosillo.Sintomas, Fecha <=dia.act), aes(x= Fecha, y= Casos.media.7d, color= "Tendencia promedio móvil 7 días por fecha de inicio de síntomas"), lineend="round", linejoin="round",linetype= "solid", size=.7, arrow=arrow(type="open", length=unit(0.10,"cm")))+
  #geom_point(data=subset(Hermosillo.Sintomas, Fecha <=dia.act), aes(x= Fecha, y= Casos,fill="Casos que iniciaron síntomas el día correspondiente"), color = "white", size = 0.7, stroke=0.2,shape = 21) +
  #geom_point(data=subset(Hermosillo.Sintomas, Fecha >dia.act), aes(x= Fecha, y= Casos), fill= alpha("#01787E", 0.45), color = "white", size = 0.7, stroke=0.2, alpha=0.45, shape = 21) +
  scale_fill_manual(name="", values= c("Casos que iniciaron síntomas el día correspondiente" = alpha("#01787E", 0.65))) +
  # geom_text(aes(x = as.Date("2020-03-08"), y = (estata.semana$Casos.diarios + 30),
  #               label = paste0("+", estata.semana$Casos.diarios, " casos el mismo día semana anterior")), stat = "unique", family = "Lato Black",
  #           size = 2, color = "gray45", hjust=0)+
  # geom_text(aes(x = as.Date("2020-03-08"), y = (estata.hoy$Casos.diarios + 35),
  #               label = paste0("+", estata.hoy$Casos.diarios, " casos hoy")), stat = "unique", family = "Lato Black",
  #           size = 3, color = "red",  hjust=0)+
  #scale_color_manual(name="", values= c("Tendencia promedio móvil 7 días por fecha de reporte" = alpha("red",0.25),"Tendencia promedio móvil 7 días por fecha de inicio de síntomas" = "#01787E")) +
  scale_color_manual(name="", values= c("Tendencia promedio móvil 7 días por fecha de inicio de síntomas" = "#01787E")) +
  scale_y_continuous(expand = c(0, 0), limits=c(0,500)) +
  scale_x_date(expand=c(0,0),limits= c(min(Hermosillo.Sintomas$Fecha)-5,max(Hermosillo.Sintomas$Fecha)+10), date_breaks = "1 month", 
               labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                            paste(month(x, label = TRUE), "\n", year(x)), 
                                            paste(month(x, label = TRUE)))) +
  guides(fill = guide_legend(order = 1), 
         color = guide_legend(order = 2)) +
  theme_bw() + temaejes +
  theme(legend.text = element_text(family = "Lato", size = 8), legend.background = element_rect(fill="transparent"), legend.box = "horizontal",
        legend.position = c(0.02,0.95),  legend.justification="left", legend.margin=margin(t = 0, unit='cm'),
        legend.key = element_rect(fill="transparent", color="transparent")) +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = paste0("<span style = 'font-size:14pt'>Covid-19 en Hermosillo</span><br><span style = 'color:#01A2AC';>Casos confirmados acumulados: ",prettyNum(as.numeric(sum(Hermosillo.DF.hoy$Casos.confirmados)), big.mark=",", preserve.width="none"),"</span>"), 
       subtitle= Fechahoy, caption =fuentefech)

CasosSon

ggsave("Gráficos diarios/Hermosillo/diariocasos.png",CasosSon, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)

SospechososSINT <- Hermosillo.Sintomas %>% ggplot() +
  geom_text(aes(x = as.Date("2020-06-03"), y =72,
                label = paste0("Variación diaria en acumulado: ",estata.hoy$Sospechosos.diarios)), stat = "unique", family = "Lato Black",
            size = 3, color = "red",  hjust=0)+
  geom_point(aes(x= Fecha, y= Sospechosos, fill= "Casos sospechosos que iniciaron síntomas el día correspondiente"), color= "white", size = 0.9, stroke=0.4, alpha=0.65, shape = 21)+
  geom_line(aes(x=Fecha, y=Sospechosos.7d, color = "Tendencia promedio móvil 7 días"), linetype= "solid", size=.75, arrow=arrow(type="open", length=unit(0.10,"cm")))+
  geom_area(aes(x=Fecha, y=Sospechosos.7d),color= "transparent", fill= "#A96AC2",alpha=0.25)+
  #geom_point( data = subset(Hermosillo.Sintomas , Fecha == max(Fecha)),aes(x= Fecha, y= Sospechosos), fill="white", size=1 , shape=21, color="#A96AC2", stroke=1) +
  #geom_dl( data = subset(Hermosillo.Sintomas , Fecha == max(Fecha)), aes(x= Fecha, y= Sospechosos, label = Sospechosos), color="#A96AC2", method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 1.5, fontfamily= "Lato Black")) +
  scale_fill_manual(name="", values= c( "Casos sospechosos que iniciaron síntomas el día correspondiente" = "#A96AC2")) +
  scale_color_manual(values= c("Tendencia promedio móvil 7 días"= "#A96AC2"))+
  scale_y_continuous(expand = c(0, 0), limits= c(0,80), breaks=seq(0,80,10)) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-05-01"), max(as.Date(Hermosillo.DF$Fecha))+10), date_breaks = "1 month", 
               labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                            paste(month(x, label = TRUE), "\n", year(x)), 
                                            paste(month(x, label = TRUE)))) +
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_bw() +
  temaejes + 
  theme(legend.text = element_text(family = "Lato", size = 7), legend.background = element_rect(fill="transparent"), legend.box = "horizontal",
        legend.position = c(0.02,0.95),  legend.justification="left", legend.margin=margin(t = 0, unit='cm'),
        legend.key = element_rect(fill="transparent", color="transparent")) +
  guides(fill = guide_legend(order = 1), 
         color = guide_legend(order = 2)) +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = paste0("<span style = 'font-size:14pt'>Covid-19 en Hermosillo</span><br><span style = 'color:#A96AC2';>Casos sospechosos al corte: ",prettyNum(as.numeric(estata.hoy$Sospechosos), big.mark=",", preserve.width="none"),"</span>"), 
       subtitle= Fechahoy, caption =fuentefech)

SospechososSINT
ggsave("Gráficos diarios/Hermosillo/diariosSospechososSINT.png",SospechososSINT,  width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


Hermosillo.Defuncion <- read_csv("Bases/ST_HermosilloDefuncion_SSFED.csv", 
                             col_types = cols(fecha_def = col_date(format = "%Y-%m-%d")))

Hermosillo.Defuncion <- rename(Hermosillo.Defuncion , Fecha=fecha_def)
Hermosillo.Defuncion <- Dias %>% left_join(Hermosillo.Defuncion, by="Fecha, Analizados")
Hermosillo.Defuncion [is.na(Hermosillo.Defuncion )] <- 0
Hermosillo.Defuncion  <- mutate(Hermosillo.Defuncion , Decesos.media.7d=round(rollmeanr(x=Decesos,7, fill=NA),1))
Hermosillo.Defuncion <- filter(Hermosillo.Defuncion, Fecha>as.Date("2020-03-01"))
Hermosillo.Defuncion [is.na(Hermosillo.Defuncion )] <- 0

DecesosSon <- ggplot(Hermosillo.Defuncion) +
  #geom_line(data=Hermosillo.ED, aes(x= Fecha, y= Decesos.media.7d, color= "Tendencia promedio móvil 7 días por fecha de reporte"),lineend="round", linejoin="round", linetype= "solid", size=.5, arrow=arrow(type="open", length=unit(0.10,"cm")))+
  geom_area(data=subset(Hermosillo.Defuncion, Fecha <=dia.act), aes( x= Fecha, y= Decesos.media.7d), fill= "#D075A3", alpha=0.3)+
  geom_hline(yintercept=13, linetype="dashed", color = "gray70") +
  #geom_hline(yintercept=estata.hoy$Decesos.diarios, linetype="dashed", color = "red") +
  geom_line(data=subset(Hermosillo.Defuncion, Fecha <=dia.act), aes(x= Fecha, y= Decesos.media.7d, color= "Tendencia promedio móvil 7 días por fecha de ocurridos"),lineend="round", linejoin="round", linetype= "solid", size=.7, arrow=arrow(type="open", length=unit(0.10,"cm")))+
  geom_point(data=subset(Hermosillo.Defuncion, Fecha <=dia.act), aes(x= Fecha, y= Decesos,fill="Decesos ocurridos el día correspondiente"), color = "white", size = 0.6, stroke=0.2, alpha=0.65, shape = 21) +
  geom_point(data=subset(Hermosillo.Defuncion, Fecha >dia.act), aes(x= Fecha, y= Decesos), fill= "#73264D", color = "white", size = 0.6, stroke=0.2, alpha=0.65, shape = 21) +
  geom_point(x=as.Date("2022-02-02"), y= 13, color = "black", fill="transparent", size = 1.2, stroke=0.2, shape = 21) +
  geom_point(x=as.Date("2021-02-03"), y= 13, color = "black", fill="transparent", size = 1.2, stroke=0.2, shape = 21) +
  scale_color_manual(name="", values= c("Tendencia promedio móvil 7 días por fecha de reporte" = alpha("steelblue",0.25),"Tendencia promedio móvil 7 días por fecha de ocurridos" = "#73264D")) +
  scale_color_manual(name="", values= c("Tendencia promedio móvil 7 días por fecha de ocurridos" = "#73264D")) +
  # geom_text(aes(x = as.Date("2020-03-28"), y = (estata.semana$Decesos.diarios + 3),
  #               label = paste0("+", estata.semana$Decesos.diarios, " decesos el mismo día semana anterior")), stat = "unique", family = "Lato Black",
  #           size = 2, color = "gray45", hjust=0)+
  geom_text(aes(x = as.Date("2022-01-29"), y = 15),
                 label = "2 de febrero de 2022\n13 decesos ocurridos", stat = "unique", family = "Lato Black",
             size = 2, color = "black", hjust=1)+
  geom_text(aes(x = as.Date("2022-01-29"), y = 20),
            label = "El día con más decesos ocurridos\ndesde el inicio del proceso de vacunación", stat = "unique", family = "Lato Black",
            size = 3, color = "#993366", hjust=1)+
  geom_text(aes(x = as.Date("2021-02-05"), y = 15),
            label = "3 de febrero de 2021\n13 decesos ocurridos", stat = "unique", family = "Lato Black",
            size = 2, color = "black", hjust=0)+
  # 
  # geom_text(aes(x = as.Date("2020-03-28"), y = estata.hoy$Decesos.diarios + 3,
  #               label = paste0("+", estata.hoy$Decesos.diarios, " decesos hoy")), stat = "unique", family = "Lato Black",
  #           size = 3, color = "red", hjust=0)+
  scale_fill_manual(name="", values= c("Decesos ocurridos el día correspondiente" = alpha("#73264D", 0.45))) + 
  scale_y_continuous(expand = c(0, 0), limits= c(0,50)) +
  scale_x_date(expand=c(0,0), limits= c(min(Hermosillo.Defuncion$Fecha)-5,max(Hermosillo.Defuncion$Fecha)+10), date_breaks = "1 month", 
               labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                            paste(month(x, label = TRUE), "\n", year(x)), 
                                            paste(month(x, label = TRUE)))) +
  guides(fill = guide_legend(order = 1), 
         color = guide_legend(order = 2)) +
  theme_bw() + temaejes +
  theme(legend.text = element_text(family = "Lato", size = 8), legend.background = element_rect(fill="transparent"),legend.box = "horizontal", 
        legend.position = c(0.02,0.95),  legend.justification="left", legend.margin=margin(t = 0, unit='cm'),
        legend.key = element_rect(fill="transparent", color="transparent")) +
  labs(y = NULL, 
       x = NULL,legend= NULL, 
       title  = paste0("<span style = 'font-size:14pt'>Covid-19 en Hermosillo</span><br><span style = 'color:#993366';>Decesos confirmados acumulados: ",prettyNum(as.numeric(estata.hoy$Decesos), big.mark=",", preserve.width="none"),"</span>"), 
       subtitle= Fechahoy, caption =fuentefech)

DecesosSon

ggsave("Gráficos diarios/Hermosillo/diariodecesos.png",DecesosSon,  width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


SospechososD <- Hermosillo.DF %>% ggplot() +
  geom_hline(yintercept=0, linetype="dashed", color = "black") +
  geom_point(aes(x= Fecha, y= Sospechosos.diarios), fill= "#A96AC2", color= "white", size = 0.9, stroke=0.4, alpha=0.65, shape = 21)+
  geom_text(aes(x = as.Date("2021-11-15"), y = -150,
                label = paste0(sintomas.hoy$Sospechosos.Act, " sospechosos en el\nrango de casos activos")), stat = "unique", family = "Lato Black",
            size = 5, color = "red", hjust=0)+
  geom_line(aes(x=Fecha, y=Sospechosos.diarios.7d, color = "Tendencia promedio móvil 7 días"), linetype= "solid", size=.75, arrow=arrow(type="open", length=unit(0.10,"cm")))+
  #geom_area(aes(x=Fecha, y=Sospechosos.7d),color= "transparent", fill= "#A96AC2",alpha=0.1)+
  geom_point( data = subset(Hermosillo.DF , Fecha == max(Fecha)),aes(x= Fecha, y= Sospechosos.diarios), fill="white", size=1 , shape=21, color="#A96AC2", stroke=1) +
  geom_dl( data = subset(Hermosillo.DF , Fecha == max(Fecha)), aes(x= Fecha, y= Sospechosos.diarios, label = Sospechosos.diarios), color="#A96AC2", method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 1.5, fontfamily= "Lato Black")) +
  scale_color_manual(values= c("Tendencia promedio móvil 7 días"= "#A96AC2"))+
  scale_y_continuous(expand = c(0, 0), limits= c(-200,200), breaks=seq(-200,200,50)) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2021-09-01"), max(as.Date(Hermosillo.DF$Fecha))+10), date_breaks = "1 month", date_labels = "%B") +
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_bw() +
  temaejes + 
  theme(legend.text = element_text(family = "Lato", size = 7), legend.background = element_rect(fill="transparent"),
        legend.position = c(0.02,0.9),  legend.justification="left", legend.margin=margin(t = 0, unit='cm'),
        legend.key = element_rect(fill="transparent", color="transparent")) +
  labs(y = NULL, 
       x = NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo</span><br><span style = 'color:#A96AC2';>Variación de pacientes sospechosos al corte</span>", 
       subtitle= Fechahoy, caption =fuente)  

SospechososD
ggsave("Gráficos diarios/Hermosillo/diariosvarSospechosos.png",SospechososD,  width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


Hermosillo.Ingreso <- read_csv("Bases/ST_HermosilloIngreso_SSFED.csv", 
                                 col_types = cols(fecha_ingreso = col_date(format = "%Y-%m-%d")))

Hermosillo.Ingreso <- rename(Hermosillo.Ingreso , Fecha=fecha_ingreso)
Hermosillo.Ingreso <- Dias %>% left_join(Hermosillo.Ingreso, by="Fecha, Analizados")
Hermosillo.Ingreso [is.na(Hermosillo.Ingreso )] <- 0
Hermosillo.Ingreso  <- mutate(Hermosillo.Ingreso , Hospitalizados.media.7d=round(rollmeanr(x=Hospitalizados,7, fill=NA),1))
Hermosillo.Ingreso <- filter(Hermosillo.Ingreso, Fecha>as.Date("2020-03-01"))
Hermosillo.Ingreso [is.na(Hermosillo.Ingreso )] <- 0

HospitalizadosSon <- ggplot(Hermosillo.Ingreso) +
  #geom_line(data=Hermosillo.ED, aes(x= Fecha, y= Decesos.media.7d, color= "Tendencia promedio móvil 7 días por fecha de reporte"),lineend="round", linejoin="round", linetype= "solid", size=.5, arrow=arrow(type="open", length=unit(0.10,"cm")))+
  geom_area(data=subset(Hermosillo.Ingreso, Fecha <=dia.act), aes( x= Fecha, y= Hospitalizados.media.7d), fill= "#F79646", alpha=0.15)+
  #geom_hline(yintercept=estata.semana$Decesos.diarios, linetype="dashed", color = "gray45") +
  #geom_hline(yintercept=estata.hoy$Decesos.diarios, linetype="dashed", color = "red") +
  geom_line(data=subset(Hermosillo.Ingreso, Fecha <=dia.act), aes(x= Fecha, y= Hospitalizados.media.7d, color= "Tendencia promedio móvil 7 días por fecha de ingreso"),lineend="round", linejoin="round", linetype= "solid", size=.7, arrow=arrow(type="open", length=unit(0.10,"cm")))+
  #geom_point(data=subset(Hermosillo.Defuncion, Fecha <=dia.act), aes(x= Fecha, y= Decesos,fill="Decesos ocurridos el día correspondiente"), color = "white", size = 0.6, stroke=0.4, alpha=0.65, shape = 21) +
  #geom_point(data=subset(Hermosillo.Defuncion, Fecha >dia.act), aes(x= Fecha, y= Decesos), fill= "#73264D", color = "white", size = 0.6, stroke=0.2, shape = 21) +
  #scale_color_manual(name="", values= c("Tendencia promedio móvil 7 días por fecha de reporte" = alpha("steelblue",0.25),"Tendencia promedio móvil 7 días por fecha de ocurridos" = "#73264D")) +
  scale_color_manual(name="", values= c("Tendencia promedio móvil 7 días por fecha de ingreso" = "#F79646")) +
  # geom_text(aes(x = as.Date("2020-03-28"), y = (estata.semana$Decesos.diarios + 3),
  #               label = paste0("+", estata.semana$Decesos.diarios, " decesos el mismo día semana anterior")), stat = "unique", family = "Lato Black",
  #           size = 2, color = "gray45", hjust=0)+
  # 
  # geom_text(aes(x = as.Date("2020-03-28"), y = estata.hoy$Decesos.diarios + 3,
  #               label = paste0("+", estata.hoy$Decesos.diarios, " decesos hoy")), stat = "unique", family = "Lato Black",
  #           size = 3, color = "red", hjust=0)+
  scale_fill_manual(name="", values= c("Decesos ocurridos el día correspondiente" = alpha("#F79646", 0.25))) + 
  scale_y_continuous(expand = c(0, 0), limits= c(0,50)) +
  scale_x_date(expand=c(0,0), limits= c(min(Hermosillo.Defuncion$Fecha)-5,max(Hermosillo.Ingreso$Fecha)+10), date_breaks = "1 month", 
               labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                            paste(month(x, label = TRUE), "\n", year(x)), 
                                            paste(month(x, label = TRUE)))) +
  guides(fill = guide_legend(order = 1), 
         color = guide_legend(order = 2)) +
  theme_bw() + temaejes +
  theme(legend.text = element_text(family = "Lato", size = 8), legend.background = element_rect(fill="transparent"),legend.box = "horizontal", 
        legend.position = c(0.02,0.95),  legend.justification="left", legend.margin=margin(t = 0, unit='cm'),
        legend.key = element_rect(fill="transparent", color="transparent")) +
  labs(y = NULL, 
       x = NULL,legend= NULL, 
       title  = paste0("<span style = 'font-size:14pt'>Covid-19 en Hermosillo</span><br><span style = 'color:#F79646';>Hospitalizados confirmados acumulados: ",prettyNum(as.numeric(estata.hoy$Hospitalizados), big.mark=",", preserve.width="none"),"</span>"), 
       subtitle= Fechahoy, caption =fuentefech)

HospitalizadosSon

ggsave("Gráficos diarios/Hermosillo/diariohospitalizados.png",HospitalizadosSon,  width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


Hospitalizados <- Hermosillo.Ingreso %>% select(Fecha, Hospitalizados, Hospitalizados.media.7d) %>% mutate(Hospitalizados.relativo=round(Hospitalizados.media.7d/44.2,3))
Decesos <- Hermosillo.Defuncion %>% select(Fecha, Decesos, Decesos.media.7d) %>% mutate(Decesos.relativo=round(Decesos.media.7d/20.9,3))
Casos <- Hermosillo.Sintomas %>% select(Fecha, Casos, Casos.media.7d) %>% mutate(Casos.relativo=round(Casos.media.7d/210.9,3))

Hermosillo.relativo <- Casos %>% left_join(Decesos, by="Fecha") %>% left_join(Hospitalizados, by="Fecha")

write.csv(Hermosillo.relativo,'ResultadoCSV/Hermosillo.csv')



Relativos <- ggplot(Hermosillo.relativo) +
  geom_hline(yintercept=1, linetype="solid", color = "gray75") +
  geom_text(aes(x = as.Date("2020-03-01"), y = 1.03),
                label = "100%", stat = "unique", family = "Lato Black",
            size = 2, color = "gray75", hjust=0)+
  geom_line(data=subset(Hermosillo.relativo, Fecha <=dia.act+7), aes(x= Fecha, y= Hospitalizados.relativo), color= "#F79646",lineend="round", linejoin="round", linetype= "solid", size=.5, alpha=0.8)+
  geom_line(data=subset(Hermosillo.relativo, Fecha <=dia.act+7), aes(x= Fecha, y= Decesos.relativo), color= "#993366",lineend="round", linejoin="round", linetype= "solid", size=.5, alpha=0.8)+
  geom_line(data=subset(Hermosillo.relativo, Fecha <=dia.act+7), aes(x= Fecha, y= Casos.relativo), color= "#01A2AC",lineend="round", linejoin="round", linetype= "solid", size=.5, alpha=0.8)+
  scale_y_continuous(expand = c(0, 0), limits= c(0,1.6), labels = scales::percent_format(scale = 100), position = "right") +
  scale_x_date(expand=c(0,0), limits= c(min(Hermosillo.Defuncion$Fecha)-5,max(Hermosillo.Ingreso$Fecha)+10), date_breaks = "1 month", 
               labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                            paste(month(x, label = TRUE), "\n", year(x)), 
                                            paste(month(x, label = TRUE)))) +
  theme_bw() + temaejes +
  theme(legend.text = element_text(family = "Lato", size = 8), legend.background = element_rect(fill="transparent"),legend.box = "horizontal", 
        legend.position = c(0.02,0.95),  legend.justification="left", legend.margin=margin(t = 0, unit='cm'),
        legend.key = element_rect(fill="transparent", color="transparent")) +
  labs(y = NULL, 
       x = NULL,legend= NULL, 
       title  = paste0("<span style = 'font-size:14pt'>Covid-19 en Hermosillo</span><br><span style = 'color:#01A2AC';>Casos, </span><span style = 'color:#F79646';>Hospitalizados y </span><span style = 'color:#993366';>Decesos</span><br>respecto al máximo anterior a la 4ta. ola"), 
       subtitle= paste0("Casos por fecha de inicio de síntomas, hospitalizados por fecha de ingreso, y decesos por fecha de defunción\n", Fechahoy), caption =fuentefech)

Relativos

ggsave("Gráficos diarios/Hermosillo/diariorelativo.png",Relativos,  width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)

