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
library(magick)

logo <- image_read("Shapes/SEDFesp.png")
logoD <- image_read("Shapes/SEDdecesos.png")
logoH <- image_read("Shapes/SEDhosp.png")
logoP <- image_read("Shapes/SEDpruebas.png")
logoS <- image_read("Shapes/SEDsosp.png")

Sonora.DF <- read_csv("Bases/ST_SonoraReporte_SSFED.csv", 
                      col_types = cols(fecha_reporte = col_date(format = "%Y-%m-%d"))) %>% filter(!is.na(fecha_reporte)) 

dia<-max(as.Date(Sonora.DF$fecha_reporte))

Fechahoy <- paste0("Al reporte del ", day(dia), " de ", months.Date(dia)," de ", year(dia),".")
fuente <- "Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud del Gobierno de la República\n*Por fecha de reporte | www.luisarmandomoreno.com"
fuenteimp <- "Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud del Gobierno de la República y Secretaría de Salud del estado de Sonora.\nwww.luisarmandomoreno.com"
fuenteprueba <- "Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud del Gobierno de la República\n*Incluye resultados válidos de pruebas PCR y antigénicas. | www.luisarmandomoreno.com"
fuenteact <- "Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud del Gobierno de la República\n*Se consideran casos activos aquellos que iniciaron síntomas dentro de los 14 días previos al corte. | www.luisarmandomoreno.com"
subtitulo <- paste0("Casos confirmados en los últimos 7 días por 100 mil habitantes\nAl reporte del ",day(dia),"/",month(dia),"/",year(dia))
subtituloact <- paste0("Casos activos (que iniciaron síntimas dentro de los 14 días previos) por 100 mil habitantes.\nAl reporte del ",day(dia),"/",month(dia),"/",year(dia))

fuentefech <- "Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud del Gobierno de la República\n*El registro de los últimos 14 días aún se están alimentando en el sistema | www.luisarmandomoreno.com"
POBMUN <- read_csv("Bases/POBMUN.csv", col_types = cols(CVEGEO = col_character()), 
                   locale = locale(encoding = "ISO-8859-1"))

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
                      col_types = cols(fecha_reporte = col_date(format = "%Y-%m-%d"))) %>% filter(fecha_reporte>as.Date("2020-10-08")) %>% 
  filter(!is.na(fecha_reporte)) 
Sonora.DF.hoy <- filter(Sonora.DF, fecha_reporte == dia)
Sonora.DF.hoy <- select(Sonora.DF.hoy, Hospitalizados.Activos, Ambulatorios.Activos, Decesos, Recuperados)
Sonora.DF.hoy <- rename(Sonora.DF.hoy, "Ambulatorios activos"= Ambulatorios.Activos, "Hospitalizados Activos"=Hospitalizados.Activos)
Sonora.DF.hoy <- gather(Sonora.DF.hoy, key= Estatus, value= Casos.confirmados) 


# Bases municipales
Casos <- read_csv("Bases/Casosdiarios_SSFED.csv", 
                  col_types = cols(CASOS = col_integer(), 
                                   CVEGEO = col_character(), Fecha = col_date(format = "%Y-%m-%d"), 
                                   MUNICIPIO = col_character(), NUEVOS = col_integer(), X1 = col_skip()), 
                  locale = locale(encoding = "ISO-8859-1")) %>% filter(Fecha>as.Date("2020-10-08")) %>% filter(CVEGEO!=26999)
casosacumdia <- Casos %>% filter(Fecha==max(as.Date(Fecha))) %>% filter(NUEVOS!=0) %>% select (Fecha, MUNICIPIO, CASOS, NUEVOS) %>% arrange(desc(NUEVOS)) %>% 
write.csv('ResultadoCSV/casoshoy.csv')


Decesos <- read_csv("Bases/Decesosdiarios_SSFED.csv", 
                    col_types = cols(DECESOS = col_integer(), 
                                     CVEGEO = col_character(), Fecha = col_date(format = "%Y-%m-%d"), 
                                     MUNICIPIO = col_character(), NUEVOS = col_integer(), X1 = col_skip()), 
                    locale = locale(encoding = "ISO-8859-1")) %>% filter(Fecha>as.Date("2020-10-08")) %>% filter(CVEGEO!=26999)
decesosacumdia <- Decesos %>% filter(Fecha==max(as.Date(Fecha))) %>% filter(NUEVOS!=0) %>% select (Fecha, MUNICIPIO, DECESOS, NUEVOS) %>% arrange(desc(NUEVOS)) %>% 
  write.csv('ResultadoCSV/decesoshoy.csv')


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
#        x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Sonora</span><br><span style = 'color:#01A2AC';>Trayectoria de casos en los municipios</span>", 
#        subtitle= Fechahoy, caption =fuente) 
#   #scale_x_continuous(trans = "log10") + scale_y_continuous(trans = "log10")
# 
# ggsave("Gráficos diarios/diariocasostray.png",Casosd , width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)
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
#        x = "Casos acumulados (log)",legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Sonora</span><br><span style = 'color:#993366';>Trayectoria de decesos en los municipios</span>", 
#        subtitle= Fechahoy, caption =fuente) + 
#   scale_y_continuous (expand = c(0, 0)) + 
#   scale_x_continuous (expand = c(0, 0))
# 
# ggsave("Gráficos diarios/diariodecesostray.png",Decesosd , width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)




# Mapa incidencia

#discrete <- c("5" = "black", "4" = "#005155","3" = "#01787E","2" = "#01A2AC", "1" = "#58BCBC")


Casossemana <- Casos %>% group_by(MUNICIPIO) %>% 
  mutate(diasemana = weekdays(Fecha)) %>% 
  filter(diasemana==weekdays(max(as.Date(Fecha)))) %>% mutate(Casossemana =  (CASOS - lag(CASOS, default = 0, order_by=Fecha))) %>% 
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
  labs(y = NULL, x = NULL, title  = "<span style = 'font-size:12pt'>Covid-19 en Sonora</span><br><span style = 'color:#01A2AC';>Incidencia semanal</span>", 
       subtitle = subtitulo,  fill = NULL, 
       caption =fuente)+
       geom_polygon(data=capa_reg, aes(x=long, y=lat, group=group), 
             fill="transparent", color="black", size=0.2)
Mapa_incidencia <- cowplot::ggdraw() +
  cowplot::draw_plot(Mapa_incidencia ,x = 0, y = 0, width = 1, height = 1) +
  cowplot::draw_image(logo, x = 0.85, y = 0.89, width = 0.1, height = 0.1)  

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
  write.csv('ResultadoCSV/Activosmunhoy.csv')


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
Mapa_act <- cowplot::ggdraw() +
  cowplot::draw_plot(Mapa_act,x = 0, y = 0, width = 1, height = 1) +
  cowplot::draw_image(logo, x = 0.85, y = 0.89, width = 0.1, height = 0.1)  
ggsave("Gráficos diarios/diariomapinci.png",Mapa_act, width = 5/2 * (16/9), height = 5, type = "cairo", dpi = 400)


Sonora.DF <- read_csv("Bases/ST_SonoraReporte_SSFED.csv", 
                      col_types = cols(fecha_reporte = col_date(format = "%Y-%m-%d")))
Sonora.DF <- rename(Sonora.DF, Fecha=fecha_reporte)
Sonora.DF <- mutate(Sonora.DF, Letalidad= round((Decesos / Casos)*100,1))

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



library(directlabels)
Letalidad <- Sonora.DF %>% ggplot(aes(x= Fecha, y= Letalidad)) +
  geom_line(color= "#993366", linetype= "solid", size=1, alpha=0.6)+
  geom_point( data = subset(Sonora.DF , Fecha == max(Fecha)), fill="#993366", size=2 , shape=21, color="white", stroke=1) +
  geom_dl( data = subset(Sonora.DF , Fecha == max(Fecha)), aes(label = Letalidad), color="#993366", method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 1.5, fontfamily= "Lato Black")) +
  scale_y_continuous(expand = c(0, 0), limits= c(0,15), breaks=seq(0,15,2)) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-04-01"), max(Sonora.DF$Fecha)+40), date_breaks = "1 month", 
               labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                            paste(month(x, label = TRUE), "\n", year(x)), 
                                            paste(month(x, label = TRUE)))) +
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_bw() +
  temaejes +
  labs(y = "Decesos por cada 100 casos", 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Sonora</span><br><span style = 'color:#993366';>Letalidad acumulada</span>", 
       subtitle= Fechahoy, caption =fuente)  

Letalidad <- cowplot::ggdraw() +
  cowplot::draw_plot(Letalidad,x = 0, y = 0, width = 1, height = 1) +
  cowplot::draw_image(logoD, x = 0.88, y = 0.89, width = 0.1, height = 0.1)

ggsave("Gráficos semanales/s13.png",Letalidad,  width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)





