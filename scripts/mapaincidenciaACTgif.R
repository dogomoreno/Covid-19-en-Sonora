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
library(gganimate)
library(ggsci)
#library(wesanderson)
#library(ggsci)
#library(RColorBrewer)
library(rcartocolor)
#library(NineteenEightyR)

Fechahoy <- paste0("Al reporte del ", day(dia), " de ", months.Date(dia)," de ", year(dia))
capa_munison <- readOGR("Shapes", layer="MUNSON")
capa_son <- readOGR("Shapes", layer="ENTSON")

Activosmun <- read_csv("Bases/Activosdiarios_SSFED.csv", 
                       col_types = cols(CASOS_ACTIVOS = col_integer(), 
                                        CVEGEO = col_character(), Fecha = col_date(format = "%Y-%m-%d"), 
                                        MUNICIPIO = col_character(), X1 = col_skip()), 
                       locale = locale(encoding = "ISO-8859-1"))
POBMUN <- read_csv("Bases/POBMUN.csv", col_types = cols(CVEGEO = col_character()), 
                   locale = locale(encoding = "ISO-8859-1"))

# Mapa incidencia

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
  mutate(IS=as.character(IS))
Activossempob$IS[is.na(Activossempob$IS)] <- "NA"



Activossempob <- Activossempob %>%  mutate(id=CVEGEO)

capa_munison <- readOGR("Shapes", layer="MUNSON")
capa_reg <- readOGR("Shapes", layer="REGSON")
capa_munison_df <- fortify(capa_munison, region="concat")
capa_munison_inci<- inner_join(capa_munison_df, Activossempob, by="id")

discrete <- c("4" = "#CE3F41","3" = "#FFA17B","2" = "#FECF7D", "1" = "#31859C")
subtitulo <- "Casos de covid-19 en los últimos 7 días por 100 mil habitantes\nCorte al 18/06/2021"
marcas <- c( "Alta\n(100 o más\ncasos por 100 mil habs.)", "Substancial\n(50-99)", "Moderada\n(10-49)","Baja\n(>0-9)", "Nula\n(0)")
romp <- c("4", "3", "2", "1", "NA")
discrete <- c("#CE3F41","#FFA17B","#FECF7D", "#31859C","gray90")

Mapa_incidencia<- ggplot(capa_munison_inci, aes(map_id = id)) +
  geom_polygon(data=capa_munison, aes(x=long, y=lat, group=group), 
               fill="gray90", color="white", size=0.12) +
  geom_map(aes(fill = factor(IS)),color = "white",size=0.22, map = capa_munison_df) + 
  scale_fill_manual(values = discrete, 
                    breaks= romp, 
                    labels = marcas) +
  theme_void() +
  theme(plot.title = (element_text(family = "Lato Black", size = 20, color = "black")),
        plot.subtitle = (element_text(family = "Lato Light", size = 8, color = "#01787E")),
        plot.margin = margin(0.5,1, 0.5, 0.5, "cm"),
        legend.position = "right",
        plot.background = element_rect(fill = "white", color="black", size=3),
        legend.key.height = unit (0.5, "cm"), legend.key.width = unit (0.2, "cm"), axis.text = element_blank(),
        legend.text = element_text(family = "Lato", size = 6, color = "black"),
        legend.title = element_text(family = "Lato Black", size = 5, color = "black"),
        plot.caption = element_text(family = "Lato Light", size = 6.5, color = "gray40"),
        axis.title = element_blank()) +
  labs(y = NULL, x = NULL, title  = "Incidencia semanal", 
       subtitle = subtitulo,  fill = NULL, 
       caption ="Elaboración Luis Armando Moreno con información de la Secretaría de Salud del Estado de Sonora")+
  geom_polygon(data=capa_reg, aes(x=long, y=lat, group=group), 
               fill="transparent", color="black", size=0.2)



diasemana <- seq(min(Activossempob$Fecha), max(Activossempob$Fecha),1)
SonoraMCsemanal  <- filter(Activossempob,Fecha %in% diasemana)

capa_munison_df <- fortify(capa_munison, region="concat")
capa_munison_casos<- inner_join(capa_munison_df, Activossempob, by="id")


Mapa_inci <- function(capa_son, capa_munison_casos) { ggplot(capa_munison_casos, aes(map_id = id)) +
        geom_polygon(data=capa_munison, aes(x=long, y=lat, group=group), 
                 fill="gray90", color="white", size=0.6) +
        geom_map(aes(fill = as.factor(IS)),color = "white",size=0.6, map = capa_munison_df) + 
        geom_polygon(data=capa_son, aes(x=long, y=lat, group=group), 
                 fill="transparent", color="black", size=0.6) +
    scale_fill_manual(values = discrete, breaks= romp, 
                      labels = marcas, drop = F)+
    
    theme_void() +
    theme(plot.title = (element_text(family = "Lato Black", size = 54, color = "black")),
          plot.subtitle = (element_text(family = "Lato Light", size = 22, color = "#01787E")),
          plot.margin = margin(1, 2.5, 1, 2.5, "cm"),
          legend.position = c(0.18,0.4),
          plot.background = element_rect(fill = "white", color="black", size=3),
          legend.key.height = unit (3, "cm"), legend.key.width = unit (0.75, "cm"), axis.text = element_blank(),
          legend.text = element_text(family = "Lato", size = 20, color = "black"),
          legend.title = element_text(family = "Lato Black", size = 28, color = "black"),
          plot.caption = element_text(family = "Lato Light", size = 15, color = "gray40"),
          axis.title = element_blank()) +
    labs(axis = NULL, y = NULL, x = NULL, title = "Incidencia de casos activos", subtitle = "Casos activos de covid-19 (inicio de síntomas dentro de los 14 días\n previos a la fecha de reporte) por 100 mil habitantes",
         caption ="Elaboración Luis Armando Moreno con información de la Secretaría de Salud del Gobierno de la República")
}


Incisemanaanim <- Mapa_inci(capa_son, capa_munison_casos) + 
  transition_manual(Fecha) +
  shadow_mark() +
  labs(fill = paste0("Al reporte del\n","{current_frame}"))

gifincisem <- animate(Incisemanaanim, end_pause = 6, fps = 20,duration = 30, width = 950, height =950, renderer = gifski_renderer())
anim_save("./Gráficos diarios/Incidenciasemanal.gif", animation=gifincisem)


