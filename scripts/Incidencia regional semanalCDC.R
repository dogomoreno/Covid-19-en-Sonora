rm(list=ls())

library(tidyverse)
library(extrafont)
library(scales)
library(tint)
library(miniUI)
library(units)
library(lubridate)
library(zoo)
library(Cairo)
#library(wesanderson)
#library(ggsci)
library(RColorBrewer)
#library(rcartocolor)
#library(NineteenEightyR)
library(magick)

logo <- image_read("Shapes/SEDFesp.png")



#discreta <- c("5" = "#893F59", "4" = "#ECA48E","3" = "#FECF7D","2" = "#9BD7D7", "1" = "gray80")
#discreta <- c("5" = "#893F59", "4" = "#ECA48E","3" = "#FECF7D","2" = "#9BD7D7", "1" = "gray80")
discreta <- c("4" = "#CE3F41","3" = "#FFA17B","2" = "#FECF7D", "1" = "#31859C")
#discreta <- wes_palette("Zissou1", 5, type = "discrete")
#discreta <- pal_ucscgb("default", alpha=0.5)(5)
#discreta <- brewer.pal(4, "RdYlBu")
#discreta <- rev(carto_pal(4, "Temps"))
#discreta <- miami1(n = 5, alpha = 0.9)

Casos <-  read_csv("Bases/Activosdiarios_SSFED.csv", 
                   col_types =cols(CASOS_ACTIVOS = col_integer(), 
                                   CVEGEO = col_character(), Fecha = col_date(format = "%Y-%m-%d"), 
                                   MUNICIPIO = col_character(), X1 = col_skip()), 
                   locale = locale(encoding = "UTF-8")) %>% filter(Fecha>=as.Date("2022-01-01")) %>% 
  rename(CASOS=CASOS_ACTIVOS)

POBMUN <- read_csv("Bases/POBMUN.csv", col_types = cols(CVEGEO = col_character()), 
                   locale = locale(encoding = "latin1"))

Casossemana <- Casos %>% group_by(MUNICIPIO) %>% 
  mutate(diasemana = weekdays(Fecha)) %>% 
  filter(diasemana==weekdays(max(as.Date(Fecha)))) %>% 
  left_join(POBMUN, by = "CVEGEO") 
Casossemana <- Casossemana %>% mutate (INCIDENCIA= round((CASOS*100000)/POB,1))
Casossemana$INCIDENCIA[Casossemana$INCIDENCIA==0] <- NA
# quantile(Casossemana$INCIDENCIA, seq(0,1, 0.05), na.rm=TRUE)
# Muyalto <- quantile(Casossemana$INCIDENCIA, 0.90, na.rm=TRUE)
# Alto <- quantile(Casossemana$INCIDENCIA, 0.75, na.rm=TRUE)
# Medio <- quantile(Casossemana$INCIDENCIA, 0.50, na.rm=TRUE)
# Bajo <- quantile(Casossemana$INCIDENCIA, 0.25, na.rm=TRUE)
casossempob <- Casossemana %>% mutate(IS=if_else(INCIDENCIA>=99.9,4, 
                                                           if_else(INCIDENCIA>49.9,3,
                                                                   if_else(INCIDENCIA>9.9,2,1))))
  
#Estilo del gráfico
paragraf <- theme(plot.title = (element_text(family = "Lato Black", size = 32, color = "black")),
                  plot.subtitle = (element_text(family = "Lato Light", size = 12, color = "gray50")),
                  legend.key.height = unit (1, "cm"), legend.position = "right",   
                  axis.text.y = element_text(family = "Lato Light", size = 15, color = "black"), 
                  axis.text.x = element_blank(),
                  legend.text = element_text(family = "Lato", size = 8, color = "black"),
                  panel.background = element_rect(fill="gray97") ,
                  panel.grid.major.y = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  legend.title = element_text(family = "Lato Black", size = 8, color = "black"),
                  plot.caption = element_text(family = "Lato Light", size =10, color = "gray50"),
                  axis.title = element_text(family = "Lato", size = 12))
subtitulo <- paste0("Incidencia semanal de casos activos confirmados de covid-19 al corte \nAl reporte del ", day(max(Casos$Fecha)),"/",month(max(Casos$Fecha)),"/",year(max(Casos$Fecha)))
Semanalab <- paste0("Semanas del 2022 de ", weekdays(max(Casos$Fecha)+1)," a ", weekdays(max(Casos$Fecha)), " (por fecha de corte)")
marcas <- c( "Alta (100 o más)", "Substancial (50-99)", "Moderada (10-49)","Baja (+0-9)")
romp <- c("4", "3", "2", "1")
capt <- "Elaboración Luis Armando Moreno con información de la Secretaría de Salud del Gobierno de la República.| Por fecha de reporte."

#Río Sonora
Region <- "Río Sonora"
casossempobF  <- casossempob %>% filter(CLAS_REG==Region)

IncidenciaG <- ggplot(data = casossempobF) + 
  geom_tile(mapping = aes(x = Fecha, y = reorder(MUNICIPIO, desc(MUNICIPIO)), fill = as.factor(IS)), color = "white", size=0.5) +
  scale_fill_manual("INCIDENCIA\n(casos por 100 mil habs.)", 
                    values = discreta, 
                    breaks= romp, 
                    labels = marcas, 
                    na.value = "gray95")+
  #scale_x_date(date_breaks = "month" , date_labels = "%d-%m") +
  scale_x_date(date_breaks = "1 month", date_labels = "%B") +
  theme_minimal() +
  labs(y = NULL, x = Semanalab, title  = paste("Región", Region), 
       subtitle = subtitulo,  fill = NULL, 
       caption =capt) +
  paragraf 
IncidenciaG <- cowplot::ggdraw() +
  cowplot::draw_plot(IncidenciaG,x = 0, y = 0, width = 1, height = 1) +
  cowplot::draw_image(logo, x = 0.88, y = 0.89, width = 0.1, height = 0.1)  
ggsave(paste("Gráficos semanales/regional/",Region,".png", sep = ""),IncidenciaG, bg = "white", height = 15, width = 30, units = "cm", dpi = 800, type = "cairo")


# Sierra Alta
Region <- "Sierra Alta"
casossempobF  <- casossempob %>% filter(CLAS_REG==Region)

IncidenciaG <- ggplot(data = casossempobF) + 
  geom_tile(mapping = aes(x = Fecha, y = reorder(MUNICIPIO, desc(MUNICIPIO)), fill = as.factor(IS)), color = "white", size=0.5) +
  scale_fill_manual("INCIDENCIA\n(casos por 100 mil habs.)", 
                    values = discreta, 
                    breaks= romp, 
                    labels = marcas, 
                    na.value = "gray95")+
  #scale_x_date(date_breaks = "month" , date_labels = "%d-%m") +
  scale_x_date(date_breaks = "1 month", date_labels = "%B") +
  theme_minimal() +
  labs(y = NULL, x = Semanalab, title  = paste("Región", Region), 
       subtitle = subtitulo,  fill = NULL, 
       caption =capt) +
  paragraf 
IncidenciaG <- cowplot::ggdraw() +
  cowplot::draw_plot(IncidenciaG,x = 0, y = 0, width = 1, height = 1) +
  cowplot::draw_image(logo, x = 0.88, y = 0.89, width = 0.1, height = 0.1) 
ggsave(paste("Gráficos semanales/regional/",Region,".png", sep = ""),IncidenciaG, bg = "white", height = 15, width = 30, units = "cm", dpi = 800, type = "cairo")

# Sierra Centro
Region <- "Sierra Centro"

casossempobF  <- casossempob %>% filter(CLAS_REG==Region)

IncidenciaG <- ggplot(data = casossempobF) + 
  geom_tile(mapping = aes(x = Fecha, y = reorder(MUNICIPIO, desc(MUNICIPIO)), fill = as.factor(IS)), color = "white", size=0.5) +
  scale_fill_manual("INCIDENCIA\n(casos por 100 mil habs.)", 
                    values = discreta, 
                    breaks= romp, 
                    labels = marcas, 
                    na.value = "gray95") +
  #scale_x_date(date_breaks = "month" , date_labels = "%d-%m") +
  scale_x_date(date_breaks = "1 month", date_labels = "%B") +
  theme_minimal() +
  labs(y = NULL, x = Semanalab, title  = paste("Región", Region), 
       subtitle = subtitulo,  fill = NULL, 
       caption =capt) +
  paragraf 
IncidenciaG <- cowplot::ggdraw() +
  cowplot::draw_plot(IncidenciaG,x = 0, y = 0, width = 1, height = 1) +
  cowplot::draw_image(logo, x = 0.88, y = 0.89, width = 0.1, height = 0.1) 
ggsave(paste("Gráficos semanales/regional/",Region,".png", sep = ""),IncidenciaG, bg = "white", height = 15, width = 30, units = "cm", dpi = 800, type = "cairo")

#Sur
Region <- "Sur"
casossempobF  <- casossempob %>% filter(CLAS_REG==Region)

IncidenciaG <- ggplot(data = casossempobF) + 
  geom_tile(mapping = aes(x = Fecha, y = reorder(MUNICIPIO, desc(MUNICIPIO)), fill = as.factor(IS)), color = "white", size=0.5) +
  scale_fill_manual("INCIDENCIA\n(casos por 100 mil habs.)", 
                    values = discreta, 
                    breaks=romp, 
                    labels = marcas, 
                    na.value = "gray95")+
  #scale_x_date(date_breaks = "month" , date_labels = "%d-%m") +
  scale_x_date(date_breaks = "1 month", date_labels = "%B") +
  theme_minimal() +
  labs(y = NULL, x = Semanalab, title  = paste("Región", Region), 
       subtitle = subtitulo,  fill = NULL, 
       caption =capt) +
  paragraf 
IncidenciaG <- cowplot::ggdraw() +
  cowplot::draw_plot(IncidenciaG,x = 0, y = 0, width = 1, height = 1) +
  cowplot::draw_image(logo, x = 0.88, y = 0.89, width = 0.1, height = 0.1) 
ggsave(paste("Gráficos semanales/regional/",Region,".png", sep = ""),IncidenciaG, bg = "white", height = 15, width = 30, units = "cm", dpi = 800, type = "cairo")


#Centro Norte
Region <- "Centro Norte"
casossempobF  <- casossempob %>% filter(CLAS_REG==Region)

IncidenciaG <- ggplot(data = casossempobF) + 
  geom_tile(mapping = aes(x = Fecha, y = reorder(MUNICIPIO, desc(MUNICIPIO)), fill = as.factor(IS)), color = "white", size=0.5) +
  scale_fill_manual("INCIDENCIA\n(casos por 100 mil habs.)", 
                    values = discreta, 
                    breaks= romp, 
                    labels = marcas, 
                    na.value = "gray95")+
  #scale_x_date(date_breaks = "month" , date_labels = "%d-%m") +
  scale_x_date(date_breaks = "1 month", date_labels = "%B") +
  theme_minimal() +
  labs(y = NULL, x = Semanalab, title  = paste("Región", Region), 
       subtitle = subtitulo,  fill = NULL, 
       caption =capt) +
  paragraf 
IncidenciaG <- cowplot::ggdraw() +
  cowplot::draw_plot(IncidenciaG,x = 0, y = 0, width = 1, height = 1) +
  cowplot::draw_image(logo, x = 0.88, y = 0.89, width = 0.1, height = 0.1) 
ggsave(paste("Gráficos semanales/regional/",Region,".png", sep = ""),IncidenciaG, bg = "white", height = 15, width = 30, units = "cm", dpi = 800, type = "cairo")



#Noroeste
Region <- "Noroeste"
casossempobF  <- casossempob %>% filter(CLAS_REG==Region)

IncidenciaG <- ggplot(data = casossempobF) + 
  geom_tile(mapping = aes(x = Fecha, y = reorder(MUNICIPIO, desc(MUNICIPIO)), fill = as.factor(IS)), color = "white", size=0.5) +
  scale_fill_manual("INCIDENCIA\n(casos por 100 mil habs.)", 
                    values = discreta, 
                    breaks=romp, 
                    labels = marcas, 
                    na.value = "gray95")+
  #scale_x_date(date_breaks = "month" , date_labels = "%d-%m") +
  scale_x_date(date_breaks = "1 month", date_labels = "%B") +
  theme_minimal() +
  labs(y = NULL, x = Semanalab, title  = paste("Región", Region), 
       subtitle = subtitulo,  fill = NULL, 
       caption =capt) +
  paragraf  
IncidenciaG <- cowplot::ggdraw() +
  cowplot::draw_plot(IncidenciaG,x = 0, y = 0, width = 1, height = 1) +
  cowplot::draw_image(logo, x = 0.88, y = 0.89, width = 0.1, height = 0.1) 
ggsave(paste("Gráficos semanales/regional/",Region,".png", sep = ""),IncidenciaG, bg = "white", height = 15, width = 30, units = "cm", dpi = 800, type = "cairo")

