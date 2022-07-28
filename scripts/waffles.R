# Paquetes

library(tidyverse)
library(readxl)
library(janitor)
library(extrafont)
library(scales)
library(zoo)
library(lubridate)
library(treemapify)
library(ggsci)
library(Cairo)
library(ggtext)
library(patchwork)
library(ggwaffle)
library(waffle)
library(hrbrthemes)

fuente <- "Elaboración Luis Armando Moreno (@dogomoreno) de la Secretaría de Salud del Gobierno de la República.\nwww.luisarmandomoreno.com"

DEFUNCION <- read_csv("Bases/COVIDSONORA_DEFUNCION.csv", 
                                  col_types = cols(X1 = col_skip(), fecha_def = col_date(format = "%Y-%m-%d")), 
                                  locale = locale(encoding = "ISO-8859-1"))

INGRESO <- read_csv("Bases/COVIDSONORA_INGRESO.csv", 
                      col_types = cols(X1 = col_skip(), fecha_ingreso = col_date(format = "%Y-%m-%d")), 
                      locale = locale(encoding = "ISO-8859-1"))

SINTOMAS <- read_csv("Bases/COVIDSONORA_SINTOMAS.csv", 
                    col_types = cols(X1 = col_skip(), fecha_sintomas = col_date(format = "%Y-%m-%d")), 
                    locale = locale(encoding = "ISO-8859-1"))

Sonora.DF <- read_csv("Bases/ST_SonoraReporte_SSFED.csv", 
                      col_types = cols(fecha_reporte = col_date(format = "%Y-%m-%d")))

dia<-max(as.Date(Sonora.DF$fecha_reporte))

subhosp <- paste0("Casos confirmados que requirieron hospitalización por grupo de edad y\nfueron ingresados en los últimos 30 días con corte al ",day(dia), "/", month(dia),"/", year(dia), ".\nCada cuadro representa a una persona" )
subdef <- paste0("Decesos de casos confirmados por grupo de edad\nCorte al ",day(dia), "/", month(dia),"/", year(dia), ".\nCada círculo representa a 100 personas" )

temasinejes <-  theme(axis.line = element_blank(),
                      plot.margin = margin(10, 25, 10, 25),
                      plot.title = element_text(family = "Lato Black", size = 25),  
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



INGRESOHOSP <- INGRESO %>% select(fecha_ingreso, grupoedad, hospitalizados) %>% 
  filter(fecha_ingreso>Sys.Date()-30) %>% group_by(grupoedad) %>% summarise(hospitalizado=sum(hospitalizados)) %>% ungroup() %>%
  mutate(grupoedad=paste0(grupoedad, " años"), PctjHosp=round(hospitalizado*100/sum(hospitalizado),1))

INGRESOHOSP$label <- c(paste0(INGRESOHOSP$grupoedad, "\n ", INGRESOHOSP$PctjHosp,"%"))
leyenda <- paste0(sum(INGRESOHOSP$hospitalizado)," hospitalizados")
HOSPINGRESO <- ggplot(INGRESOHOSP, aes(values = hospitalizado, fill= grupoedad, color=grupoedad)) + 
  geom_waffle(    n_rows = 40, size = 0.3, flip=TRUE,
                  radius = unit(2, "pt")) +
  scale_fill_manual(values=c(alpha("#E26B0A",0.25), alpha("#E26B0A",0.5), alpha("#E26B0A",0.75),  "#E26B0A"), labels=INGRESOHOSP$label) +
  scale_color_manual(values=c("white", "white",  "white",  "white")) +
  guides(colour = "none", fill=guide_legend(reverse = TRUE, title=leyenda)) +
  coord_equal() + theme_ipsum_rc(grid="") +
  theme_enhance_waffle() + temasinejes +
  theme(plot.title = element_markdown(family = "Lato Black", size = 23), plot.background = element_rect(fill = "white", color = "white", size = 3),
        axis.line.x = element_blank(), axis.text.x = element_blank(), plot.margin = margin(10, 15, 10, 15),legend.title= element_text(family = "Lato Black", size = 10),
        legend.position = "right", legend.key.size = unit(1, 'cm'), legend.key = element_rect(size = 1),        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm"), legend.text.align = 0, axis.title.x= element_text(family = "Lato Black", size = 10),)+
  labs(y = NULL, 
       x = "Cada cuadro representa a una persona",legend= leyenda,  title  = "<span style = 'font-size:14pt'>Covid-19 en Sonora:</span><br><span style = 'color:#F79646';>Hospitalizados durante el último mes</span>", 
       subtitle= subhosp, caption =fuente)
HOSPINGRESO

ggsave("Gráficos semanales/s19.png",HOSPINGRESO , width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)



Decesosmes <- DEFUNCION %>% select(fecha_def, grupoedad, decesos) %>% 
  filter(fecha_def>Sys.Date()-30) %>% group_by(grupoedad) %>% summarise(decesos=sum(decesos)) %>% ungroup() %>%
  mutate(grupoedad=paste0(grupoedad, " años"), PctjDE=round(decesos*100/sum(decesos),1)) %>% filter(decesos!=0)


Decesosmes$label <- c(paste0(Decesosmes$grupoedad, "\n ", Decesosmes$PctjDE,"%"))
leyenda <- paste0(sum(Decesosmes$decesos)," decesos")
DEEstatus <- ggplot(Decesosmes, aes(values = decesos, fill= grupoedad, color=grupoedad)) + 
  geom_waffle(    n_rows = 25, size = 0.3, flip=TRUE,
                  radius = unit(2, "pt")) +
  scale_fill_manual(values=c(alpha("#993366",0.25), alpha("#993366",0.5), alpha("#993366",0.75),  "#993366"), labels=Decesosmes$label) +
  scale_color_manual(values=c("white", "white",  "white",  "white")) +
  guides(colour = "none", fill=guide_legend(reverse = TRUE, title=leyenda)) +
  coord_equal() + theme_ipsum_rc(grid="") +
  theme_enhance_waffle() + temasinejes +
  theme(plot.title = element_markdown(family = "Lato Black", size = 23), plot.background = element_rect(fill = "white", color = "white", size = 3),
        axis.line.x = element_blank(), axis.text.x = element_blank(), plot.margin = margin(10, 15, 10, 15),legend.title= element_text(family = "Lato Black", size = 10),
        legend.position = "right", legend.key.size = unit(1, 'cm'), legend.key = element_rect(size = 1),        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm"), legend.text.align = 0, axis.title.x= element_text(family = "Lato Black", size = 10),)+
  labs(y = NULL, 
       x = "Cada cuadro representa a una persona",legend= leyenda,  title  = "<span style = 'font-size:14pt'>Covid-19 en Sonora:</span><br><span style = 'color:#993366';>Decesos ocurridos en el último mes</span>", 
       subtitle= subdef, caption =fuente)
DEEstatus

ggsave("Gráficos semanales/s17.png",DEEstatus , width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


DEFUNCION <- read_csv("Bases/SonoraCasos.csv", 
                        col_types = cols(fecha_sintomas = col_date(format = "%Y-%m-%d"), 
                                         fecha_def = col_date(format = "%Y-%m-%d"), 
                                         fecha_ingreso = col_date(format = "%Y-%m-%d")), 
                        locale = locale(encoding = "ISO-8859-1")) %>% filter(deceso=="Sí")


Decesostotal <- DEFUNCION %>% select(grupoedad, deceso) %>% 
  group_by(grupoedad) %>% summarise(decesos=n()) %>% ungroup() %>%
  mutate(grupoedad=paste0(grupoedad, " años"), PctjDE=round(decesos*100/sum(decesos),1)) %>% filter(decesos!=0)


Decesostotal$label <- c(paste0(Decesostotal$grupoedad, "\n ", Decesostotal$decesos, ", ",Decesostotal$PctjDE,"%"))
leyenda <- paste0(prettyNum(sum(Decesostotal$decesos), big.mark = ","), " decesos")
DEEstatus <- ggplot(Decesostotal, aes(values = decesos, fill= grupoedad, color=grupoedad)) + 
  geom_waffle(    n_rows = 10, size = 1, flip=FALSE, make_proportional = TRUE,
                  radius = unit(10, "pt")) +
  scale_fill_manual(values=c(alpha("#993366",0.75), alpha("#993366",0.5), alpha("#993366",0.25),  "#993366"), labels=Decesostotal$label,drop = F) +
  scale_color_manual(values=c("white", "white",  "white",  "white")) +
  guides(colour = "none", fill=guide_legend(reverse = TRUE, title=leyenda)) +
  coord_equal() + theme_ipsum_rc(grid="") +
  theme_enhance_waffle() + temasinejes +
  theme(plot.title = element_markdown(family = "Lato Black", size = 23), plot.background = element_rect(fill = "white", color = "white", size = 3),
        axis.line.x = element_blank(), axis.text.x = element_blank(), plot.margin = margin(10, 15, 10, 15),legend.title= element_text(family = "Lato Black", size = 10),
        legend.position = "right", legend.key.size = unit(1, 'cm'), legend.key = element_rect(size = 1),        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm"), legend.text.align = 0, axis.title.x= element_text(family = "Lato Black", size = 10),)+
  labs(y = NULL, 
       x = "Cada cuadro representa a 100 personas",legend= leyenda,  title  = "<span style = 'font-size:14pt'>Covid-19 en Sonora:</span><br><span style = 'color:#993366';>Decesos ocurridos</span>", 
       subtitle= subdef, caption =fuente)
DEEstatus

ggsave("Gráficos semanales/decesostotal.png",DEEstatus , width = 5, height = 5, type = "cairo", dpi = 400)
