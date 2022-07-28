
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

dia <- Sys.Date()
fuente <- "Elaboración Luis Armando Moreno (@dogomoreno) de la Secretaría de Salud del Gobierno de la República.\nwww.luisarmandomoreno.com"

subact <- paste0("Casos confirmados que iniciaron síntomas dentro de los 14 días previos al ", day(dia), "/", month(dia),"/", year(dia), ".\nCada cuadro representa a 1%" )
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



SINTOMAS <- read_csv("Bases/COVIDSONORA_SINTOMAS.csv", 
                     col_types = cols(X1 = col_skip(), fecha_sintomas = col_date(format = "%Y-%m-%d")), 
                     locale = locale(encoding = "ISO-8859-1"))


act <- SINTOMAS %>%
  mutate(dias_sintomas=as.numeric(Sys.Date()-as.Date(fecha_sintomas))) 

# Casos Activos
act <- act %>%
  mutate(activo=if_else(dias_sintomas>13,"No", "Sí")) %>% filter(activo=="Sí")

activos <- act %>% 
   summarise(Totales=sum(confirmados), Hospitalizados=sum(hospitalizados)-sum(decesos), Ambulatorios=sum(ambulatorios), Decesos=sum(decesos)) 


sexo <- act %>% 
  group_by(sexo) %>% summarise(Totales=sum(confirmados), Hospitalizados=sum(hospitalizados)-sum(decesos), Ambulatorios=sum(ambulatorios), Decesos=sum(decesos))


edad <- act %>% 
  group_by(grupoedad) %>% summarise(Totales=sum(confirmados), Hospitalizados=sum(hospitalizados)-sum(decesos), Ambulatorios=sum(ambulatorios), Decesos=sum(decesos))

edadactivos <- edad %>% select(grupoedad,Totales)

sexoedad <- act %>% 
  group_by(sexo,grupoedad) %>% summarise(Totales=sum(confirmados), Hospitalizados=sum(hospitalizados)-sum(decesos), Ambulatorios=sum(ambulatorios), Decesos=sum(decesos))


edadactivos <- edad %>% select(grupoedad,Totales) %>%
mutate(grupoedad=paste0(grupoedad, " años"), PctjAct=round(Totales*100/sum(Totales),1))

edadactivos$label <- c(paste0(edadactivos$grupoedad, "\n ", edadactivos$PctjAct,"%"))
leyenda <- paste0(sum(edadactivos$Totales)," casos activos")
edadactivosWaff <- ggplot(edadactivos, aes(values = Totales, fill= grupoedad, color=grupoedad)) + 
  geom_waffle(    n_rows = 15, size = .5, flip=TRUE,
                  radius = unit(7, "pt"), make_proportional = TRUE) +
  scale_fill_manual(values=c(alpha("#3B9494",0.25), alpha("#3B9494",0.75), "#3B9494", alpha("#3B9494",0.5)  ), labels=edadactivos$label) +
  scale_color_manual(values=c("white", "white",  "white",  "white")) +
  guides(colour = "none", fill=guide_legend(reverse = TRUE, title=leyenda)) +
  coord_equal() + theme_ipsum_rc(grid="") +
  theme_enhance_waffle() + temasinejes +
  theme(plot.title = element_markdown(family = "Lato Black", size = 23), plot.background = element_rect(fill = "white", color = "white", size = 3),
        axis.line.x = element_blank(), axis.text.x = element_blank(), plot.margin = margin(10, 15, 10, 15),legend.title= element_text(family = "Lato Black", size = 10),
        legend.position = "right", legend.key.size = unit(1, 'cm'), legend.key = element_rect(size = 1),        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm"), legend.text.align = 0, axis.title.x= element_text(family = "Lato Black", size = 10),)+
  labs(y = NULL, 
       x = "Cada cuadro representa a 1%",legend= leyenda,  title  = "<span style = 'font-size:14pt'>Covid-19 en Sonora:</span><br><span style = 'color:#58BCBC';>Casos activos al corte</span>", 
       subtitle= subact, caption =fuente)
edadactivosWaff

ggsave("Gráficos semanales/s27.png",edadactivosWaff , width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)

sexoactivos <- sexo %>% select(sexo,Totales) %>%
  mutate(sexo=paste0(sexo), PctjAct=round(Totales*100/sum(Totales),1))

sexoactivos$label <- c(paste0(sexoactivos$sexo, "\n ", sexoactivos$PctjAct,"%"))
leyenda <- paste0(sum(sexoactivos$Totales)," casos activos")
sexoactivosWaff <- ggplot(sexoactivos, aes(values = Totales, fill= sexo, color=sexo)) + 
  geom_waffle(    n_rows = 15, size = .5, flip=TRUE,
                  radius = unit(7, "pt"), make_proportional = TRUE) +
  scale_fill_manual(values=c(alpha("#3B9494",0.25), alpha("#3B9494",0.75), "#3B9494", alpha("#3B9494",0.5)  ), labels=sexoactivos$label) +
  scale_color_manual(values=c("white", "white",  "white",  "white")) +
  guides(colour = "none", fill=guide_legend(reverse = TRUE, title=leyenda)) +
  coord_equal() + theme_ipsum_rc(grid="") +
  theme_enhance_waffle() + temasinejes +
  theme(plot.title = element_markdown(family = "Lato Black", size = 23), plot.background = element_rect(fill = "white", color = "white", size = 3),
        axis.line.x = element_blank(), axis.text.x = element_blank(), plot.margin = margin(10, 15, 10, 15),legend.title= element_text(family = "Lato Black", size = 10),
        legend.position = "right", legend.key.size = unit(1, 'cm'), legend.key = element_rect(size = 1),        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm"), legend.text.align = 0, axis.title.x= element_text(family = "Lato Black", size = 10),)+
  labs(y = NULL, 
       x = "Cada cuadro representa a 1%",legend= leyenda,  title  = "<span style = 'font-size:14pt'>Covid-19 en Sonora:</span><br><span style = 'color:#58BCBC';>Casos activos al corte</span>", 
       subtitle= subact, caption =fuente)
sexoactivosWaff

ggsave("Gráficos semanales/s29.png",sexoactivosWaff , width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


activos <- activos %>% pivot_longer(names_to = "Estatus", cols = c("Hospitalizados", "Ambulatorios", "Decesos"), values_to = "Casos")
statusactivos <- activos %>% select(Estatus,Casos) %>%
  mutate(Estatus=paste0(Estatus), PctjAct=round(Casos*100/sum(Casos),1))

statusactivos$label <- c(paste0(statusactivos$Estatus, "\n ", statusactivos$PctjAct,"%"))
leyenda <- paste0(sum(statusactivos$Casos)," casos activos")
estatusactivosWaff <- ggplot(statusactivos, aes(values = Casos, fill= Estatus, color=Estatus)) + 
  geom_waffle(    n_rows = 15, size = .5, flip=TRUE,
                  radius = unit(7, "pt"), make_proportional = TRUE) +
  scale_fill_manual(values=c(alpha("#3B9494",0.25), alpha("#3B9494",0.75), "#3B9494", alpha("#3B9494",0.5)  ), labels=statusactivos$label) +
  scale_color_manual(values=c("white", "white",  "white",  "white")) +
  guides(colour = "none", fill=guide_legend(reverse = TRUE, title=leyenda)) +
  coord_equal() + theme_ipsum_rc(grid="") +
  theme_enhance_waffle() + temasinejes +
  theme(plot.title = element_markdown(family = "Lato Black", size = 23), plot.background = element_rect(fill = "white", color = "white", size = 3),
        axis.line.x = element_blank(), axis.text.x = element_blank(), plot.margin = margin(10, 15, 10, 15),legend.title= element_text(family = "Lato Black", size = 10),
        legend.position = "right", legend.key.size = unit(1, 'cm'), legend.key = element_rect(size = 1),        legend.key.height = unit(1, "cm"),
        legend.key.width = unit(1, "cm"), legend.text.align = 0, axis.title.x= element_text(family = "Lato Black", size = 10),)+
  labs(y = NULL, 
       x = "Cada cuadro representa a 1%",legend= leyenda,  title  = "<span style = 'font-size:14pt'>Covid-19 en Sonora:</span><br><span style = 'color:#58BCBC';>Casos activos al corte</span>", 
       subtitle= subact, caption =fuente)
estatusactivosWaff

ggsave("Gráficos semanales/s29.png",sexoactivosWaff , width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)



