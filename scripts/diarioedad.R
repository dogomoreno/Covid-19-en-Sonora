rm(list=ls())


# Paquetes

library(tidyverse)
library(extrafont)
library(scales)
library(units)
library(zoo)
library(lubridate)
library("Cairo")
library(directlabels)
library(ggtext)
library(patchwork)
# Función para ejes en números enteros
int_breaks_rounded <- function(x, n = 5)  pretty(x, n)[round(pretty(x, n),1) %% 1 == 0] 

temaejes <- theme(plot.margin = margin(10, 25, 10, 25), panel.grid=element_blank(), panel.border=element_blank(), axis.line= element_line(color = "black", size = 0.3),
                  plot.title = element_markdown(family = "Lato Black", size = 25),  
                  plot.subtitle = element_text(family = "Lato Light", size = 10, color = "black"), legend.title = element_blank(),
                  strip.text = element_markdown(family = "Lato Black", size = 8, hjust=0),
                  axis.text = element_text(family = "Lato", size =5),
                  plot.background = element_rect(fill = "white", color = "white", size = 3),
                  axis.title.x = element_text(family = "Lato Light", size = 8, hjust=1),
                  axis.title.y = element_text(family = "Lato Light", size = 8, hjust=1), 
                  plot.caption = element_text(family = "Lato", size = 6),
                  legend.text = element_blank(), legend.background =  element_rect(fill="transparent", color="transparent"),
                  legend.box.background=  element_rect(fill="transparent", color="transparent" ), 
                  legend.key = element_rect(fill="transparent", color="transparent"),strip.background = element_blank(),
                  legend.position = "none", plot.title.position = 'plot', plot.caption.position = 'plot')


Sonora.DF <- read_csv("Bases/ST_SonoraReporte_SSFED.csv", 
                      col_types = cols(fecha_reporte = col_date(format = "%Y-%m-%d")))

dia<-max(as.Date(Sonora.DF$fecha_reporte))

Fechahoy <- paste0("Al reporte del ", day(dia), " de ", months.Date(dia)," de ", year(dia))
fuente <- "Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud del Gobierno de la República\n*Por fecha de reporte | www.luisarmandomoreno.com"
subtitulo <- paste0("Casos confirmados en los últimos 7 días por 100 mil habitantes\nAl reporte del ",day(dia),"/",month(dia),"/",year(dia))
fuentefech <- "Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud del Gobierno de la República\n*Los últimos 14 días aún se están alimentando en el sistema | www.luisarmandomoreno.com"
POBMUN <- read_csv("Bases/POBMUN.csv", col_types = cols(CVEGEO = col_character()), 
                   locale = locale(encoding = "ISO-8859-1"))

Casos <- Sonora.DF %>% select(fecha_reporte, C_0_14, C_15_29,C_30_59,C_60_mas) %>% 
  select(fecha_reporte, C_0_14, C_15_29,C_30_59,C_60_mas) %>% 
  rename ("0 a 14 años"=C_0_14, "15 a 29 años"=C_15_29,"30 a 59 años"=C_30_59,"60 o más años"=C_60_mas) %>% 
  gather(key= "grupoedad", value= "Casos", ends_with("años"))

Casos <- Casos %>% rename(Fecha = fecha_reporte) %>% group_by(grupoedad) %>% 
  mutate(Casos.diarios= Casos - lag(Casos, default = Casos[1], order_by=Fecha)) %>% 
  mutate(Casos.media.repo.7d=round(rollmeanr(x=Casos.diarios, 7, fill = NA),1)) 

Casos.repo<- Casos %>% select(Fecha, grupoedad, Casos.media.repo.7d)
Casos.repo$Casos.media.repo.7d[Casos.repo$Casos.media.repo.7d<0] <-0

Casos.hoy <- Casos %>% filter(Fecha==max(Fecha)) %>% rename(Casos.hoy= Casos.diarios) %>% select(grupoedad, Casos.hoy)
Casos.semana <- Casos %>% filter(Fecha==max(Fecha)-7) %>%  rename (Casos.semana=Casos.diarios) %>% select(grupoedad, Casos.semana)
dia.act <- max(Casos$Fecha) -14


Sonora.Sintomas <- read_csv("Bases/ST_SonoraSintomas_SSFED.csv", 
                            col_types = cols(fecha_sintomas = col_date(format = "%Y-%m-%d"))) %>% rename(Fecha=fecha_sintomas)

Sonora.Sintomas <- Sonora.Sintomas %>% select(Fecha, C_0_14, C_15_29,C_30_59,C_60_mas) %>% 
  select(Fecha, C_0_14, C_15_29,C_30_59,C_60_mas) %>% 
  rename ("0 a 14 años"=C_0_14, "15 a 29 años"=C_15_29,"30 a 59 años"=C_30_59,"60 o más años"=C_60_mas) %>% 
  gather(key= "grupoedad", value= "Casos", ends_with("años")) 

Sonora.Sintomas <- Sonora.Sintomas %>% 
  group_by(grupoedad) %>% 
  mutate(Casos.media.7d=round(rollmeanr(x=Casos, 7, fill = NA),1)) %>% 
  filter(Fecha>as.Date("2020-03-03")) %>% 
  left_join(Casos.hoy, by="grupoedad") %>% left_join(Casos.semana, by="grupoedad") %>% 
  left_join(Casos.repo, by=c("Fecha","grupoedad"))

Sonora.Sintomas <- Sonora.Sintomas %>%
  mutate(label=paste0(grupoedad, "<br><span style = 'font-size:6pt; font-family:Lato'>",prettyNum(sum(Casos), big.mark=",", preserve.width="none"), " casos acumulados, ", prettyNum(Casos.hoy, big.mark=",", preserve.width="none"), " confirmados hoy</span>"))

# Casos diarios Estatal

CasosSon <- ggplot(Sonora.Sintomas) +
  geom_line(data=subset(Sonora.Sintomas, Fecha >= as.Date("2021-01-01")), aes(x= Fecha, y= Casos.media.repo.7d, color= "Tendencia promedio móvil 7 días por reporte"), linetype= "solid", size=.45, lineend="round", linejoin="round", arrow=arrow(type="open", length=unit(0.10,"cm")))+
  geom_area(data=subset(Sonora.Sintomas, Fecha <= dia.act), aes( x= Fecha, y= Casos.media.7d), fill= "#58BCBC", alpha=0.3)+
  # geom_hline(yintercept=max(Sonora.Sintomas$Casos.semana), linetype="dashed", color = "gray80") +
  # geom_hline(yintercept=max(Sonora.Sintomas$Casos.hoy), linetype="dashed", color = "red") +
  geom_line(data=subset(Sonora.Sintomas, Fecha <=dia.act), aes(x= Fecha, y= Casos.media.7d, color= "Tendencia promedio móvil 7 días por inicio de síntomas"), linetype= "solid", size=.55,lineend="round", linejoin="round",  arrow=arrow(type="open", length=unit(0.10,"cm")))+
  geom_point(aes(x= Fecha, y= Casos,fill="Casos que iniciaron síntomas el día correspondiente"), color = "white", size = 0.4, stroke=0.2, shape = 21) +
  scale_fill_manual(name="", values= c("Casos que iniciaron síntomas el día correspondiente" = alpha("#01787E", 0.45))) +
  scale_color_manual(name="", values= c("Tendencia promedio móvil 7 días por inicio de síntomas" = "#01787E", "Tendencia promedio móvil 7 días por reporte" = alpha("red", 0.25))) +
  scale_y_continuous(expand = c(0, 1)) +
  scale_x_date(expand=c(0,0),limits= c(min(Sonora.Sintomas$Fecha)-5,max(Sonora.Sintomas$Fecha)+10), date_breaks = "2 month", 
               labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                            paste(month(x, label = TRUE), "\n", year(x)), 
                                            paste(month(x, label = TRUE)))) + 
  facet_wrap(~label, scales = "free") +
  theme_bw() + temaejes +
  theme(legend.text = element_text(family = "Lato", size = 5), legend.background = element_rect(fill="transparent"), legend.box = "vertical",
        legend.position = c(0.005,0.92),  legend.justification="left", legend.margin=margin(t = 0, unit='cm'), legend.spacing.y = unit(0.000001, "cm"),
        legend.key = element_rect(fill="transparent", color="transparent")) +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = paste0("<span style = 'font-size:14pt'>Covid-19 en Sonora</span><br><span style = 'color:#01A2AC';>Casos confirmados por grupo de edad</span>"), 
       subtitle= Fechahoy, caption =fuentefech)

CasosSon

ggsave("Gráficos diarios/diariocasosedad.png",CasosSon, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)



  
  
Sonora.DF<- read_csv("Bases/ST_SonoraReporte_SSFED.csv", 
                             col_types = cols(fecha_reporte = col_date(format = "%Y-%m-%d")))

Decesos <- Sonora.DF %>% select(fecha_reporte, D_0_14, D_15_29,D_30_59,D_60_mas) %>% 
  select(fecha_reporte, D_0_14, D_15_29,D_30_59,D_60_mas) %>% 
  rename ("0 a 14 años"=D_0_14, "15 a 29 años"=D_15_29,"30 a 59 años"=D_30_59,"60 o más años"=D_60_mas) %>% 
  gather(key= "grupoedad", value= "Decesos", ends_with("años"))

Decesos <- Decesos %>% rename(Fecha = fecha_reporte) %>% group_by(grupoedad) %>% 
  mutate(Decesos.diarios= Decesos - lag(Decesos, default = Decesos[1], order_by=Fecha)) %>% 
  mutate(Decesos.media.repo.7d=round(rollmeanr(x=Decesos.diarios, 7, fill = NA),1))

Decesos.hoy <- Decesos %>% filter(Fecha==max(Fecha)) %>% rename(Decesos.hoy= Decesos.diarios) %>% select(grupoedad, Decesos.hoy)
Decesos.semana <- Decesos %>% filter(Fecha==max(Fecha)-7) %>%  rename (Decesos.semana=Decesos.diarios) %>% select(grupoedad, Decesos.semana)
dia.act <- max(Casos$Fecha) -14
Decesos.repo <- Decesos %>% select(Fecha, grupoedad, Decesos.media.repo.7d)

Decesos.repo$Decesos.media.repo.7d[Decesos.repo$Decesos.media.repo.7d<0] <- 0

Sonora.Def <- read_csv("Bases/ST_SonoraDefuncion_SSFED.csv", 
                            col_types = cols(fecha_def = col_date(format = "%Y-%m-%d"))) %>% rename(Fecha=fecha_def)

Sonora.Def <- Sonora.Def %>% select(Fecha, D_0_14, D_15_29,D_30_59,D_60_mas) %>% 
  select(Fecha, D_0_14, D_15_29,D_30_59,D_60_mas) %>% 
  rename ("0 a 14 años"=D_0_14, "15 a 29 años"=D_15_29,"30 a 59 años"=D_30_59,"60 o más años"=D_60_mas) %>% 
  gather(key= "grupoedad", value= "Decesos", ends_with("años")) 

Sonora.Def <- Sonora.Def %>% 
  group_by(grupoedad) %>% 
  mutate(Decesos.media.7d=round(rollmeanr(x=Decesos, 7, fill = NA),1)) %>% 
  filter(Fecha>as.Date("2020-03-28")) %>% 
  left_join(Decesos.hoy, by="grupoedad") %>% left_join(Decesos.semana, by="grupoedad") %>% 
  left_join(Decesos.repo, by=c("Fecha", "grupoedad"))

Sonora.Defuncion <- Sonora.Def %>%
  mutate(label=paste0(grupoedad, "<br><span style = 'font-size:6pt; font-family:Lato'>",prettyNum(sum(Decesos), big.mark=",", preserve.width="none"), " decesos acumulados, ", prettyNum(Decesos.hoy, big.mark=",", preserve.width="none"), " confirmados hoy</span>"))



DecesosSon <- ggplot(Sonora.Defuncion) +
  geom_line(data=subset(Sonora.Defuncion, Fecha >as.Date("2021-01-01")), aes(x= Fecha, y= Decesos.media.repo.7d, color= "Tendencia promedio móvil 7 días por reporte"), linetype= "solid", lineend="round", linejoin="round", size=.45, arrow=arrow(type="open", length=unit(0.10,"cm")))+
  geom_area(data=subset(Sonora.Defuncion, Fecha <=dia.act), aes( x= Fecha, y= Decesos.media.7d), fill= "#D075A3", alpha=0.3)+
  geom_line(data=subset(Sonora.Defuncion, Fecha <=dia.act), aes(x= Fecha, y= Decesos.media.7d, color= "Tendencia promedio móvil 7 días por fecha de ocurrido"), linetype= "solid", lineend="round", linejoin="round", size=.45, arrow=arrow(type="open", length=unit(0.10,"cm")))+
  geom_point(aes(x= Fecha, y= Decesos,fill="Decesos ocurridos el día correspondiente"), color = "white", size = 0.3, stroke=0.2, shape = 21) +
  scale_fill_manual(name="", values= c("Decesos ocurridos el día correspondiente" = alpha("#73264D",0.65))) + 
  scale_color_manual(name="", values= c("Tendencia promedio móvil 7 días por reporte" = alpha("steelblue", 0.25),"Tendencia promedio móvil 7 días por fecha de ocurrido" = "#73264D")) +
  scale_y_continuous(expand = c(0, 0), breaks = int_breaks_rounded) +
  scale_x_date(expand=c(0,0), date_breaks = "2 month", 
               labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                            paste(month(x, label = TRUE), "\n", year(x)), 
                                            paste(month(x, label = TRUE)))) +
  coord_cartesian(expand = FALSE, clip = 'off') +
  facet_wrap(~label, scales = "free") +
  theme_bw() + temaejes +
  theme(legend.text = element_text(family = "Lato", size = 5), legend.background = element_rect(fill="transparent"), legend.box = "vertical",
        legend.position = c(0.005,0.92),  legend.justification="left", legend.margin=margin(t = 0, unit='cm'), legend.spacing.y = unit(0.000001, "cm"),
        legend.key = element_rect(fill="transparent", color="transparent")) +
  labs(y = NULL, 
       x = NULL,legend= NULL, 
       title  = paste0("<span style = 'font-size:14pt'>Covid-19 en Sonora</span><br><span style = 'color:#993366';>Decesos confirmados por grupo de edad</span>"), 
       subtitle= Fechahoy, caption =fuentefech)

DecesosSon

ggsave("Gráficos diarios/diariodecesosedad.png",DecesosSon,  width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


