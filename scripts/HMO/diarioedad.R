rm(list=ls())


# Paquetes

library(tidyverse)
library(extrafont)
library(scales)
library(showtext)
library(units)
library(zoo)
library(lubridate)
library("Cairo")
library(directlabels)
library(ggtext)
library(patchwork)
# Función para ejes en números enteros
int_breaks_rounded <- function(x, n = 5)  pretty(x, n)[round(pretty(x, n),1) %% 1 == 0] 

Casos <- HMO.DF %>% select(fecha_reporte, C_0_14, C_15_29,C_30_59,C_60_mas) %>% 
  select(fecha_reporte, C_0_14, C_15_29,C_30_59,C_60_mas) %>% 
  rename ("0 a 14 años"=C_0_14, "15 a 29 años"=C_15_29,"30 a 59 años"=C_30_59,"60 o más años"=C_60_mas) %>% 
  gather(key= "grupoedad", value= "Casos", ends_with("años"))

Casos <- Casos %>% rename(Fecha = fecha_reporte) %>% group_by(grupoedad) %>% mutate(Casos.diarios= Casos - lag(Casos, default = Casos[1], order_by=Fecha)) 

Casos.hoy <- Casos %>% filter(Fecha==max(Fecha)) %>% rename(Casos.hoy= Casos.diarios) %>% select(grupoedad, Casos.hoy)
Casos.semana <- Casos %>% filter(Fecha==max(Fecha)-7) %>%  rename (Casos.semana=Casos.diarios) %>% select(grupoedad, Casos.semana)
dia.act <- max(Casos$Fecha) -14

temaejes <- theme(plot.margin = margin(10, 25, 10, 25),
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

Fechahoy <- paste0("Al reporte del ", day(dia), " de ", months.Date(dia)," de ", year(dia))
fuente <- "Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud del Gobierno de la República\n*Por fecha de reporte | www.luisarmandomoreno.com"
subtitulo <- paste0("Casos confirmados en los últimos 7 días por 100 mil habitantes\nAl reporte del ",day(dia),"/",month(dia),"/",year(dia))
fuentefech <- "Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud del Gobierno de la República\n*Los últimos 14 días aún se están alimentando en el sistema | www.luisarmandomoreno.com"
POBMUN <- read_csv("Bases/POBMUN.csv", col_types = cols(CVEGEO = col_character()), 
                   locale = locale(encoding = "ISO-8859-1"))

HMO.Sintomas <- read_csv("Bases/ST_HMOSintomas_SSFED.csv", 
                            col_types = cols(fecha_sintomas = col_date(format = "%Y-%m-%d"))) %>% rename(Fecha=fecha_sintomas)

HMO.Sintomas <- HMO.Sintomas %>% select(Fecha, C_0_14, C_15_29,C_30_59,C_60_mas) %>% 
  select(Fecha, C_0_14, C_15_29,C_30_59,C_60_mas) %>% 
  rename ("0 a 14 años"=C_0_14, "15 a 29 años"=C_15_29,"30 a 59 años"=C_30_59,"60 o más años"=C_60_mas) %>% 
  gather(key= "grupoedad", value= "Casos", ends_with("años")) 

HMO.Sintomas <- HMO.Sintomas %>% 
  group_by(grupoedad) %>% 
  mutate(Casos.media.7d=round(rollmeanr(x=Casos, 7, fill = NA),1)) %>% 
  filter(Fecha>as.Date("2020-03-03")) %>% 
  left_join(Casos.hoy, by="grupoedad") %>% left_join(Casos.semana, by="grupoedad") 
  

HMO.Sintomas <- HMO.Sintomas %>%
  mutate(label=paste0(grupoedad, "<br><span style = 'font-size:6pt; font-family:Lato'>",prettyNum(sum(Casos), big.mark=",", preserve.width="none"), " casos acumulados, ", prettyNum(Casos.hoy, big.mark=",", preserve.width="none"), " confirmados hoy</span>"))

# Casos diarios Estatal

CasosHMO <- ggplot(HMO.Sintomas) +
  geom_area(data=subset(HMO.Sintomas, Fecha <= dia.act), aes( x= Fecha, y= Casos.media.7d), fill= "#58BCBC", alpha=0.3)+
  # geom_hline(yintercept=max(HMO.Sintomas$Casos.semana), linetype="dashed", color = "gray80") +
  # geom_hline(yintercept=max(HMO.Sintomas$Casos.hoy), linetype="dashed", color = "red") +
  geom_line(data=subset(HMO.Sintomas, Fecha <=dia.act), aes(x= Fecha, y= Casos.media.7d, color= "Tendencia promedio móvil 7 días"), linetype= "solid", size=.55, arrow=arrow(type="open", length=unit(0.10,"cm")))+
  geom_point(data=subset(HMO.Sintomas, Fecha <=dia.act), aes(x= Fecha, y= Casos,fill="Casos que iniciaron síntomas el día correspondiente"), color = "white", size = 0.5, stroke=0.4, alpha=0.65, shape = 21) +
  geom_area(data=subset(HMO.Sintomas, Fecha >=dia.act), aes( x= Fecha, y= Casos.media.7d), fill= "#58BCBC", alpha=0.1)+
  geom_point(data=subset(HMO.Sintomas, Fecha >dia.act), aes(x= Fecha, y= Casos), fill= "#01787E", color = "white", size = 0.5, stroke=0.4, alpha=0.30, shape = 21) +
  scale_fill_manual(name="", values= c("Tendencia promedio móvil 7 días" = "#58BCBC", "Casos que iniciaron síntomas el día correspondiente" = "#01787E")) +
  scale_color_manual(name="", values= c("Tendencia promedio móvil 7 días" = "#01787E", "Casos diarios" = "white")) +
  scale_y_continuous(expand = c(0, 0), breaks = int_breaks_rounded) +
  scale_x_date(expand=c(0,0),limits= c(min(HMO.Sintomas$Fecha)-5,max(HMO.Sintomas$Fecha)+10), date_breaks = "2 month", 
               labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                            paste(month(x, label = TRUE), "\n", year(x)), 
                                            paste(month(x, label = TRUE)))) + 
  facet_wrap(~label, scales = "free_y") +
  theme_minimal() + temaejes +
  theme(legend.text = element_text(family = "Lato", size = 5), legend.background = element_rect(fill="transparent"), legend.box = "vertical",
        legend.position = c(0.005,0.92),  legend.justification="left", legend.margin=margin(t = 0, unit='cm'), legend.spacing.y = unit(0.000001, "cm"),
        legend.key = element_rect(fill="transparent", color="transparent")) +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = paste0("<span style = 'font-size:14pt'>Covid-19 en Hermosillo</span><br><span style = 'color:#01A2AC';>Casos confirmados por grupo de edad</span>"), 
       subtitle= Fechahoy, caption =fuentefech)

CasosHMO

ggsave("Gráficos diarios/HMO/diariocasosedadHMO.png",CasosHMO, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)



  
  
HMO.DF<- read_csv("Bases/ST_HMOReporte_SSFED.csv", 
                             col_types = cols(fecha_reporte = col_date(format = "%Y-%m-%d")))

Decesos <- HMO.DF %>% select(fecha_reporte, D_0_14, D_15_29,D_30_59,D_60_mas) %>% 
  select(fecha_reporte, D_0_14, D_15_29,D_30_59,D_60_mas) %>% 
  rename ("0 a 14 años"=D_0_14, "15 a 29 años"=D_15_29,"30 a 59 años"=D_30_59,"60 o más años"=D_60_mas) %>% 
  gather(key= "grupoedad", value= "Decesos", ends_with("años"))

Decesos <- Decesos %>% rename(Fecha = fecha_reporte) %>% group_by(grupoedad) %>% mutate(Decesos.diarios= Decesos - lag(Decesos, default = Decesos[1], order_by=Fecha)) 

Decesos.hoy <- Decesos %>% filter(Fecha==max(Fecha)) %>% rename(Decesos.hoy= Decesos.diarios) %>% select(grupoedad, Decesos.hoy)
Decesos.semana <- Decesos %>% filter(Fecha==max(Fecha)-7) %>%  rename (Decesos.semana=Decesos.diarios) %>% select(grupoedad, Decesos.semana)
dia.act <- dia-14


HMO.Def <- read_csv("Bases/ST_HMODefuncion_SSFED.csv", 
                            col_types = cols(fecha_def = col_date(format = "%Y-%m-%d"))) %>% rename(Fecha=fecha_def)

HMO.Def <- HMO.Def %>% select(Fecha, D_0_14, D_15_29,D_30_59,D_60_mas) %>% 
  select(Fecha, D_0_14, D_15_29,D_30_59,D_60_mas) %>% 
  rename ("0 a 14 años"=D_0_14, "15 a 29 años"=D_15_29,"30 a 59 años"=D_30_59,"60 o más años"=D_60_mas) %>% 
  gather(key= "grupoedad", value= "Decesos", ends_with("años")) 

HMO.Def <- HMO.Def %>% 
  group_by(grupoedad) %>% 
  mutate(Decesos.media.7d=round(rollmeanr(x=Decesos, 7, fill = NA),1)) %>% 
  filter(Fecha>as.Date("2020-03-28")) %>% 
  left_join(Decesos.hoy, by="grupoedad") %>% left_join(Decesos.semana, by="grupoedad") 

HMO.Defuncion <- HMO.Def %>%
  mutate(label=paste0(grupoedad, "<br><span style = 'font-size:6pt; font-family:Lato'>",prettyNum(sum(Decesos), big.mark=",", preserve.width="none"), " decesos acumulados, ", prettyNum(Decesos.hoy, big.mark=",", preserve.width="none"), " confirmados hoy</span>"))



DecesosHMO <- ggplot(HMO.Defuncion) +
  geom_area(data=subset(HMO.Defuncion, Fecha <=dia.act), aes( x= Fecha, y= Decesos.media.7d), fill= "#D075A3", alpha=0.3)+
  geom_line(data=subset(HMO.Defuncion, Fecha <=dia.act), aes(x= Fecha, y= Decesos.media.7d, color= "Tendencia promedio móvil 7 días"), linetype= "solid", size=.45, arrow=arrow(type="open", length=unit(0.10,"cm")))+
  geom_point(data=subset(HMO.Defuncion, Fecha <=dia.act), aes(x= Fecha, y= Decesos,fill="Decesos ocurridos el día correspondiente"), color = "white", size = 0.4, stroke=0.4, alpha=0.65, shape = 21) +
  geom_area(data=subset(HMO.Defuncion, Fecha >=dia.act), aes( x= Fecha, y= Decesos.media.7d), fill= "#D075A3", alpha=0.15)+
  geom_point(data=subset(HMO.Defuncion, Fecha >dia.act), aes(x= Fecha, y= Decesos), fill= "#73264D", color = "white", size = 0.4, stroke=0.4, alpha=0.20, shape = 21) +
  scale_color_manual(name="", values= c("Decesos diarios" = "white","Tendencia promedio móvil 7 días" = "#73264D")) +
  scale_fill_manual(name="", values= c("Decesos ocurridos el día correspondiente" = "#73264D", "Tendencia promedio móvil 7 días" = "#D075A3")) + 
  scale_y_continuous(expand = c(0, 0), breaks = int_breaks_rounded) +
  scale_x_date(expand=c(0,0), date_breaks = "2 month", 
               labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                            paste(month(x, label = TRUE), "\n", year(x)), 
                                            paste(month(x, label = TRUE)))) +
  coord_cartesian(expand = FALSE, clip = 'off') +
  facet_wrap(~label, scales = "free_y") +
  theme_minimal() + temaejes +
  theme(legend.text = element_text(family = "Lato", size = 5), legend.background = element_rect(fill="transparent"), legend.box = "vertical",
        legend.position = c(0.005,0.92),  legend.justification="left", legend.margin=margin(t = 0, unit='cm'), legend.spacing.y = unit(0.000001, "cm"),
        legend.key = element_rect(fill="transparent", color="transparent")) +
  labs(y = NULL, 
       x = NULL,legend= NULL, 
       title  = paste0("<span style = 'font-size:14pt'>Covid-19 en Hermosillo</span><br><span style = 'color:#993366';>Decesos confirmados por grupo de edad</span>"), 
       subtitle= Fechahoy, caption =fuentefech)

DecesosHMO

ggsave("Gráficos diarios/HMO/diariodecesosedadHMO.png",DecesosHMO, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


