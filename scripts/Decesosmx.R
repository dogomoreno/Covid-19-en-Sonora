# Paquetes

library(tidyverse)
library(readxl)
library(janitor)
library(zip)
library(extrafont)
library(showtext)
library(ggtext)
library("Cairo")
library(lubridate)


# Lectura de datos
DecesosNacional <- read_csv("Bases/DecesosNacional.csv", 
                            locale = locale(encoding = "ISO-8859-1"))


## Gráfica de Decesos

# Filtramos decesos  confirmados y sospechosos
mx_decesos <- DecesosNacional %>%  
  select(clasificacion_final_simple, deceso, fecha_def) %>% 
  filter(clasificacion_final_simple=="Caso confirmado", deceso=="Sí") 

ggplot(mx_decesos, aes(fecha_def, deceso)) +
  geom_jitter(aes(color=clasificacion_final_simple), alpha=0.1, width = 0.9, height = 0.5, size=0.3) + # Cada punto es un deceso
  geom_text(aes(x = as.Date("2020-02-15"), y = "Sí",
                label = "Cada punto representa a una\npersona fallecida por covid-19"), stat = "unique", family = "Lato Black", #texto aclaratorio
            size = 10, color = "#993366")+ 
  scale_x_date(expand=c(0,30), date_breaks = "1 month", labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                                                                    paste( year(x)," ",month(x, abbr=FALSE, label = TRUE)), 
                                                                                    paste(month(x, abbr=FALSE, label = TRUE)))) +
  coord_flip()+
  scale_color_manual(values=c("#993366", "#58BCBC"))+ 
  guides(colour = guide_legend(nrow = 1)) + # Leyenda en un renglón 
  theme_minimal() + theme (plot.title = element_markdown(family = "Lato Black", size = 35),  # element_markdown para uso de texto enriquecido en título con ggtext
                           plot.subtitle = element_text(family = "Lato Light", size = 18, color = "gray50"), legend.title = element_blank(),
                           axis.text.y = element_text(family = "Lato", size =14), axis.text.x = element_blank(),plot.title.position = 'plot', plot.caption.position = 'plot',
                           plot.background = element_rect(fill = "white", color = "white", size = 2.5),
                           axis.title.y = element_text(family = "Lato Light", size = 16, hjust=0),
                           axis.title.x = element_text(family = "Lato Light", size =14, hjust=1), 
                           axis.line.y = element_line(arrow = grid::arrow(length = unit(0.3, "cm"))),
                           plot.caption = element_text(family = "Lato", size = 15, color = "black"), panel.grid.major.y = element_line(size=0.4), 
                           panel.grid.major.x = element_blank(), legend.key.height = unit (0.5, "cm"), legend.key.width = unit (0.5, "cm"),
                           legend.text = element_text(family = "Lato", size = 16),legend.position = "none", legend.justification="left",
                           panel.grid.minor= element_blank(), plot.margin = unit(c(0.5,0.5,0.3,0.5), "cm"), legend.margin=margin(t = 0, unit='cm')) +
  labs(y = NULL, 
       x = NULL,legend= NULL, 
       title  = "<span style = 'color:#993366'; 'font-size:40pt'>321,931 mexicanos</span><br><span style = 'font-size:28pt'>con covid-19 confirmado han fallecido</span>", 
       subtitle= "Decesos confirmados con covid-19 por fecha de ocurridos.\nCorte al 18 de marzo de 2022.",
       caption ="Elaboración: Luis Armando Moreno (@dogomoreno)\ncon información de la Secretaría de Salud federal") # ggtext para texto enriquecido en título

ggsave("Gráficos diarios/Decesosmx.png", width = 5 * (16/9), height = 5* (16/9)* (16/9), type = "cairo", dpi = 400)

son_decesos <- DecesosNacional %>%  
  select(entidad_res,clasificacion_final_simple, deceso, fecha_def) %>% 
  filter(clasificacion_final_simple=="Caso confirmado" | clasificacion_final_simple=="Caso sospechoso", deceso=="Sí",entidad_res=="26" ) 

ggplot(son_decesos, aes(fecha_def, deceso)) +
  geom_jitter(aes(color=clasificacion_final_simple), alpha=0.1, width = 0.9, height = 0.5, size=0.3) + # Cada punto es un deceso
  geom_text(aes(x = as.Date("2020-02-15"), y = "Sí",
                label = "Cada punto representa a una\npersona fallecida por covid-19"), stat = "unique", family = "Lato Black", #texto aclaratorio
            size = 10, color = "#993366")+ 
  scale_x_date(expand=c(0,30), date_breaks = "1 month", labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                                                                     paste(month(x, abbr=FALSE, label = TRUE), "\n", year(x)), 
                                                                                     paste(month(x, abbr=FALSE, label = TRUE)))) +
  coord_flip()+
  scale_color_manual(values=c("#993366", "#58BCBC"))+ 
  guides(colour = guide_legend(nrow = 1)) + # Leyenda en un renglón 
  theme_minimal() + theme (plot.title = element_markdown(family = "Lato Black", size = 35),  # element_markdown para uso de texto enriquecido en título con ggtext
                           plot.subtitle = element_text(family = "Lato Light", size = 18, color = "gray50"), legend.title = element_blank(),
                           axis.text.y = element_text(family = "Lato", size =14), axis.text.x = element_blank(),
                           plot.background = element_rect(fill = "white", color = "black", size = 2.5),
                           axis.title.y = element_text(family = "Lato Light", size = 16, hjust=0),
                           axis.title.x = element_text(family = "Lato Light", size =14, hjust=1), 
                           plot.caption = element_text(family = "Lato", size = 15, color = "black"), panel.grid.major.y = element_line(size=0.4), 
                           panel.grid.major.x = element_blank(), legend.key.height = unit (0.5, "cm"), legend.key.width = unit (0.5, "cm"),
                           legend.text = element_text(family = "Lato", size = 16),legend.position = "none", legend.justification="left",
                           panel.grid.minor= element_blank(), plot.margin = unit(c(0.5,0.5,0.3,0.5), "cm"), legend.margin=margin(t = 0, unit='cm')) +
  labs(y = NULL, 
       x = NULL,legend= NULL, 
       title  = "<span style = 'color:#993366'; 'font-size:35pt'>10,131 mexicanos</span><br>han fallecido por Covid-19", 
       subtitle= "Decesos confirmados y sospechosos por fecha de ocurridos.\nCorte al 28 de febrero de 2022.",
       caption ="Elaboración: Luis Armando Moreno (@dogomoreno)\ncon información de la Secretaría de Salud federal") # ggtext para texto enriquecido en título

ggsave("Gráficos diarios/Decesosson.png", width = 5 * (16/9), height = 5* (16/9)* (16/9), type = "cairo", dpi = 300)





son_decesos <- DecesosNacional %>%  
  select(entidad_res,clasificacion_final_simple, deceso, fecha_def) %>% 
  filter(clasificacion_final_simple=="Caso confirmado" | clasificacion_final_simple=="Caso sospechoso", deceso=="Sí",entidad_res=="26" ) 

ggplot(son_decesos, aes(fecha_def, deceso)) +
  geom_jitter(aes(color=clasificacion_final_simple), alpha=0.4, width = 0.9, height = 0.5, size=0.5) + # Cada punto es un deceso
  geom_text(aes(x = as.Date("2020-02-15"), y = "Sí",
                label = "Cada punto\nrepresenta a una\npersona fallecida\npor covid-19"), stat = "unique", family = "Lato Black", #texto aclaratorio
            size = 2.5, color = "#993366")+ 
  # geom_vline(xintercept=as.Date("2020-06-01"), linetype="dashed", color = "red", size=0.5) + # Líneas de cambio de semáforo 
  # geom_vline(xintercept=as.Date("2020-07-20"), linetype="dashed", color = "orange", size=0.5) +
  # geom_vline(xintercept=as.Date("2020-08-31"), linetype="dashed", color = "yellow", size=0.5) +
  # geom_vline(xintercept=as.Date("2020-11-09"), linetype="dashed", color = "orange", size=0.5) +
  # geom_vline(xintercept=as.Date("2021-02-15"), linetype="dashed", color = "yellow", size=0.5) +
  # geom_vline(xintercept=as.Date("2021-03-15"), linetype="dashed", color = "green", size=0.5) +
  # geom_vline(xintercept=as.Date("2021-03-29"), linetype="dashed", color = "yellow", size=0.5) +
  geom_vline(xintercept=as.Date("2021-02-15"), linetype="dashed", color = "green", size=0.5) +
  scale_x_date(date_breaks = "1 month", date_labels = "%B") +
  scale_color_manual(values=c("#993366", "#58BCBC"))+ 
  guides(colour = guide_legend(nrow = 1)) + # Leyenda en un renglón 
  theme_minimal() + theme (plot.title = element_markdown(family = "Lato Black", size = 25),  # element_markdown para uso de texto enriquecido en título con ggtext
                           plot.subtitle = element_text(family = "Lato Light", size = 10, color = "gray50"), legend.title = element_blank(),
                           strip.text = element_text(family = "Lato Black", size = 8),
                           axis.text.x = element_text(family = "Lato", size =7), axis.text.y = element_blank(),
                           plot.background = element_rect(fill = "white", color = "black", size = 2.5),
                           axis.title.x = element_text(family = "Lato Light", size = 8, hjust=0),
                           axis.title.y = element_text(family = "Lato Light", size =7, hjust=1), 
                           plot.caption = element_text(family = "Lato", size = 8, color = "black"), panel.grid.major.x = element_line(size=0.2), 
                           panel.grid.major.y = element_blank(), legend.key.height = unit (0.2, "cm"), legend.key.width = unit (0.2, "cm"),
                           legend.text = element_text(family = "Lato", size = 8),legend.position = "none", legend.justification="left",
                           panel.grid.minor= element_blank(), plot.margin = unit(c(0.5,0.5,0.3,0.5), "cm"), legend.margin=margin(t = 0, unit='cm')) +
  labs(y = NULL, 
       x = NULL,legend= NULL, 
       title  = "<span style = 'color:#993366'; 'font-size:25pt'>10,039 decesos</span> confirmados en Sonora", 
       subtitle= "Decesos confirmados con residencia en la Entidad por fecha de ocurridos.\nCorte al 13 de marzo de 2021.", 
       caption ="Elaboración: Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud federal") # ggtext para texto enriquecido en título

ggsave("03Gráficos/DecesosSonora.png", width = 5 * (16/9), height = 5, type = "cairo", dpi = 300)

son_decesos <- DecesosNacional %>%  
  select(entidad_res,clasificacion_final_simple, deceso, fecha_def, edad) %>% 
  filter(clasificacion_final_simple=="Caso confirmado" | clasificacion_final_simple=="Caso sospechoso", deceso=="Sí",entidad_res=="26" ) 

ggplot(son_decesos, aes(fecha_def, edad)) +
  geom_jitter(aes(color=clasificacion_final_simple), alpha=0.4, width = 0.9, height = 0.5, size=0.3) + # Cada punto es un deceso
  geom_text(aes(x = as.Date("2020-02-15"), y = 55,
                label = "Cada punto\nrepresenta a una\npersona fallecida\npor covid-19"), stat = "unique", family = "Lato Black", #texto aclaratorio
            size = 2.5, color = "#993366")+
  geom_text(aes(x = as.Date("2021-02-20"), y = 115,
                label = "Letalidad después de inicio de vacunación = 3.6% "), stat = "unique", family = "Lato",
            size = 2.5, color = "#b79c2c",  hjust=0)+
  geom_text(aes(x = as.Date("2021-02-10"), y = 115,
                label = "Letalidad antes de inicio de vacunación = 9.7% "), stat = "unique", family = "Lato",
            size = 2.5, color = "gray50",  hjust=1)+
  geom_segment(aes(xend = as.Date("2020-01-15"), y = 115, x = as.Date("2020-07-20"), yend = 115), color= "gray60", alpha=0.1,
               size = 0.5, arrow = arrow(length = unit(0.03, "inches"))) +
  geom_segment(aes(xend = as.Date("2022-03-15"), y = 115, x = as.Date("2021-09-25"), yend = 115), color= "#cbad31", alpha=0.1,
               size = 0.5, arrow = arrow(length = unit(0.03, "inches"))) +
  
  # geom_vline(xintercept=as.Date("2020-06-01"), linetype="dashed", color = "red", size=0.5) + # Líneas de cambio de semáforo 
  # geom_vline(xintercept=as.Date("2020-07-20"), linetype="dashed", color = "orange", size=0.5) +
  # geom_vline(xintercept=as.Date("2020-08-31"), linetype="dashed", color = "yellow", size=0.5) +
  # geom_vline(xintercept=as.Date("2020-11-09"), linetype="dashed", color = "orange", size=0.5) +
  # geom_vline(xintercept=as.Date("2021-02-15"), linetype="dashed", color = "yellow", size=0.5) +
  # geom_vline(xintercept=as.Date("2021-03-15"), linetype="dashed", color = "green", size=0.5) +
  # geom_vline(xintercept=as.Date("2021-03-29"), linetype="dashed", color = "yellow", size=0.5) +
  geom_vline(xintercept=as.Date("2021-02-15"), linetype="dashed", color = "#cbad31", size=0.5) +
  scale_x_date(limits=c(as.Date("2020-01-01"), as.Date("2022-04-01")),expand=c(0,5), date_breaks = "1 month", labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                                                                    paste(month(x, label = TRUE), "\n", year(x)), 
                                                                                    paste(month(x, label = TRUE)))) +
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90,100), labels =c("0","","20","","40","","60","","80","","100 años de edad")) + 
  scale_color_manual(values=c("#993366", "#92CDDC"))+ 
  guides(colour = guide_legend(nrow = 1)) + # Leyenda en un renglón 
  theme_minimal() + theme (plot.title = element_markdown(family = "Lato Black", size = 25),  # element_markdown para uso de texto enriquecido en título con ggtext
                           plot.subtitle = element_text(family = "Lato Light", size = 9, color = "gray50"), legend.title = element_blank(),
                           strip.text = element_text(family = "Lato Black", size = 8),
                           axis.text.x = element_text(family = "Lato", size =7),
                           axis.text.y = element_text(family = "Lato", size =7,hjust=0, vjust = -0.7, margin = margin(l = 10, r = -55), color="gray50"),
                           plot.background = element_rect(fill = "white", color = "white", size = 2.5),
                           axis.title.x = element_text(family = "Lato Light", size = 8, hjust=0),
                           axis.title.y = element_text(family = "Lato Light", size =7, hjust=1), 
                           plot.caption = element_text(family = "Lato", size = 8, color = "gray50"), panel.grid.major = element_line(size=0.2, linetype="dashed"), 
                           legend.text = element_text(family = "Lato", size = 8),legend.position = "none", legend.justification="left",
                           panel.grid.minor= element_blank(), plot.margin = unit(c(0.5,0.5,0.3,0.5), "cm"), legend.margin=margin(t = 0, unit='cm')) +
  labs(y = NULL, 
       x = NULL,legend= NULL, 
       title  = "<span style = 'font-size:12pt'>Covid-19 en Sonora</span><br><span style = 'color:#993366'; 'font-size:25pt'>Han fallecido 10,039 sonorenses<br>con covid confirmado
       </span> y <span style = 'color:#92CDDC'; 'font-size:25pt'>195 con sospecha</span>", 
       subtitle= "Decesos confirmados y sospechos con residencia en la Entidad por edad y fecha de ocurridos.\nCorte al 13 de marzo de 2021.", 
       caption ="Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud federal\nwww.luisarmandomoreno.com") # ggtext para texto enriquecido en título

ggsave("Gráficos diarios/DecesosSonora.png", width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


son_decesos %>% group_by(clasificacion_final_simple) %>% count(deceso)

Sonora.Sintomas <- read_csv("Bases/ST_SonoraSintomas_SSFED.csv", 
                            col_types = cols(fecha_sintomas = col_date(format = "%Y-%m-%d"))) %>% 
  rename(Fecha=fecha_sintomas) %>% 
  filter(Fecha>as.Date("2020-03-03")) %>% 
  mutate(vac=if_else(Fecha>=as.Date("2021-02-15"), "Sí", "No"))

vacsdecesos <- son_decesos %>%  mutate(vac=if_else(fecha_def>=as.Date("2021-02-15"), "Sí", "No")) %>% 
  group_by(vac, clasificacion_final_simple) %>% summarise(decesos=n())
Vacs <- Sonora.Sintomas %>% 
  group_by(vac) %>% summarise(letalidad=sum(Decesos)/sum(Casos))

ggplot(son_decesos, aes(fecha_def, edad)) +
  geom_jitter(aes(color=clasificacion_final_simple), alpha=0.4, width = 0.9, height = 0.5, size=0.35) + # Cada punto es un deceso
  geom_text(aes(x = as.Date("2020-02-15"), y = 55,
                label = "Cada punto representa a una\npersona fallecida por covid-19"), stat = "unique", family = "Lato Black", #texto aclaratorio
            size = 4, color = "#993366")+
  geom_text(aes(x = as.Date("2021-04-15"), y = 2,
                label = "Letalidad después de\ninicio de vacunación\n3.6%"), stat = "unique", family = "Lato",
            size = 2.5, color = "#b79c2c",  hjust=0)+
  geom_text(aes(x = as.Date("2020-12-20"), y = 2,
                label = "Letalidad antes de\ninicio de vacunación\n9.7%"), stat = "unique", family = "Lato",
            size = 2.5, color = "gray50",  hjust=0)+
  geom_segment(aes(xend = as.Date("2020-11-13"), y = 0, x = as.Date("2021-02-01"), yend = 0), color= "gray60", alpha=0.1,
               size = 0.5, arrow = arrow(length = unit(0.03, "inches"))) +
  geom_segment(aes(xend = as.Date("2021-05-17"), y = 0, x = as.Date("2021-03-03"), yend = 0), color= "#cbad31", alpha=0.1,
               size = 0.5, arrow = arrow(length = unit(0.03, "inches"))) +

  # geom_vline(xintercept=as.Date("2020-06-01"), linetype="dashed", color = "red", size=0.5) + # Líneas de cambio de semáforo 
  # geom_vline(xintercept=as.Date("2020-07-20"), linetype="dashed", color = "orange", size=0.5) +
  # geom_vline(xintercept=as.Date("2020-08-31"), linetype="dashed", color = "yellow", size=0.5) +
  # geom_vline(xintercept=as.Date("2020-11-09"), linetype="dashed", color = "orange", size=0.5) +
  # geom_vline(xintercept=as.Date("2021-02-15"), linetype="dashed", color = "yellow", size=0.5) +
  # geom_vline(xintercept=as.Date("2021-03-15"), linetype="dashed", color = "green", size=0.5) +
  # geom_vline(xintercept=as.Date("2021-03-29"), linetype="dashed", color = "yellow", size=0.5) +
  geom_vline(xintercept=as.Date("2021-02-15"), linetype="dashed", color = "#cbad31", size=0.5) +
  scale_x_date(limits=c(as.Date("2020-01-01"), as.Date("2022-04-01")),expand=c(0,5), date_breaks = "1 month", labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                                                                                                                           paste(month(x, label = TRUE), "\n", year(x)), 
                                                                                                                                           paste(month(x, label = TRUE)))) +
  scale_y_continuous(sec.axis = sec_axis(~., breaks = c(0,10,20,30,40,50,60,70,80,90,100), labels =c("0","","20","","40","","60","","80","","100 años de edad")),
                     breaks = c(0,10,20,30,40,50,60,70,80,90,100), labels =c("","","","","","","","","","","")) + 
  scale_color_manual(values=c("#993366", "#92CDDC"))+ 
  coord_flip()+
  theme_minimal() + theme (plot.title = element_markdown(family = "Lato Black", size = 18),  # element_markdown para uso de texto enriquecido en título con ggtext
                           plot.subtitle = element_text(family = "Lato Light", size = 8, color = "gray50"), legend.title = element_blank(),
                           strip.text = element_text(family = "Lato Black", size = 8),
                           axis.text.x = element_text(family = "Lato", size =7),
                           axis.text.y = element_text(family = "Lato", size =7, color="gray70"),
                           plot.background = element_rect(fill = "white", color = "white", size = 2.5),
                           axis.title.x = element_text(family = "Lato Light", size = 8, hjust=0),
                           axis.title.y = element_text(family = "Lato Light", size =7, hjust=1), 
                           plot.caption = element_text(family = "Lato", size = 7, color = "gray50"), panel.grid.major = element_line(size=0.2, linetype="dashed"), 
                           legend.text = element_text(family = "Lato", size = 7),legend.position = "none", legend.justification="left",
                           panel.grid.minor= element_blank(), plot.margin = unit(c(0.5,0.5,0.3,0.5), "cm"), legend.margin=margin(t = 0, unit='cm')) +
  labs(y = NULL, 
       x = NULL,legend= NULL, 
       title  = "<span style = 'font-size:12pt'>Covid-19 en Sonora</span><br><span style = 'color:#993366'; 'font-size:18pt'>Han fallecido 10,008 sonorenses con<br>covid confirmado
       </span> y <span style = 'color:#92CDDC'; 'font-size:18pt'>196 con sospecha</span>", 
       subtitle= "Decesos confirmados y sospechos con residencia en la Entidad por edad y fecha de ocurridos.\nCorte al 10 de marzo de 2021.", 
       caption ="Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud federal\nwww.luisarmandomoreno.com") # ggtext para texto enriquecido en título

ggsave("Gráficos diarios/DecesosSonoraEjes.png", width = 5 , height = 3.5* (16/9), type = "cairo", dpi = 400)

