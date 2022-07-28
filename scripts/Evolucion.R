rm(list=ls())

# Paquetes

library(tidyverse)
library(extrafont)
library(scales)
library(zoo)
library(lubridate)
library("Cairo")
library(directlabels)
library(ggtext)
library(geofacet)


Sonora.DF <- read_csv("Bases/ST_SonoraReporte_SSFED.csv", 
                      col_types = cols(X1 = col_skip(), 
                                       fecha_reporte = col_date(format = "%Y-%m-%d"))) %>% rename(Fecha=fecha_reporte)
dia<-max(as.Date(Sonora.DF$Fecha))
lundom <- weekdays(dia)


Sonora.Defuncion <- read_csv("Bases/ST_SonoraDefuncion_SSFED.csv", 
                             col_types = cols(fecha_def = col_date(format = "%Y-%m-%d"))) %>% rename(Fecha=fecha_def) %>% filter(Fecha>as.Date("2020-03-03"))
Sonora.Sintomas <- read_csv("Bases/ST_SonoraSintomas_SSFED.csv", 
                            col_types = cols(fecha_sintomas = col_date(format = "%Y-%m-%d"))) %>% rename(Fecha=fecha_sintomas) %>% filter(Fecha>as.Date("2020-03-03"))

Sonora.Ingreso <- read_csv("Bases/ST_SonoraIngreso_SSFED.csv", 
                           col_types = cols(fecha_ingreso = col_date(format = "%Y-%m-%d"))) %>% rename(Fecha=fecha_ingreso) %>% filter(Fecha>as.Date("2020-03-03"))

dia.ev <- dia-14
dia.act <- max(as.Date(Sonora.DF$Fecha))-14

lundom <- weekdays(dia)
Fechasem <- paste0("Al reporte del ", day(dia), " de ", months.Date(dia)," de ", year(dia)," | Confirmados acumulados de ",weekdays(dia+1)," a ", weekdays(dia))
Fechaobs <- paste0("Al reporte del ", day(dia), " de ", months.Date(dia)," de ", year(dia)," | Ingresados acumulados de ",weekdays(dia+1)," a ", weekdays(dia))
Fechadom <- paste0("Al reporte del ", day(dia), " de ", months.Date(dia)," de ", year(dia),"  | Cifras al ", weekdays(dia)," de cada semana.")
Fechahoy <- paste0("Al reporte del ", day(dia), " de ", months.Date(dia)," de ", year(dia))
fuente <- "Elaboración Luis Armando Moreno con información de la Secretaría de Salud del Gobierno de la República\n*Por fecha de reporte. | www.luisarmandomoreno.com"
fuentefech <- paste0("Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud del Gobierno de la República\n*",Fechahoy,", el registro de las últimas 2 semanas aún se encuentra en proceso.| www.luisarmandomoreno.com")
fuentedes <- "Elaboración Luis Armando Moreno con información de la Secretaría de Salud del Gobierno de la República\n* www.luisarmandomoreno.com"
fuenteedad <- "Elaboración Luis Armando Moreno con información de la Secretaría de Salud del Gobierno de la República\n*Por fecha de reporte, negativos por ajustes. | www.luisarmandomoreno.com"
fuentepruebas <- "Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud del Gobierno de la República\n*Incluye resultados válidos de pruebas PCR y antigénicas | www.luisarmandomoreno.com"
fuenteingreso<- "Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud del Gobierno de la República\n*El registro de las última semana aún se encuentra en proceso.| www.luisarmandomoreno.com"
temaejes <- theme(plot.margin = margin(10, 25, 10, 25),
                  plot.title = element_markdown(family = "Lato Black", size =  46), 
                  panel.grid=element_blank(), panel.border=element_blank(), 
                  axis.line= element_line(color = "black", size = 0.3), 
                  plot.subtitle = element_text(family = "Lato Light", size = 10, color = "black"), legend.title = element_blank(),
                  strip.text = element_text(family = "Lato Black", size = 10),
                  axis.text = element_text(family = "Lato", size =6),
                  plot.background = element_rect(fill = "white", color = "white", size = 3),
                  axis.title.x = element_text(family = "Lato Light", size = 8, hjust=1),
                  axis.title.y = element_text(family = "Lato Light", size = 8, hjust=1), 
                  plot.caption = element_text(family = "Lato", size = 14),
                  legend.text = element_blank(),
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
                      legend.position = "none",  legend.justification="left", plot.title.position = 'plot', plot.caption.position = 'plot')

temadiario <- theme(plot.margin = margin(10, 25, 10, 25),
                    plot.title = element_markdown(family = "Lato Black", size = 25), 
                    panel.grid=element_blank(), panel.border=element_blank(), axis.line= element_line(color = "black", size = 0.3),
                    plot.subtitle = element_text(family = "Lato Light", size = 10, color = "black"), legend.title = element_blank(),
                    strip.text = element_text(family = "Lato Black", size = 8, hjust=0),
                    axis.text = element_text(family = "Lato", size =6),
                    plot.background = element_rect(fill = "white", color = "white", size = 3),
                    axis.title.x = element_text(family = "Lato Light", size = 12, hjust=1),
                    axis.title.y = element_text(family = "Lato Light", size = 12, hjust=1), 
                    plot.caption = element_text(family = "Lato", size = 6),
                    legend.text = element_blank(), legend.background =  element_rect(fill="transparent", color="transparent"),
                    legend.box.background=  element_rect(fill="transparent", color="transparent" ), 
                    legend.key = element_rect(fill="transparent", color="transparent"),
                    legend.position = "none", plot.title.position = 'plot', plot.caption.position = 'plot')

# Gráfico estatal

Fechas <- data.frame(Fecha=as.Date(Sonora.DF$Fecha)) 

Sintomassemanas <- Sonora.Sintomas %>% 
  full_join(Fechas, by="Fecha") %>%
  mutate_if(is.numeric,coalesce,0) %>% mutate(Casos.semana=rollsum(Casos, 7, align="right", fill = 0),
                                              Decesos.semana=rollsum(Decesos, 7, align="right", fill = 0),
                                              C_0_14.semana=rollsum(C_0_14, 7, align="right", fill = 0),
                                              C_15_29.semana=rollsum(C_15_29, 7, align="right", fill = 0),
                                              C_30_59.semana=rollsum(C_30_59, 7, align="right", fill = 0),
                                              C_60_mas.semana=rollsum(C_60_mas, 7, align="right", fill = 0),
                                              D_0_14.semana=rollsum(D_0_14, 7, align="right", fill = 0),
                                              D_15_29.semana=rollsum(D_15_29, 7, align="right", fill = 0),
                                              D_30_59.semana=rollsum(D_30_59, 7, align="right", fill = 0),
                                              D_60_mas.semana=rollsum(D_60_mas, 7, align="right", fill = 0)) %>% 
  select(Fecha, Casos.semana, Decesos.semana, C_0_14.semana, C_15_29.semana, C_30_59.semana, 
         C_60_mas.semana, D_0_14.semana, D_15_29.semana, D_30_59.semana, D_60_mas.semana) %>% 
  mutate(diasemana = weekdays(Fecha)) %>% 
  filter(diasemana==lundom) 
Casossemanas <- Sintomassemanas %>% select(Fecha, Casos.semana)

Decesossemanas <- Sonora.Defuncion %>%
  mutate_if(is.numeric,coalesce,0) %>% mutate(Decesos.semana=rollsum(Decesos, 7, align="right", fill = 0),
  ) %>% 
  select(Fecha, Decesos.semana) %>% 
  mutate(diasemana = weekdays(Fecha)) %>% 
  filter(diasemana==lundom)

Hospitalizadossemanas <- Sonora.Ingreso %>%  
  mutate_if(is.numeric,coalesce,0) %>% mutate(Hospitalizados.semana=rollsum(Hospitalizados, 7, align="right", fill = 0)) %>% 
  select(Fecha, Hospitalizados.semana) %>%
  mutate(diasemana = weekdays(Fecha)) %>% 
  filter(diasemana==lundom)




Evolucion <- Casossemanas %>% left_join(Decesossemanas, by="Fecha") %>% left_join(Hospitalizadossemanas, by="Fecha") %>% mutate_if(is.numeric,coalesce,0)
Evoprev <-Evolucion %>% filter(Fecha<as.Date("2022-01-01"))
casosmax <- max(Evoprev$Casos.semana)
decesosmax <- max(Evoprev$Decesos.semana)
hospitalizadosmax <- max(Evoprev$Hospitalizados.semana)
Evolucion <- Evolucion %>% mutate( Casos=round(Casos.semana/casosmax,3), Decesos=round(Decesos.semana/decesosmax,3), Hospitalizados=round(Hospitalizados.semana/hospitalizadosmax,3))


CasosSonrelativa <- ggplot() +
  geom_hline(yintercept=1, linetype="dashed", color = "black", size=0.2) +
  geom_vline(xintercept=as.Date("2021-02-15"), linetype="solid", color = alpha("#D9C264",0.8), size=0.5, ) +
  annotate("rect", xmin = as.Date("2021-02-15"), xmax = max(Sonora.Sintomas$Fecha)+90, ymin = 0, ymax = max(Evolucion$Casos)+.2,
           alpha = .07,fill = "#D9C264") +
  geom_line(data=subset(Evolucion, Fecha <=dia.ev),aes( x= Fecha, y= Decesos),color="#73264D", lineend="round", linejoin="round",linetype= "solid", size=.7, alpha=0.7) +
  geom_line(data=subset(Evolucion, Fecha <=dia.ev),aes( x= Fecha, y= Hospitalizados),color="#F79646", lineend="round", linejoin="round",linetype= "solid", size=.7, alpha=0.7) +
  geom_line(data=subset(Evolucion, Fecha <=dia.ev),aes( x= Fecha, y= Casos),color="#01A2AC", lineend="round", linejoin="round",linetype= "solid", size=.7) +
  geom_point(data=subset(Evolucion, Fecha ==dia.ev),aes( x= Fecha, y= Decesos),color="#73264D",  size=1) +
  geom_point(data=subset(Evolucion, Fecha ==dia.ev),aes( x= Fecha, y= Hospitalizados),color="#F79646", size=1) +
  geom_point(data=subset(Evolucion, Fecha ==dia.ev),aes( x= Fecha, y= Casos),color="#01A2AC", size=1) +
  geom_point(data=subset(Evolucion, Casos ==max(Casos)),aes( x= Fecha, y= Casos),color="#01A2AC", size=1) +
  geom_dl( data=subset(Evolucion, Fecha ==dia.ev), aes(x= Fecha, y= Decesos, label=paste0(Decesos*100,"%", "; ",Decesos.semana,"\n")), color="#993366", 
           method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.6, fontfamily= "Lato Black")) +
  geom_dl( data=subset(Evolucion, Fecha ==dia.ev), aes(x= Fecha, y= Hospitalizados,  label =paste0(Hospitalizados*100,"%", "; ",Hospitalizados.semana)), color="#F79646", 
           method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.6, fontfamily= "Lato Black")) +
  geom_dl( data=subset(Evolucion, Fecha ==dia.ev), aes(x= Fecha, y= Casos,  label = paste0(Casos*100,"%", "; ",Casos.semana)), color="#01A2AC", 
           method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.6, fontfamily= "Lato Black")) +
  geom_text(aes(x = as.Date("2021-02-19"), y = max(Evolucion$Casos),
                label = "Campaña de vacunación en Sonora"), stat = "unique", family = "Lato",
            size = 3.5, color = alpha("#D9C264",0.8),  hjust=0)+
  annotate("segment",
           x = as.Date("2021-02-15"), xend = as.Date("2021-10-15"), 
           y = max(Evolucion$Casos)-.1, yend = max(Evolucion$Casos)-.1,
           color = alpha("#D9C264",0.5), size = 0.4,arrow = arrow(length = unit(.1,"cm"))) +
  geom_text(aes(x = as.Date("2020-03-05"), y = 1.8,
                label = paste0("Casos por semana de inicio de síntomas\n(100% = ",casosmax,")")), stat = "unique", family = "Lato Black",
            size = 2.7, color = "#01A2AC",  hjust=0)+
  geom_text(aes(x = as.Date("2022-01-17"), y = 2.9,
                label = paste0("Pico ómicron\n",max(Evolucion$Casos)*100,"%","; ",max(Evolucion$Casos.semana))), stat = "unique", family = "Lato Black",
            size = 2.7, color = "#01A2AC",  hjust=0)+
  geom_text(aes(x = as.Date("2020-03-05"), y = 1.5,
                label = paste0("Hospitalizados por semana de ingreso\n(100% = ",hospitalizadosmax,")")), stat = "unique", family = "Lato Black",
            size = 2.7, color = "#F79646",  hjust=0)+
  geom_text(aes(x = as.Date("2020-03-05"), y = 1.2,
                label = paste0("Decesos por semana de ocurridos\n(100% = ",decesosmax,")")), stat = "unique", family = "Lato Black",
            size = 2.7, color = "#993366",  hjust=0)+
  geom_text(aes(x = as.Date("2021-02-19"), y = 0.75,
                label = "Desde el inicio de la vacunación\nninguna semana ha alcanzado\nal menos la mitad del máximo\nen decesos y hospitalizaciones"), 
            stat = "unique", family = "Lato Black",
            size = 2.2, color = "#D9C264",  hjust=0)+
  scale_y_continuous(expand = c(0, 0), limits= c(0,max(Evolucion$Casos)+.2), labels = scales::percent_format(scale = 100), position = "left") +
  scale_x_date(expand=c(0,0),limits= c(min(Sonora.Sintomas$Fecha)-5,max(Sonora.Sintomas$Fecha)+90), date_breaks = "1 month", 
               labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                            paste(month(x, label = TRUE), "\n", year(x)), 
                                            paste(month(x, label = TRUE)))) +  theme_linedraw()+ temaejes +
  theme(legend.text = element_text(family = "Lato", size = 8), legend.background = element_rect(fill="transparent"), legend.box = "horizontal",
        axis.title.y = element_markdown(family = "Lato Light", size = 8, hjust=1),plot.subtitle = element_markdown(family = "Lato Light", size = 10, color = "black"),
        panel.grid.major = element_line(linetype = "dashed", color="gray93", size=.2), panel.background = element_rect(color="transparent"),
        axis.line.y = element_blank(), axis.ticks.y = element_blank(),panel.grid.minor.y = element_line(linetype = "dashed", color="gray93", size=.2),
        legend.position = c(0.02,0.95),  legend.justification="left", legend.margin=margin(t = 0, unit='cm'),
        legend.key = element_rect(fill="transparent", color="transparent")) +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Sonora</span><br>Evolución relativa de los casos confirmados", 
       subtitle= paste0("Respecto al máximo semanal previo a 2022.<br>", Fechahoy, " con corte al ", day(dia.ev)," de ",months.Date(dia.ev)," de ",year(dia.ev),"*"),
       caption =fuentefech)
CasosSonrelativa 

ggsave("Gráficos semanales/evolucionrelativa.png",CasosSonrelativa, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


Confirmados <- read_csv("Bases/ConfirmadosNacional.csv", 
                                locale = locale(encoding = "ISO-8859-1"), 
                                na = "NA")

Confirmados[is.na(Confirmados)] <- 0

Confirmados <- Confirmados %>% group_by(entidad_res, entidad_residencia) %>% 
  mutate(Casos.semana=rollsum(Casos_sintomas, 7, align="right", fill = 0),
         Decesos.semana.sint=rollsum(Decesos_sintomas, 7, align="right", fill = 0),
         Hospitalizados.semana.sint=rollsum(Hospitalizados_sintomas, 7, align="right", fill = 0),
         Decesos.semana=rollsum(Decesos_ocurridos, 7, align="right", fill = 0),
         Hospitalizados.semana=rollsum(Hospitalizados_ingresos, 7, align="right", fill = 0)) %>% 
    mutate(diasemana = weekdays(Fecha)) %>% 
    filter(diasemana==lundom) %>% select(Fecha, entidad_res,entidad_residencia, Casos.semana, Decesos.semana.sint, Decesos.semana, Hospitalizados.semana.sint, Hospitalizados.semana)

Casosprev2022<- Confirmados %>%
  select(entidad_res, entidad_residencia, Fecha, Casos.semana) %>%
  filter(Fecha<as.Date("2021-10-01")) %>% group_by(entidad_res,entidad_residencia) %>% 
  filter(Casos.semana ==max(Casos.semana)) %>% rename(Casos.max=Casos.semana) %>% 
  select(entidad_res,entidad_residencia,Casos.max)
Decesosprev2022<- Confirmados %>%
  select(entidad_res, entidad_residencia, Fecha, Decesos.semana) %>%
  filter(Fecha<as.Date("2021-10-01")) %>% group_by(entidad_res,entidad_residencia) %>% 
  filter(Decesos.semana ==max(Decesos.semana)) %>% rename(Decesos.max=Decesos.semana) %>% 
  select(entidad_res,entidad_residencia,Decesos.max)
Hospitalizadosprev2022<- Confirmados %>%
  select(entidad_res, entidad_residencia, Fecha, Hospitalizados.semana) %>%
  filter(Fecha<as.Date("2021-10-01")) %>% group_by(entidad_res,entidad_residencia) %>% 
  filter(Hospitalizados.semana ==max(Hospitalizados.semana)) %>% rename(Hospitalizados.max=Hospitalizados.semana) %>% 
  select(entidad_res,entidad_residencia,Hospitalizados.max)

Confirmados <- Confirmados %>% left_join(Casosprev2022, by=c("entidad_res", "entidad_residencia")) %>% 
  left_join(Decesosprev2022, by=c("entidad_res", "entidad_residencia")) %>%
  left_join(Hospitalizadosprev2022, by=c("entidad_res", "entidad_residencia")) 

Confirmadosrelativo <- Confirmados %>% mutate (Casos=round(Casos.semana/Casos.max,3),
                                               Decesos=round(Decesos.semana/Decesos.max,3),
                                               Hospitalizados=round(Hospitalizados.semana/Hospitalizados.max,3))

Entidad<- "Tabasco"
Entidadrelativa <- Confirmadosrelativo %>% filter(entidad_residencia==Entidad)

CasosSonrelativa <- ggplot() +
  geom_hline(yintercept=1, linetype="dashed", color = "black", size=0.2) +
  # geom_vline(xintercept=as.Date("2021-02-15"), linetype="solid", color = alpha("#D9C264",0.8), size=0.5, ) +
  # annotate("rect", xmin = as.Date("2021-02-15"), xmax = max(Sonora.Sintomas$Fecha)+90, ymin = 0, ymax = max(Evolucion$Casos)+.2,
  #          alpha = .07,fill = "#D9C264") +
  geom_line(data=subset(Entidadrelativa, Fecha <=dia.ev),aes( x= Fecha, y= Decesos),color="#73264D", lineend="round", linejoin="round",linetype= "solid", size=.7, alpha=0.7) +
  geom_line(data=subset(Entidadrelativa, Fecha <=dia.ev),aes( x= Fecha, y= Hospitalizados),color="#F79646", lineend="round", linejoin="round",linetype= "solid", size=.7, alpha=0.7) +
  geom_line(data=subset(Entidadrelativa, Fecha <=dia.ev),aes( x= Fecha, y= Casos),color="#01A2AC", lineend="round", linejoin="round",linetype= "solid", size=.7) +
  geom_point(data=subset(Entidadrelativa, Fecha ==dia.ev),aes( x= Fecha, y= Decesos),color="#73264D",  size=1) +
  geom_point(data=subset(Entidadrelativa, Fecha ==dia.ev),aes( x= Fecha, y= Hospitalizados),color="#F79646", size=1) +
  geom_point(data=subset(Entidadrelativa, Fecha ==dia.ev),aes( x= Fecha, y= Casos),color="#01A2AC", size=1) +
  geom_point(data=subset(Entidadrelativa, Casos ==max(Casos)),aes( x= Fecha, y= Casos),color="#01A2AC", size=1) +
  geom_dl( data=subset(Entidadrelativa, Fecha ==dia.ev), aes(x= Fecha, y= Decesos, label=paste0(Decesos*100,"%", "; ",Decesos.semana)), color="#993366", 
           method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.6, fontfamily= "Lato Black")) +
  geom_dl( data=subset(Entidadrelativa, Fecha ==dia.ev), aes(x= Fecha, y= Hospitalizados,  label =paste0(Hospitalizados*100,"%", "; ",Hospitalizados.semana)), color="#F79646", 
           method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.6, fontfamily= "Lato Black")) +
  geom_dl( data=subset(Entidadrelativa, Fecha ==dia.ev), aes(x= Fecha, y= Casos,  label = paste0(Casos*100,"%", "; ",Casos.semana)), color="#01A2AC", 
           method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.6, fontfamily= "Lato Black")) +
  # geom_text(aes(x = as.Date("2021-02-19"), y = max(Entidadrelativa$Casos),
  #               label = "Campaña de vacunación en Sonora"), stat = "unique", family = "Lato",
  #           size = 3.5, color = alpha("#D9C264",0.8),  hjust=0)+
  # annotate("segment",
  #          x = as.Date("2021-02-15"), xend = as.Date("2021-10-15"), 
  #          y = max(Evolucion$Casos)-.1, yend = max(Evolucion$Casos)-.1,
  #          color = alpha("#D9C264",0.5), size = 0.4,arrow = arrow(length = unit(.1,"cm"))) +
  geom_text(data=subset(Entidadrelativa, Casos ==max(Casos)),aes(x = min(Entidadrelativa$Fecha)+2, y = 2,
                label = paste0("Casos por semana de inicio de síntomas\n(100% = ",Casos.max,")")), stat = "unique", family = "Lato Black",
            size = 2.7, color = "#01A2AC",  hjust=0)+
  # geom_text(aes(x = as.Date("2022-01-17"), y = 2.9,
  #               label = paste0("Pico ómicron\n",max(Evolucion$Casos)*100,"%","; ",max(Evolucion$Casos.semana))), stat = "unique", family = "Lato Black",
  #           size = 2.7, color = "#01A2AC",  hjust=0)+
  geom_text(data=subset(Entidadrelativa, Casos ==max(Casos)),aes(x = min(Entidadrelativa$Fecha)+2, y = 1.6,
                label = paste0("Hospitalizados por semana de ingreso\n(100% = ",Hospitalizados.max,")")), stat = "unique", family = "Lato Black",
            size = 2.7, color = "#F79646",  hjust=0)+
  geom_text(data=subset(Entidadrelativa, Casos ==max(Casos)), aes(x = min(Entidadrelativa$Fecha)+2, y = 1.2,
                label = paste0("Decesos por semana de ocurridos\n(100% = ",Decesos.max,")")), stat = "unique", family = "Lato Black",
            size = 2.7, color = "#993366",  hjust=0)+
  # geom_text(aes(x = as.Date("2021-02-19"), y = 0.75,
  #               label = "Desde el inicio de la vacunación\nninguna semana ha alcanzado\nal menos la mitad del máximo\nen decesos y hospitalizaciones"), 
  #           stat = "unique", family = "Lato Black",
  #           size = 2.2, color = "#D9C264",  hjust=0)+
  scale_y_continuous(expand = c(0, 0), limits= c(0,max(Entidadrelativa$Casos)+.2), labels = scales::percent_format(scale = 100), position = "left") +
  scale_x_date(expand=c(0,0),limits= c(min(Entidadrelativa$Fecha)-5,max(Entidadrelativa$Fecha)+90), date_breaks = "1 month", 
               labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                            paste(month(x, label = TRUE), "\n", year(x)), 
                                            paste(month(x, label = TRUE)))) +  theme_linedraw()+ temaejes +
  theme(legend.text = element_text(family = "Lato", size = 8), legend.background = element_rect(fill="transparent"), legend.box = "horizontal",
        axis.title.y = element_markdown(family = "Lato Light", size = 8, hjust=1),plot.subtitle = element_markdown(family = "Lato Light", size = 10, color = "black"),
        panel.grid.major = element_line(linetype = "dashed", color="gray93", size=.2), panel.background = element_rect(color="transparent"),
        axis.line.y = element_blank(), axis.ticks.y = element_blank(),panel.grid.minor.y = element_line(linetype = "dashed", color="gray93", size=.2),
        legend.position = c(0.02,0.95),  legend.justification="left", legend.margin=margin(t = 0, unit='cm'),
        legend.key = element_rect(fill="transparent", color="transparent")) +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = paste0("<span style = 'font-size:14pt'>Covid-19 en ",Entidad,"</span><br>Evolución relativa de los casos confirmados"), 
       subtitle= paste0("Respecto al máximo semanal previo a octubre 2021.<br>", Fechahoy, " con corte al ", day(dia.ev)," de ",months.Date(dia.ev)," de ",year(dia.ev),"*"),
       caption =fuentefech)
CasosSonrelativa 

ggsave(paste0("Gráficos semanales/evolucionrelativa",Entidad,".png"),CasosSonrelativa, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)



Entidadrelativa <- Confirmadosrelativo %>% mutate(code=as.character(as.double(entidad_res)),
                                                  name=entidad_residencia) %>% 
    filter(Fecha>=as.Date("2021-11-01")) %>% 
  group_by(entidad_res, entidad_residencia) %>% 
  mutate(Casosacum=sum(Casos.semana), Decesosacum=sum(Decesos.semana), Hospitalizadosacum=sum(Hospitalizados.semana), 
         clasf=if_else(max(Hospitalizados)>=.7, "A", "B"))

Casosnalrelativa <- ggplot(Entidadrelativa) +
  geom_hline(yintercept=1, linetype="dashed", color = "black", size=0.2) +
  # geom_rect(data=subset(Entidadrelativa, clasf =="A"),xmin = as.Date("2021-11-20"), xmax = as.Date(max(Entidadrelativa$Fecha)), ymin = 0, ymax = 6,
  #            fill=alpha("#FABF8F",0.3), stat="unique") +
  # geom_vline(xintercept=as.Date("2021-02-15"), linetype="solid", color = alpha("#D9C264",0.8), size=0.5, ) +
  # annotate("rect", xmin = as.Date("2021-02-15"), xmax = max(Sonora.Sintomas$Fecha)+90, ymin = 0, ymax = max(Evolucion$Casos)+.2,
  #          alpha = .07,fill = "#D9C264") +
  geom_line(data=subset(Entidadrelativa, Fecha <=dia.ev),aes( x= Fecha, y= Decesos,color="<span style = 'color:#993366';>Decesos por semana de ocurridos</span>"), lineend="round", linejoin="round",linetype= "solid", size=.7, alpha=0.7) +
  geom_line(data=subset(Entidadrelativa, Fecha <=dia.ev),aes( x= Fecha, y= Hospitalizados,color="<span style = ';color:#F79646';>Hospitalizados por semana de ingreso</span>"),color="#F79646", lineend="round", linejoin="round",linetype= "solid", size=.7, alpha=0.7) +
  geom_line(data=subset(Entidadrelativa, Fecha <=dia.ev),aes( x= Fecha, y= Casos, color="<span style = 'color:#01A2AC';>Casos por semana de inicio de síntomas</span>"), lineend="round", linejoin="round",linetype= "solid", size=.7) +
  geom_point(data=subset(Entidadrelativa, Fecha ==dia.ev),aes( x= Fecha, y= Decesos),color="#73264D",  size=1) +
  geom_point(data=subset(Entidadrelativa, Fecha ==dia.ev),aes( x= Fecha, y= Hospitalizados),color="#F79646", size=1) +
  geom_point(data=subset(Entidadrelativa, Fecha ==dia.ev),aes( x= Fecha, y= Casos),color="#01A2AC", size=1) +
  geom_point(data=subset(Entidadrelativa, Casos ==max(Casos)),aes( x= Fecha, y= Casos),color="#01A2AC", size=1) +
  # geom_dl( data=subset(Entidadrelativa, Fecha ==dia.ev), aes(x= Fecha, y= Decesos, label=paste0(Decesos*100,"%", "; ",Decesos.semana)), color="#993366", 
  #          method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.6, fontfamily= "Lato Black")) +
  # geom_dl( data=subset(Entidadrelativa, Fecha ==dia.ev), aes(x= Fecha, y= Hospitalizados,  label =paste0(Hospitalizados*100,"%", "; ",Hospitalizados.semana)), color="#F79646", 
  #          method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.6, fontfamily= "Lato Black")) +
  # geom_dl( data=subset(Entidadrelativa, Fecha ==dia.ev), aes(x= Fecha, y= Casos,  label = paste0(Casos*100,"%", "; ",Casos.semana)), color="#01A2AC", 
  #          method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.6, fontfamily= "Lato Black")) +
  facet_geo(~ code, 
            grid = mx_state_grid2, 
            label = "name",
            scales = "fixed") +
  # geom_text(aes(x = as.Date("2021-11-20")+2, y = 5,
  #                                                                label = "Acumulados 4ta. ola:"), stat = "unique", family = "Lato",
  #           size = 2.5, color = "black",  hjust=0)+
  geom_text(aes(x = as.Date("2021-11-20")+2, y = 5,
                                                                 label = prettyNum(Casosacum,big.mark=",",scientific=FALSE)), stat = "unique", family = "Lato Black",
            size = 3.8, color = "#01A2AC",  hjust=0)+
  geom_text(aes(x = as.Date("2021-11-20")+2, y = 3.5,
                                                                 label = prettyNum(Hospitalizadosacum,big.mark=",",scientific=FALSE)), stat = "unique", family = "Lato Black",
            size = 3.8, color = "#F79646",  hjust=0)+
  geom_text(aes(x = as.Date("2021-11-20")+2, y = 2,
                                                                  label = prettyNum(Decesosacum,big.mark=",",scientific=FALSE)), stat = "unique", family = "Lato Black",
            size = 3.8, color = "#993366",  hjust=0)+
  scale_y_continuous(expand = c(0, 0), limits= c(0,6),breaks = c(1,3,5), labels = c("100%"," ","500%"), position = "left") +
  scale_x_date(expand=c(0,0),limits= c(as.Date("2021-11-20"),max(Entidadrelativa$Fecha)), date_breaks = "1 month", 
               labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                            paste(month(x, label = TRUE), "\n", year(x)), 
                                            paste(month(x, label = TRUE)))) +
  scale_color_manual(values=c("<span style = 'color:#01A2AC';>Casos por semana de inicio de síntomas</span>"="#01A2AC", 
                              "<span style = 'color:#F79646';>Hospitalizados por semana de ingreso</span>"="#F79646",
                              "<span style = 'color:#993366';>Decesos por semana de ocurridos</span>"="#993366")) +
  theme_linedraw()+ temaejes +
  theme(legend.background = element_rect(fill="transparent"), legend.box = "horizontal",
        plot.title = element_markdown(family = "Lato Black", size =  52), 
        plot.caption = element_text(family = "Lato", size = 12, color="gray65"),
        axis.text = element_text(size=8),
        axis.title.y = element_markdown(family = "Lato Light", size = 8, hjust=1),
        plot.subtitle = element_markdown(family = "Lato Light", size = 22, color = "gray65"),
        plot.tag = element_markdown(family = "Lato", size = 16, color = "black", hjust=0),
        panel.grid.major = element_line(linetype = "dashed", color="gray90", size=.2), 
        panel.background = element_rect(color="gray75"),
        axis.line = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.minor.y = element_line(linetype = "dashed", color="gray85", size=.2),
        legend.position = c(0.13,.3), legend.text=element_markdown(family = "Lato", size = 17, hjust=0),
        legend.title=element_markdown(family = "Lato", size = 14, hjust=0),
        strip.background = element_blank(),
        strip.text = element_text(family = "Lato Black", size = 14, color="black", hjust=0), 
        legend.key.size = unit(1.0, 'cm'),
        plot.tag.position = c(0.013, 0.2)) +
  labs(y = NULL, color="Respecto al máximo semanal previo a octubre de 2021",
       tag="<span style = 'font-size:14pt;color:#000000';>En número se muestran los acumulados desde 01/11/2021</span>",
       x = NULL,legend= NULL, title  = paste0("<span style = 'font-size:24pt'>Covid-19 en ","México","</span><br>La cuarta ola de contagios"), 
       subtitle= paste0(Fechahoy, " con corte al ", day(dia.ev)," de ",months.Date(dia.ev)," de ",year(dia.ev),"*"),
       caption =fuentefech)
Casosnalrelativa 


ggsave("Gráficos semanales/evolucionrelativanacional.png",Casosnalrelativa , width = 9.7 * (16/9), height = 11.2, type = "cairo", dpi = 600)
#<br><br><br><span style = 'font-size:18pt;color:#FABF8F';>**Entidades donde los hospitalizados<br>semanales en la 4ta. ola alcanzaron<br>70% o más del máximo semanal<br>anterior al 01/10/2021**.</span>"

Confirmados <- read_csv("Bases/ConfirmadosNacional.csv", 
                        locale = locale(encoding = "ISO-8859-1"), 
                        na = "NA")

Confirmados[is.na(Confirmados)] <- 0

Confirmados <- Confirmados %>% group_by(entidad_res, entidad_residencia) %>% 
  mutate(Casos.semana=rollsum(Casos_sintomas, 7, align="right", fill = 0),
         Decesos.semana.sint=rollsum(Decesos_sintomas, 7, align="right", fill = 0),
         Hospitalizados.semana.sint=rollsum(Hospitalizados_sintomas, 7, align="right", fill = 0),
         Decesos.semana=rollsum(Decesos_ocurridos, 7, align="right", fill = 0),
         Hospitalizados.semana=rollsum(Hospitalizados_ingresos, 7, align="right", fill = 0)) %>% 
  mutate(diasemana = weekdays(Fecha)) %>% 
  filter(diasemana==lundom) %>% select(Fecha, entidad_res,entidad_residencia, Casos.semana, Decesos.semana.sint, Decesos.semana, Hospitalizados.semana.sint, Hospitalizados.semana)

Casosprev2022<- Confirmados %>%
  select(entidad_res, entidad_residencia, Fecha, Casos.semana) %>%
  filter(Fecha<as.Date("2021-10-01")) %>% group_by(entidad_res,entidad_residencia) %>% 
  filter(Casos.semana ==max(Casos.semana)) %>% rename(Casos.max=Casos.semana) %>% 
  select(entidad_res,entidad_residencia,Casos.max)
Decesosprev2022<- Confirmados %>%
  select(entidad_res, entidad_residencia, Fecha, Decesos.semana.sint) %>%
  filter(Fecha<as.Date("2021-10-01")) %>% group_by(entidad_res,entidad_residencia) %>% 
  filter(Decesos.semana.sint ==max(Decesos.semana.sint)) %>% rename(Decesos.max=Decesos.semana.sint) %>% 
  select(entidad_res,entidad_residencia,Decesos.max)
Hospitalizadosprev2022<- Confirmados %>%
  select(entidad_res, entidad_residencia, Fecha, Hospitalizados.semana.sint) %>%
  filter(Fecha<as.Date("2021-10-01")) %>% group_by(entidad_res,entidad_residencia) %>% 
  filter(Hospitalizados.semana.sint ==max(Hospitalizados.semana.sint)) %>% rename(Hospitalizados.max=Hospitalizados.semana.sint) %>% 
  select(entidad_res,entidad_residencia,Hospitalizados.max)

Confirmados <- Confirmados %>% left_join(Casosprev2022, by=c("entidad_res", "entidad_residencia")) %>% 
  left_join(Decesosprev2022, by=c("entidad_res", "entidad_residencia")) %>%
  left_join(Hospitalizadosprev2022, by=c("entidad_res", "entidad_residencia")) 

Confirmadosrelativo <- Confirmados %>% mutate (Casos=round(Casos.semana/Casos.max,3),
                                               Decesos=round(Decesos.semana.sint/Decesos.max,3),
                                               Hospitalizados=round(Hospitalizados.semana.sint/Hospitalizados.max,3))

Entidadrelativa <- Confirmadosrelativo %>% mutate(code=as.character(as.double(entidad_res)),
                                                  name=entidad_residencia) %>% 
  filter(Fecha>=as.Date("2021-12-27")) %>% 
  group_by(entidad_res, entidad_residencia) %>% 
  mutate(Casosacum=sum(Casos.semana), Decesosacum=sum(Decesos.semana.sint), Hospitalizadosacum=sum(Hospitalizados.semana.sint), 
         clasf=if_else(entidad_residencia=="Tamaulipas", "100%", " ")) %>% 
  mutate(Let=round(Decesosacum/Casosacum,3), Hosp= round(Hospitalizadosacum/Casosacum,3)) %>% 
  mutate(Decesoslab=paste0(prettyNum(Decesosacum,big.mark=",",scientific=FALSE), " (", Let*100,"%)"), Hospitalizadoslab=paste0(prettyNum(Hospitalizadosacum,big.mark=",",scientific=FALSE), " (", Hosp*100,"%)"))
diainicio <- min(Entidadrelativa$Fecha)-6
CasosNacional <- sum(unique(Entidadrelativa$Casosacum))
DecesosNacional <- sum(unique(Entidadrelativa$Decesosacum))
HospitalizadosNacional <- sum(unique(Entidadrelativa$Hospitalizadosacum))
LetNacional <- round(DecesosNacional/CasosNacional,3)*100
HospNacional <- round(HospitalizadosNacional/CasosNacional,3)*100

Casoscol <- paste0("<span style = 'color:#01A2AC';>Casos: ", prettyNum(CasosNacional,big.mark=",",scientific=FALSE), "</span>")
Hospitalizadoscol <- paste0("<span style = 'color:#F79646';>Hospitalizados: ", prettyNum(HospitalizadosNacional,big.mark=",",scientific=FALSE),"</span> <span style = 'color:#F79646;font-size:18pt'>(",HospNacional,"%)</span>")
Decesoscol <- paste0("<span style = 'color:#993366';>Decesos: ", prettyNum(DecesosNacional,big.mark=",",scientific=FALSE),"</span> <span style = 'color:#993366;font-size:18pt'>(",LetNacional,"%)</span>")                            
labs=c(Casoscol,Hospitalizadoscol,Decesoscol)
Casosnalrelativa <- ggplot(Entidadrelativa) +
  #geom_hline(yintercept=1, linetype="dashed", color = "black", size=0.2) +
  # geom_rect(data=subset(Entidadrelativa, clasf =="A"),xmin = as.Date("2021-11-20"), xmax = as.Date(max(Entidadrelativa$Fecha)), ymin = 0, ymax = 6,
  #            fill=alpha("#FABF8F",0.3), stat="unique") +
  # geom_vline(xintercept=as.Date("2021-02-15"), linetype="solid", color = alpha("#D9C264",0.8), size=0.5, ) +
   annotate("segment", x = as.Date("2022-01-01"), xend = max(Entidadrelativa$Fecha)+10, y = 1, yend = 1,
            linetype="dashed",color = "black",size=0.2) +
  geom_line(data=subset(Entidadrelativa, Fecha <=dia.ev),aes( x= Fecha, y= Decesos,color="Decesos"), lineend="round", linejoin="round",linetype= "solid", size=.7, alpha=0.7) +
  geom_line(data=subset(Entidadrelativa, Fecha <=dia.ev),aes( x= Fecha, y= Hospitalizados,color="Hospitalizados"),color="#F79646", lineend="round", linejoin="round",linetype= "solid", size=.7, alpha=0.7) +
  geom_line(data=subset(Entidadrelativa, Fecha <=dia.ev),aes( x= Fecha, y= Casos, color="Casos"), lineend="round", linejoin="round",linetype= "solid", size=.7) +
  geom_point(data=subset(Entidadrelativa, Fecha ==dia.ev),aes( x= Fecha, y= Decesos),color="#73264D",  size=1) +
  geom_point(data=subset(Entidadrelativa, Fecha ==dia.ev),aes( x= Fecha, y= Hospitalizados),color="#F79646", size=1) +
  geom_point(data=subset(Entidadrelativa, Fecha ==dia.ev),aes( x= Fecha, y= Casos),color="#01A2AC", size=1) +
  geom_point(data=subset(Entidadrelativa, Casos ==max(Casos)),aes( x= Fecha, y= Casos),color="#01A2AC", size=1) +
  # geom_dl( data=subset(Entidadrelativa, Fecha ==dia.ev), aes(x= Fecha, y= Decesos, label=paste0(Decesos*100,"%", "; ",Decesos.semana)), color="#993366", 
  #          method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.6, fontfamily= "Lato Black")) +
  # geom_dl( data=subset(Entidadrelativa, Fecha ==dia.ev), aes(x= Fecha, y= Hospitalizados,  label =paste0(Hospitalizados*100,"%", "; ",Hospitalizados.semana)), color="#F79646", 
  #          method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.6, fontfamily= "Lato Black")) +
  # geom_dl( data=subset(Entidadrelativa, Fecha ==dia.ev), aes(x= Fecha, y= Casos,  label = paste0(Casos*100,"%", "; ",Casos.semana)), color="#01A2AC", 
  #          method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 0.6, fontfamily= "Lato Black")) +
  facet_geo(~ code, 
            grid = mx_state_grid2, 
            label = "name",
            scales = "fixed") +
  # geom_text(aes(x = as.Date("2021-11-20")+2, y = 5,
  #                                                                label = "Acumulados 4ta. ola:"), stat = "unique", family = "Lato",
  #           size = 2.5, color = "black",  hjust=0)+
  geom_text(aes(x = as.Date("2021-11-10")+2, y = 4.5,
                label = prettyNum(Casosacum,big.mark=",",scientific=FALSE)), stat = "unique", family = "Lato Black",
            size = 3.4, color = "#01A2AC",  hjust=0)+
  geom_text(aes(x = as.Date("2021-11-10")+2, y = 3,
                label = Hospitalizadoslab), stat = "unique", family = "Lato Black",
            size = 3.4, color = "#F79646",  hjust=0)+
  geom_text(aes(x = as.Date("2021-11-10")+2, y = 1.5,
                label = Decesoslab), stat = "unique", family = "Lato Black",
            size = 3.4, color = "#993366",  hjust=0)+
  geom_text(aes(x = max(Entidadrelativa$Fecha)-10, y = 1.6, label = clasf ),
               stat = "unique", family = "Lato",
            size = 3, color = "black",  hjust=0)+
  scale_y_continuous(expand = c(0, 0), limits= c(0,6),breaks = c(1,3,5), labels = c("100%"," ","500%"), position = "right") +
  scale_x_date(expand=c(0,0),limits= c(as.Date("2021-11-10"),max(Entidadrelativa$Fecha)+10), date_breaks = "1 year", 
               date_labels ="%Y") +
  scale_color_manual(values=c("Casos"="#01A2AC", 
                              "Hospitalizados"="#F79646",
                              "Decesos"="#993366"),
                     labels=labs) +
  theme_linedraw()+ temaejes +
  theme(legend.background = element_rect(fill="transparent"), legend.box = "horizontal",
        plot.title = element_markdown(family = "Lato Black", size =  54), 
        plot.caption = element_text(family = "Lato", size = 12, color="gray65"),
        axis.text.x = element_text(size=10,family = "Lato Light"),
        axis.text.y = element_blank(),
        axis.title.y = element_markdown(family = "Lato Light", size = 8, hjust=1),
        plot.subtitle = element_markdown(family = "Lato Light", size = 22, color = "gray65"),
        plot.tag = element_markdown(family = "Lato", size = 16, color = "black", hjust=0),
        panel.grid.major.x = element_line(linetype = "solid", color="gray70", size=.2), 
        panel.grid.major.y = element_blank(), 
        panel.background = element_rect(color="gray75"),
        axis.line = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = c(0.15,.3), legend.text=element_markdown(family = "Lato Black", size = 24, hjust=0),
        legend.title=element_markdown(family = "Lato", size = 22, hjust=0),
        strip.background = element_blank(),
        strip.text = element_text(family = "Lato Black", size = 14, color="black", hjust=0), 
        legend.key.size = unit(1.5, 'cm'),
        plot.tag.position = c(0.67, 0.65)) +
  labs(y = NULL, color=paste0("**ACUMULADO NACIONAL**<br><span style = 'font-size:14pt;color:#000000';>desde el ", day(diainicio)," de ",months.Date(diainicio)," de ",year(diainicio), "</span>"),
       tag="<span style = 'font-size:10pt;color:#000000';>Respecto al máximo semanal<br>previo a octubre de 2021</span>",
       x = NULL,legend= NULL, title  = paste0("<span style = 'font-size:24pt'>Covid-19 en ","México","</span><br><span style = 'color:#01A2AC';>Un difícil inicio de 2022</span>"), 
       subtitle= paste0("Casos confirmados por fecha de inicio de síntomas desde el ", day(diainicio)," de ",months.Date(diainicio)," de ",year(diainicio), " al ", day(dia.ev)," de ",months.Date(dia.ev)," de ",year(dia.ev),"*<br>"),
       caption =fuentefech)

ggsave("Gráficos semanales/evolucionrelativanacionalSINT3.png",Casosnalrelativa , width = 9.7 * (16/9), height = 11.2, type = "cairo", dpi = 400)

