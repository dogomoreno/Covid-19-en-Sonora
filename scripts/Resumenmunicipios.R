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
  library(magick)

logo <- image_read("Shapes/SEDFesp.png")
# Función para ejes en números enteros
int_breaks_rounded <- function(x, n = 5)  pretty(x, n)[round(pretty(x, n),1) %% 1 == 0] 
 
 # Bases municipales
  Casos <- read_csv("Bases/Casosdiarios_SSFED.csv", 
                    col_types = cols(CASOS = col_integer(), 
                                     CVEGEO = col_character(), Fecha = col_date(format = "%Y-%m-%d"), 
                                     MUNICIPIO = col_character(), NUEVOS = col_integer(), X1 = col_skip()), 
                    locale = locale(encoding = "ISO-8859-1")) %>% rename(Casos.diarios=NUEVOS) %>% filter(CVEGEO!=26999) 
  
  Casossemana <- Casos %>%
    mutate(diasemana = weekdays(Fecha)) %>% filter(diasemana=="sábado") %>% 
    group_by(MUNICIPIO) %>% 
    mutate(Casossemana =  (CASOS - lag(CASOS, default = 0, order_by=Fecha))) %>% select(Fecha, CVEGEO, MUNICIPIO, Casossemana)
  
  Decesos <- read_csv("Bases/Decesosdiarios_SSFED.csv", 
                      col_types = cols(DECESOS = col_integer(), 
                                       CVEGEO = col_character(), Fecha = col_date(format = "%Y-%m-%d"), 
                                       MUNICIPIO = col_character(), NUEVOS = col_integer(), X1 = col_skip()), 
                      locale = locale(encoding = "ISO-8859-1")) %>% rename(Decesos.diarios=NUEVOS) %>% filter(CVEGEO!=26999)
  
 Decesossemana <- Decesos %>%
    mutate(diasemana = weekdays(Fecha)) %>% filter(diasemana=="sábado") %>% 
    group_by(MUNICIPIO) %>% 
    mutate(Decesossemana =  (DECESOS - lag(DECESOS, default = 0, order_by=Fecha))) %>% select(Fecha, CVEGEO, MUNICIPIO, Decesossemana)
  
  CasosSINT <- read_csv("Bases/CasosdiariosSINT_SSFED.csv", 
                    col_types = cols(CASOS_SINTOMAS = col_integer(), 
                                     CVEGEO = col_character(), Fecha = col_date(format = "%Y-%m-%d"), 
                                     MUNICIPIO = col_character(), X1 = col_skip()), 
                    locale = locale(encoding = "ISO-8859-1")) %>% filter(CVEGEO!=26999)
  DecesosDEF <- read_csv("Bases/DecesosdiariosDEF_SSFED.csv", 
                      col_types = cols(DECESOS_OCURRIDOS = col_integer(), 
                                       CVEGEO = col_character(), Fecha = col_date(format = "%Y-%m-%d"), 
                                       MUNICIPIO = col_character(), X1 = col_skip()), 
                      locale = locale(encoding = "ISO-8859-1")) %>% filter(CVEGEO!=26999) 
  
  Activosdiarios <- read_csv("Bases/Activosdiarios_SSFED.csv", 
                                   col_types =cols(CASOS_ACTIVOS = col_integer(), 
                                                                          CVEGEO = col_character(), Fecha = col_date(format = "%Y-%m-%d"), 
                                                                          MUNICIPIO = col_character(), X1 = col_skip()), 
                                   locale = locale(encoding = "ISO-8859-1")) %>% filter(CVEGEO!=26999) 
  
  
  Fechahoy <- paste0("Al reporte del ", day(max(Casos$Fecha)), " de ", months.Date(max(Casos$Fecha))," de ", year(max(Casos$Fecha)))
  fuente <- "Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud del Gobierno de la República\n*El registro de los últimos 14 días aún se están alimentando en el sistema. Casos activos incluyen decesos. | www.luisarmandomoreno.com"
  
  POBMUN <- read_csv("Bases/POBMUN.csv", col_types = cols(CVEGEO = col_character()), 
                     locale = locale(encoding = "ISO-8859-1"))
  
  temaejes <- theme(axis.line = element_line(linetype = "solid",color = "black", size = 0.3), 
                    plot.margin = margin(10, 25, 10, 25), panel.grid=element_blank(),
                    plot.title = element_markdown(family = "Lato Black", size = 15),  
                    plot.subtitle = element_text(family = "Lato Light", size = 10, color = "black"), legend.title = element_blank(),
                    strip.text = element_text(family = "Lato Black", size = 10),
                    axis.text = element_text(family = "Lato", size =6),
                    plot.background = element_blank(),
                    axis.title.x = element_text(family = "Lato Light", size = 8, hjust=1),
                    axis.title.y = element_text(family = "Lato Light", size = 8, hjust=1), 
                    plot.caption = element_text(family = "Lato", size = 6),
                    legend.text = element_blank(),
                    legend.position = "none", plot.title.position = 'plot', plot.caption.position = 'plot')
  
  
  Casosconfd <-CasosSINT %>% group_by(MUNICIPIO) %>%  
    mutate(Casos.media.7d=round(rollmeanr(x=CASOS_SINTOMAS, 7, fill = NA),1))
  
  
  Decesosconfd <-DecesosDEF %>% group_by(MUNICIPIO) %>% 
    mutate(Decesos.media.7d=round(rollmeanr(x=DECESOS_OCURRIDOS, 7, fill = NA),1))
  
  Casos <-  Casos %>% group_by(MUNICIPIO) %>%  
    mutate(Casos.repo.media.7d=round(rollmeanr(x=Casos.diarios, 7, fill = NA),1))
  Casos$Casos.repo.media.7d[Casos$Casos.repo.media.7d<0] <- 0
  
  
  Decesos <-Decesos %>% group_by(MUNICIPIO) %>% 
    mutate(Decesos.repo.media.7d=round(rollmeanr(x=Decesos.diarios, 7, fill = NA),1))
  Decesos$Decesos.repo.media.7d[Decesos$Decesos.repo.media.7d<0] <- 0
  
  CasosDecesos <- Casos %>% full_join(Decesos, by= c("Fecha", "CVEGEO", "MUNICIPIO")) %>% 
    full_join(Casosconfd, by= c("Fecha", "CVEGEO", "MUNICIPIO")) %>% 
    full_join(Decesosconfd, by= c("Fecha", "CVEGEO", "MUNICIPIO")) %>% 
    full_join(Activosdiarios, by= c("Fecha", "CVEGEO", "MUNICIPIO")) %>% 
    full_join(Casossemana, by= c("Fecha", "CVEGEO", "MUNICIPIO")) %>% 
    full_join(Decesossemana, by= c("Fecha", "CVEGEO", "MUNICIPIO")) %>% arrange(Fecha, .by_group = FALSE) %>% 
    filter(Fecha>as.Date("2020-03-10")) 
  CasosDecesos[is.na(CasosDecesos)] <- 0
CasosDecesos <- CasosDecesos %>% 
  group_by(MUNICIPIO) %>% 
  mutate(Decesos.7d=rollsum(DECESOS_OCURRIDOS,7, align="right", fill = 0))
  
  dia.act <- max(as.Date(Casos$Fecha))-14

  
  plot_municipio <- function(x = "Hermosillo") {
    tmp <- CasosDecesos %>%
      filter(MUNICIPIO == x)
    tmp2 <- tmp %>% filter(Fecha==max(as.Date(Fecha)))
    
    p1 <- ggplot(tmp) +
      #geom_line(data=subset(tmp, Fecha >as.Date("2020-11-01")),aes(x= Fecha, y= Casos.repo.media.7d, color= "Tendencia promedio móvil 7 días por reporte"), linetype= "solid", size=.55,lineend="round", linejoin="round", arrow=arrow(type="open", length=unit(0.10,"cm")))+
      geom_area(data=subset(tmp, Fecha <=dia.act), aes( x= Fecha, y= Casos.media.7d), fill= "#58BCBC", alpha=0.3)+
      geom_line(data=subset(tmp, Fecha <=dia.act), aes(x= Fecha, y= Casos.media.7d, color= "Tendencia promedio móvil 7 días por inicio de síntomas"), linetype= "solid",lineend="round", linejoin="round",  size=.75, arrow=arrow(type="open", length=unit(0.10,"cm")))+
      geom_point(data=subset(tmp, Fecha <=dia.act), aes(x= Fecha, y= CASOS_SINTOMAS,fill="Casos que iniciaron síntomas el día correspondiente"), color = "white", size = 0.9, stroke=0.2, shape = 21) +
      geom_point(data=subset(tmp, Fecha >dia.act), aes(x= Fecha, y= CASOS_SINTOMAS), fill= alpha("#01787E", 0.45), color = "white", size = 0.9, stroke=0.4, shape = 21) +
      scale_fill_manual(name="", values= c("Casos que iniciaron síntomas el día correspondiente" = "#01787E")) +
      scale_color_manual(name="", values= c("Tendencia promedio móvil 7 días por inicio de síntomas" = "#01787E")) +
      scale_y_continuous(expand = c(0, 0), limits=c(0, max(tmp$CASOS_SINTOMAS)+5), breaks = int_breaks_rounded) +
      scale_x_date(expand=c(0,0),limits= c(min(tmp$Fecha)-2,max(tmp$Fecha)+5), date_breaks = "1 month", 
                   labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                                paste(month(x, label = TRUE), "\n", year(x)), 
                                                paste(month(x, label = TRUE)))) +
      theme_bw() + temaejes +
      theme(legend.text = element_text(family = "Lato", size = 7), legend.background = element_rect(fill="transparent"), legend.box = "horizontal",
            legend.position = c(0.02,0.95),  legend.justification="left", legend.margin=margin(t = 0, unit='cm'),
            legend.key = element_rect(fill="transparent", color="transparent")) +
      labs(y = NULL, 
           x = NULL,legend= NULL, title  = paste0("<span style = 'color:#01A2AC';>Casos confirmados acumulados: ", prettyNum(as.numeric(tmp2$CASOS), big.mark=",", preserve.width="none"),"</span>"), 
           subtitle= paste0("Casos confirmados en la última semana: ",tmp2$Casossemana, ", casos activos al corte: ",tmp2$CASOS_ACTIVOS), caption =NULL)
    
   
    p2 <- ggplot(tmp) +
      #geom_line(data=subset(tmp, Fecha >as.Date("2020-11-01")),aes(x= Fecha, y= Decesos.repo.media.7d, color= "Tendencia promedio móvil 7 días por reporte"), linetype= "solid", size=.55, lineend="round", linejoin="round", arrow=arrow(type="open", length=unit(0.10,"cm")))+
      geom_area(data=subset(tmp, Fecha <=dia.act), aes( x= Fecha, y= Decesos.media.7d), fill= "#D075A3", alpha=0.3)+
      geom_line(data=subset(tmp, Fecha <=dia.act), aes(x= Fecha, y= Decesos.media.7d, color= "Tendencia promedio móvil 7 días por fecha de ocurridos"), linetype= "solid", size=.55,lineend="round", linejoin="round", arrow=arrow(type="open", length=unit(0.10,"cm")))+
      geom_point(data=subset(tmp, Fecha <=dia.act), aes(x= Fecha, y= DECESOS_OCURRIDOS,fill="Decesos ocurridos el día correspondiente"), color = "white", size = 0.9, stroke=0.24, shape = 21) +
      geom_point(data=subset(tmp, Fecha >dia.act), aes(x= Fecha, y= DECESOS_OCURRIDOS), fill= "#73264D", color = "white", size = 0.9, stroke=0.4, alpha=0.35, shape = 21) +
      scale_fill_manual(name="", values= c("Decesos ocurridos el día correspondiente" = alpha("#73264D", 0.65))) + 
      scale_color_manual(name="", values= c("Tendencia promedio móvil 7 días por fecha de ocurridos" = "#73264D")) +
      scale_y_continuous(expand = c(0, 0), limits= c(0,max(tmp$DECESOS_OCURRIDOS)+2), breaks = int_breaks_rounded) +
      scale_x_date(expand=c(0,0), limits= c(min(tmp$Fecha)-2,max(tmp$Fecha)+5), date_breaks = "1 month", 
                   labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                                paste(month(x, label = TRUE), "\n", year(x)), 
                                                paste(month(x, label = TRUE)))) +
      theme_bw() + temaejes +
      theme(legend.text = element_text(family = "Lato", size = 7), legend.background = element_rect(fill="transparent"),legend.box = "horizontal", 
            legend.position = c(0.02,0.95),  legend.justification="left", legend.margin=margin(t = 0, unit='cm'),
            legend.key = element_rect(fill="transparent", color="transparent")) +
      labs(y = NULL, 
           x = NULL,legend= NULL, title  =  paste0("<span style = 'color:#993366';> Decesos confirmados acumulados: ", prettyNum(as.numeric(tmp2$DECESOS), big.mark=",", preserve.width="none"),"</span>"), 
           subtitle= paste0("Decesos confirmados en la última semana: ",tmp2$Decesossemana, ", ocurridos dentro de las últimos 7 días: ",tmp2$Decesos.7d), caption =NULL)
    
    patchwork <- (p1 / p2)
    p3 <- patchwork + plot_annotation(
      title = paste0("<span style = 'font-size:12pt'>Covid-19 en Sonora:</span><br>",x),
      subtitle = Fechahoy,
      caption = fuente, theme= theme(
        plot.title = element_markdown(family = "Lato Black", size = 30),  
        plot.subtitle = element_text(family = "Lato Light", size = 12, color = "black"),
        plot.caption = element_text(family = "Lato", size = 8), plot.title.position = 'plot', 
        plot.caption.position = 'plot', plot.margin = margin(10, 25, 10, 25), 
        plot.background = element_rect(fill = "white", color = "white", size = 3)))
    p4 <- cowplot::ggdraw() +
      cowplot::draw_plot(p3,x = 0, y = 0, width = 1, height = 1) +
      cowplot::draw_image(logo, x = 0.85, y = 0.89, width = 0.1, height = 0.1)     
  
    ggsave(paste0("municipales/", x,".png"),p4, width = 5 * (16/9), height = 10, type = "cairo", dpi = 400)
    
  }
 
  plot_municipio("Hermosillo")  
    
  for (k in unique(CasosDecesos$MUNICIPIO)) {
    plot_municipio(k)
  }
  
  





# Ficha estatal

# Sonora.DF <- read_csv("Bases/ST_SonoraReporte_SSFED.csv",
#                       col_types = cols(fecha_reporte = col_date(format = "%Y-%m-%d")))
#
# dia<-max(as.Date(Sonora.DF$fecha_reporte))
#
# Fechahoy <- paste0("Al reporte del ", day(dia), " de ", months.Date(dia)," de ", year(dia),".")
# fuente <- "Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud del Gobierno de la República\n*Por fecha de reporte | www.luisarmandomoreno.com"
# fuenteimp <- "Elaboración Luis Armando Moreno (@dogomoreno) con información de la SSA del Gobierno Federal y la SSA del Gobierno Federal y SSA del Gobierno de Sonora\nwww.luisarmandomoreno.com"
# fuenteprueba <- "Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud del Gobierno de la República\n*Incluye resultados válidos de pruebas PCR y antigénicas. | www.luisarmandomoreno.com"
# fuenteact <- "Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud del Gobierno de la República\n*Se consideran casos activos aquellos que iniciaron síntomas dentro de los 14 días previos al corte. | www.luisarmandomoreno.com"
# subtitulo <- paste0("Casos confirmados en los últimos 7 días por 100 mil habitantes\nAl reporte del ",day(dia),"/",month(dia),"/",year(dia))
# subtituloact <- paste0("Casos activos (que iniciaron síntimas dentro de los 14 días previos) por 100 mil habitantes.\nAl reporte del ",day(dia),"/",month(dia),"/",year(dia))
#
# fuentefech <- "Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud del Gobierno de la República\n*El registro de los últimos 14 días aún se están alimentando en el sistema | www.luisarmandomoreno.com"
# POBMUN <- read_csv("Bases/POBMUN.csv", col_types = cols(CVEGEO = col_character()),
#                    locale = locale(encoding = "ISO-8859-1"))


# Sonora.DF <- read_csv("Bases/ST_SonoraReporte_SSFED.csv",
#                       col_types = cols(fecha_reporte = col_date(format = "%Y-%m-%d")))
# Sonora.DF <- rename(Sonora.DF, Fecha=fecha_reporte)
# Sonora.DF <- mutate(Sonora.DF, Casos.diarios= Casos - lag(Casos, default = Casos[1], order_by=Fecha))
# Sonora.DF <- mutate(Sonora.DF, Confirmados.diarios= Confirmados_lab - lag(Confirmados_lab, default = Confirmados_lab[1], order_by=Fecha))
# Sonora.DF <- mutate(Sonora.DF, Hospitalizados.diarios= Hospitalizados - lag(Hospitalizados, default = Casos[1], order_by=Fecha))
# Sonora.DF <- mutate(Sonora.DF, Decesos.diarios= Decesos - lag(Decesos, default = Decesos[1], order_by=Fecha))
# Sonora.DF <- mutate(Sonora.DF, Casos.media.7d=round(rollmeanr(x=Casos.diarios, 7, fill = NA),1))
# Sonora.DF <- mutate(Sonora.DF, Decesos.media.7d=round(rollmeanr(x=Decesos.diarios,7, fill=NA),1))
# Sonora.DF <- mutate(Sonora.DF, Hospitalizados.media.7d=round(rollmeanr(x=Hospitalizados.diarios, 7, fill = NA),1))
# Sonora.DF <- mutate(Sonora.DF, Resultados.diarios= Resultados_lab - lag(Resultados_lab, default = Resultados_lab [1]))
# Sonora.DF <- mutate(Sonora.DF, Resultados.media.7d=round(rollmeanr(x=Resultados.diarios,7, fill=NA),1))
# Sonora.DF <- mutate(Sonora.DF, Ambulatorios.Activos.7d=round(rollmeanr(x=Ambulatorios.Activos,7, fill=NA),1))
# Sonora.DF <- mutate(Sonora.DF, Sospechosos.7d=round(rollmeanr(x=Sospechosos,7, fill=NA),1))
# Sonora.DF <- mutate(Sonora.DF, Sospechosos.diarios= Sospechosos - lag(Sospechosos, default = Sospechosos[1], order_by=Fecha))
# Sonora.DF <- mutate(Sonora.DF, Sospechosos.diarios.7d=round(rollmeanr(x=Sospechosos.diarios,7, fill=NA),1))
# Sonora.DF <- mutate(Sonora.DF, Hospitalizados.Activos.7d=round(rollmeanr(x=Hospitalizados.Activos,7, fill=NA),1))
# Sonora.DF <- mutate(Sonora.DF, Incidencia= round((Casos / 30.74745),2))
# Sonora.DF <- mutate(Sonora.DF, Letalidad= round((Decesos / Casos)*100,1))
# Sonora.DF <- mutate(Sonora.DF, Mortalidad= round((Decesos / 30.74745)*100,2))
# Sonora.DF <- mutate(Sonora.DF, Positividad.diaria= round((Confirmados.diarios / Resultados.diarios)*100,2))
# Sonora.DF <- mutate(Sonora.DF, Positividad= round((Confirmados_lab / Resultados_lab )*100,2))
# Sonora.DF <- mutate(Sonora.DF, Analizados.diarios= Analizados - lag(Analizados, default = Analizados[1], order_by=Fecha))
# Sonora.DF <- mutate(Sonora.DF, Analizados.media.7d=round(rollmeanr(x=Analizados.diarios, 7, fill = NA),1))
#
# estata.hoy <- Sonora.DF %>% filter(Fecha==max(as.Date(Fecha)))
# estata.semana <- Sonora.DF %>% filter(Fecha==(max(as.Date(Fecha))-7))
#
#
# #Casos diarios Estatal
#
# Sonora.ED <- read_csv("Bases/ST_SonoraInformesCOVID_SSEDO.csv",
#                       col_types = cols(fecha_corte = col_date(format = "%d/%m/%Y")))
# Sonora.ED <- rename(Sonora.ED, Fecha_reporte= Fecha)
# Sonora.ED <- rename(Sonora.ED, Fecha=fecha_corte)
# Sonora.ED <- mutate(Sonora.ED, Casos.diarios= Casos - lag(Casos, default = Casos[1], order_by=Fecha))
# Sonora.ED <- mutate(Sonora.ED, Decesos.diarios= Decesos_imp - lag(Decesos_imp, default = Decesos_imp[1], order_by=Fecha))
# Sonora.ED <- select(Sonora.ED, Fecha, Casos.diarios, Decesos.diarios)
#
# CasosPos <- Sonora.DF %>% filter(Fecha>as.Date("2021-09-19")) %>%
#   select(Fecha, Casos.diarios, Decesos.diarios)
#
# Sonora.ED <- Sonora.ED %>% bind_rows(CasosPos)
# Sonora.ED <- mutate(Sonora.ED, Casos.media.7d=round(rollmeanr(x=Casos.diarios, 7, fill = NA),1),
#                     Decesos.media.7d=round(rollmeanr(x=Decesos.diarios, 7, fill = NA),1))
# Sonora.ED[is.na(Sonora.ED)] <- 0
#
#
# e1 <- ggplot(Sonora.ED) +
#   geom_hline(yintercept=estata.semana$Casos.diarios, linetype="dashed", color = "gray65") +
#   geom_hline(yintercept=estata.hoy$Casos.diarios, linetype="dashed", color = "red") +
#   geom_area(aes(x= Fecha, y= Casos.media.7d), fill= "#58BCBC", alpha=0.3)+
#   geom_line(aes(x= Fecha, y= Casos.media.7d, color= "Tendencia promedio móvil 7 días"), linetype= "solid", size=.7, arrow=arrow(type="open", length=unit(0.10,"cm")))+
#   geom_point(aes(x= Fecha, y= Casos.diarios, fill= "Variación diaria por fecha de reporte"), color = "white", size = 0.8, stroke=0.4, alpha=0.65, shape = 21) +
#   scale_color_manual(name="", values= c("Tendencia promedio móvil 7 días" = "#01787E")) +
#   scale_fill_manual(name="", values= c("Variación diaria por fecha de reporte"= "#01787E")) +
#   scale_y_continuous(expand = c(0, 15), limits=c(0, max(Sonora.ED$Casos.diarios)+100)) +
#   scale_x_date(expand=c(0,5), date_breaks = "1 month", labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x),
#                                                                                     paste(month(x, label = TRUE), "\n", year(x)),
#                                                                                     paste(month(x, label = TRUE)))) +
#   geom_text(aes(x = as.Date("2020-03-08"), y = (estata.semana$Casos.diarios + 30),
#                 label = paste0("+", estata.semana$Casos.diarios, " casos el mismo día semana anterior")), stat = "unique", family = "Lato Black",
#             size = 2.5, color = "gray45", hjust=0)+
#   geom_text(aes(x = as.Date("2020-03-10"), y = (estata.hoy$Casos.diarios + 30),
#                 label = paste0("+", estata.hoy$Casos.diarios, " casos hoy")), stat = "unique", family = "Lato Black",
#             size = 3.5, color = "red",  hjust=0)+
#   theme_bw() + temaejes +
#   theme(legend.text = element_text(family = "Lato", size = 7), legend.background = element_rect(fill="transparent"), legend.box = "horizontal",
#         legend.position = c(0.02,0.95),  legend.justification="left", legend.margin=margin(t = 0, unit='cm'),
#         legend.key = element_rect(fill="transparent", color="transparent")) +
#   labs(y = NULL,
#        x = NULL,legend= NULL, title  = paste0("<span style = 'color:#01A2AC';>Casos confirmados acumulados: ", prettyNum(as.numeric(estata.hoy$Casos), big.mark=",", preserve.width="none"),"</span>"),
#        subtitle= paste0("Casos confirmados hoy: ",prettyNum(as.numeric(estata.hoy$Casos.diarios), big.mark=",", preserve.width="none"), ", casos activos al corte: ",prettyNum(as.numeric(estata.hoy$Activos), big.mark=",", preserve.width="none")), caption =NULL)
#
#
# e2 <- ggplot(Sonora.ED) +
#   geom_hline(yintercept=estata.semana$Decesos.diarios, linetype="dashed", color = "gray65") +
#
#   geom_hline(yintercept=estata.hoy$Decesos.diarios, linetype="dashed", color = "red") +
#   geom_area(aes(x= Fecha, y= Decesos.media.7d), fill= "#D075A3", alpha=0.3)+
#   geom_line(aes(x= Fecha, y= Decesos.media.7d, color= "Tendencia promedio móvil 7 días"), linetype= "solid", size=.75, arrow=arrow(type="open", length=unit(0.10,"cm")))+
#   geom_point(aes(x= Fecha, y= Decesos.diarios, fill= "Variación diaria por fecha de reporte"), color = "white", size = 0.8, stroke=0.4, alpha=0.65, shape = 21) +
#   scale_color_manual(name="", values= c("Tendencia promedio móvil 7 días" = "#73264D")) +
#   scale_fill_manual(name="", values= c("Variación diaria por fecha de reporte"= "#73264D")) +
#   scale_y_continuous(expand = c(0, 0), limits=c(0, (max(Sonora.ED$Casos.diarios)+100)/10)) +
#   scale_x_date(expand=c(0,5), date_breaks = "1 month", labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x),
#                                                                                     paste(month(x, label = TRUE), "\n", year(x)),
#                                                                                     paste(month(x, label = TRUE)))) +
#   geom_text(aes(x = as.Date("2020-03-10"), y = (estata.semana$Decesos.diarios + 3),
#                 label = paste0("+", estata.semana$Decesos.diarios, " decesos el mismo día semana anterior")), stat = "unique", family = "Lato Black",
#             size = 2.5, color = "gray45", hjust=0)+
#   geom_text(aes(x = as.Date("2020-03-10"), y = estata.hoy$Decesos.diarios + 3,
#                 label = paste0("+", estata.hoy$Decesos.diarios, " decesos hoy")), stat = "unique", family = "Lato Black",
#             size = 3.5, color = "red", hjust=0)+
#   theme_bw() + temaejes +
#   theme(legend.text = element_text(family = "Lato", size = 7), legend.background = element_rect(fill="transparent"),legend.box = "horizontal",
#         legend.position = c(0.02,0.95),  legend.justification="left", legend.margin=margin(t = 0, unit='cm'),
#         legend.key = element_rect(fill="transparent", color="transparent")) +
#   labs(y = NULL,
#        x = NULL,legend= NULL, title  =  paste0("<span style = 'color:#993366';> Decesos confirmados acumulados: ", prettyNum(as.numeric(estata.hoy$Decesos), big.mark=",", preserve.width="none"),"</span>"),
#        subtitle= paste0("Decesos confirmados hoy: ",estata.hoy$Decesos.diarios), caption =NULL)
#
# patchwork <- (e1 / e2)
# e3 <- patchwork + plot_annotation(
#   title = "<span style = 'font-size:12pt'></span><br>Covid-19 en Sonora",
#   subtitle = Fechahoy,
#   caption = fuenteimp, theme= theme(
#     plot.title = element_markdown(family = "Lato Black", size = 36),
#     plot.subtitle = element_text(family = "Lato Light", size = 12, color = "black"),
#     plot.caption = element_text(family = "Lato", size = 8), plot.title.position = 'plot',
#     plot.caption.position = 'plot', plot.margin = margin(10, 25, 10, 25),
#     plot.background = element_rect(fill = "white", color = "white", size = 3)))
#
#
# ggsave(paste0("Gráficos diarios/fichaestatal.png"),e3, width = 5 * (16/9), height = 10, type = "cairo", dpi = 400)
# library(magick)
#
# logo <- image_read("Shapes/SEDFesp.png")
# e4 <- cowplot::ggdraw() +
#   cowplot::draw_plot(e3,x = 0, y = 0, width = 1, height = 1) +
#   cowplot::draw_image(logo, x = 0.85, y = 0.89, width = 0.1, height = 0.1)
#
#   ggsave(paste0("Gráficos diarios/fichaestatal.png"), e4, width = 5 * (16/9), height = 10, type = "cairo", dpi = 400)
