rm(list=ls())

# Paquetes

library(tidyverse)
library(extrafont)
library(scales)
library(plotly)
library(htmlwidgets)
library(showtext)
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

HMO.DF <- read_csv("Bases/ST_HMOReporte_SSFED.csv", 
                      col_types = cols(fecha_reporte = col_date(format = "%Y-%m-%d")))

dia<-max(as.Date(HMO.DF$fecha_reporte))

Fechahoy <- paste0("Al reporte del ", day(dia), " de ", months.Date(dia)," de ", year(dia))
fuente <- "Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud del Gobierno de la República\n*Por fecha de reporte | www.luisarmandomoreno.com"
subtitulo <- paste0("Casos confirmados en los últimos 7 días por 100 mil habitantes\nAl reporte del ",day(dia),"/",month(dia),"/",year(dia))

POBMUN <- read_csv("Bases/POBMUN.csv", col_types = cols(CVEGEO = col_character()), 
                   locale = locale(encoding = "ISO-8859-1"))

temaejes <- theme(plot.margin = margin(10, 25, 10, 25), panel.grid=element_blank(), panel.border=element_blank(), axis.line= element_line(color = "black", size = 0.6),
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
                      legend.box.background=  element_rect(fill="transparent", color="transparent" ), 
                      legend.key = element_rect(fill="transparent", color="transparent"),
                      legend.position = "none",  legend.justification="left", plot.title.position = 'plot', plot.caption.position = 'plot')

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
HMO.DF <- read_csv("Bases/ST_HMOReporte_SSFED.csv", 
                      col_types = cols(fecha_reporte = col_date(format = "%Y-%m-%d"))) %>% filter(fecha_reporte>as.Date("2020-10-08"))
HMO.DF.hoy <- filter(HMO.DF, fecha_reporte == dia)
HMO.DF.hoy <- select(HMO.DF.hoy, Hospitalizados.Activos, Ambulatorios.Activos, Decesos, Recuperados)
HMO.DF.hoy <- rename(HMO.DF.hoy, "Ambulatorios activos"= Ambulatorios.Activos, "Hospitalizados Activos"=Hospitalizados.Activos)
HMO.DF.hoy <- gather(HMO.DF.hoy, key= Estatus, value= Casos.confirmados) 

#Treemap

HMO.DF.hoy <- filter(HMO.DF, fecha_reporte == max(fecha_reporte))
HMO.DF.hoy <- select(HMO.DF.hoy, Hospitalizados.Activos, Ambulatorios.Activos, Decesos, Recuperados)
HMO.DF.hoy <- rename(HMO.DF.hoy, "Ambulatorios activos"= Ambulatorios.Activos, "Hospitalizados Activos"=Hospitalizados.Activos)
HMO.DF.hoy <- gather(HMO.DF.hoy, key= Estatus, value= Casos.confirmados) 
tituestatus <- paste0("<span style = 'font-size:14pt'>Covid-19 en Hermosillo:</span><br>Estatus de los <span style = 'color:#01A2AC';>", prettyNum(as.numeric(sum(HMO.DF.hoy$Casos.confirmados)), big.mark=",", preserve.width="none"), "</span> casos confirmados")


HMO.DF.hoy$label <- c(paste0(HMO.DF.hoy$Estatus, "\n ",prettyNum(as.numeric(HMO.DF.hoy$Casos.confirmados), big.mark=",", preserve.width="none"), "\n ", (round((HMO.DF.hoy$Casos.confirmados/(sum(HMO.DF.hoy$Casos.confirmados))*100), digits = 1)),"%"))
Hosp.hoy <- HMO.DF.hoy %>% filter(Estatus=="Hospitalizados Activos")

hosplab <- Hosp.hoy$label

Estatus <- ggplot(HMO.DF.hoy, aes(area = Casos.confirmados, fill= Estatus, label= HMO.DF.hoy$label)) + geom_treemap( size=2, color= "white") +
  scale_fill_manual(values= c("#01A2AC", "#993366", "#F79646", "#4BACC6")) +
  scale_color_manual(values=c("#005156","#4D1933", "#984807",  "#215968" )) +
  geom_treemap_text(family = "Lato Black", colour = "white", place = "topleft",
                    grow = FALSE, size=10) +  theme_void() +
  #annotate(geom = "text", x = 0.93, y = 0.99, label = hosplab, color= "white", family= "Lato Black",  hjust=1, size=2) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_void() + temasinejes +
  theme(plot.tag = element_text(family = "Lato Black", size = 6,color = "#F79646",hjust=1),
        plot.tag.position = c(1, 0.84), axis.line.x = element_blank(), axis.text.x = element_blank())+
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = tituestatus, tag = hosplab, 
       subtitle= Fechahoy, caption =fuente)
Estatus


ggsave("Gráficos diarios/HMO/Diarioestatus.png",Estatus , width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)



# Gráfico estatal


HMO.DF <- rename(HMO.DF, Fecha=fecha_reporte)
HMO.DF <-  filter(HMO.DF, Fecha>as.Date("2020-10-08"))
HMO.DF <- mutate(HMO.DF, Casos.diarios= Casos - lag(Casos, default = Casos[1], order_by=Fecha))
HMO.DF <- mutate(HMO.DF, Confirmados.diarios= Confirmados_lab - lag(Confirmados_lab, default = Confirmados_lab[1], order_by=Fecha))
HMO.DF <- mutate(HMO.DF, Decesos.diarios= Decesos - lag(Decesos, default = Decesos[1], order_by=Fecha))
HMO.DF <- mutate(HMO.DF, Casos.media.7d=round(rollmeanr(x=Casos.diarios, 7, fill = NA),1))
HMO.DF <- mutate(HMO.DF, Decesos.media.7d=round(rollmeanr(x=Decesos.diarios,7, fill=NA),1))
HMO.DF <- mutate(HMO.DF, Resultados.diarios= Resultados_lab - lag(Resultados_lab, default = Resultados_lab [1]))
HMO.DF <- mutate(HMO.DF, Resultados.media.7d=round(rollmeanr(x=Resultados.diarios,7, fill=NA),1))
HMO.DF <- mutate(HMO.DF,Ambulatorios.Activos.7d=round(rollmeanr(x=Ambulatorios.Activos,7, fill=NA),1))
HMO.DF <- mutate(HMO.DF, Incidencia= round((Casos / 30.74745),2))
HMO.DF <- mutate(HMO.DF, Letalidad= round((Decesos / Casos)*100,1))
HMO.DF <- mutate(HMO.DF, Mortalidad= round((Decesos / 30.74745)*100,2))
HMO.DF <- mutate(HMO.DF, Positividad.diaria= round((Confirmados.diarios / Resultados.diarios)*100,2))
HMO.DF <- mutate(HMO.DF, Positividad= round((Confirmados_lab / Resultados_lab )*100,2))

estata.hoy <- HMO.DF %>% filter(Fecha==max(as.Date(Fecha)))
estata.semana <- HMO.DF %>% filter(Fecha==(max(as.Date(Fecha))-7))


# Casos diarios Estatal
CasosHMO <- ggplot(HMO.DF) +
  geom_hline(yintercept=estata.semana$Casos.diarios, linetype="dashed", color = "gray80") +
  geom_text(aes(x = as.Date("2020-10-20"), y = (estata.semana$Casos.diarios + 1),
                label = paste0("Semana anterior ", estata.semana$Casos.diarios, " casos")), stat = "unique", family = "Lato Black",
            size = 2, color = "gray80", hjust=0)+
  geom_hline(yintercept=estata.hoy$Casos.diarios, linetype="dashed", color = "red") +
  geom_area(aes(x= Fecha, y= Casos.media.7d), fill= "#58BCBC", alpha=0.3)+
  geom_line(aes(x= Fecha, y= Casos.media.7d, color= "Tendencia promedio móvil 7 días"), linetype= "solid", size=.75, arrow=arrow(type="open", length=unit(0.10,"cm")))+
  geom_point(aes(x= Fecha, y= Casos.diarios), color = "white", fill= "#01787E", size = 0.9, stroke=0.4, alpha=0.65, shape = 21) +
  scale_fill_manual(name="", values= c("Tendencia promedio móvil 7 días" = "#58BCBC", "Casos diarios" = "#01787E")) + 
  scale_color_manual(name="", values= c("Tendencia promedio móvil 7 días" = "#01787E", "Casos diarios" = "white")) +
  scale_y_continuous(expand = c(0, 0), limits=c(0, max(HMO.DF$Casos.diarios)+5)) +
  scale_x_date(expand=c(0,5), date_breaks = "1 month", date_labels = "%B") +
  geom_text(aes(x = as.Date("2020-10-20"), y = (estata.hoy$Casos.diarios + 1),
                   label = paste0("Nuevos ", estata.hoy$Casos.diarios, " casos")), stat = "unique", family = "Lato Black",
               size = 3, color = "red",  hjust=0)+
  theme_bw() + temaejes +
  theme(legend.text = element_text(family = "Lato", size = 7), legend.background = element_rect(fill="transparent"),
        legend.position = c(0.02,0.90),  legend.justification="left", legend.margin=margin(t = 0, unit='cm'),
        legend.key = element_rect(fill="transparent", color="transparent")) +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo:</span><br><span style = 'color:#01A2AC';>Casos confirmados diariamente</span>", 
       subtitle= Fechahoy, caption =fuente)

CasosHMO

ggsave("Gráficos diarios/HMO/diariocasos.png",CasosHMO, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)

  
  DecesosSon <- ggplot(HMO.DF) +
  
  geom_hline(yintercept=estata.semana$Decesos.diarios, linetype="dashed", color = "gray80") +
  geom_text(aes(x = as.Date("2020-10-20"), y = (estata.semana$Decesos.diarios + 1.5),
                  label = paste0("Semana anterior ", estata.semana$Decesos.diarios, " decesos")), stat = "unique", family = "Lato Black",
              size = 2, color = "gray80", hjust=0)+
  geom_hline(yintercept=estata.hoy$Decesos.diarios, linetype="dashed", color = "red") +
  geom_area(aes(x= Fecha, y= Decesos.media.7d), fill= "#D075A3", alpha=0.3)+  
  geom_line(aes(x= Fecha, y= Decesos.media.7d, color= "Tendencia promedio móvil 7 días"), linetype= "solid", size=.75, arrow=arrow(type="open", length=unit(0.10,"cm")))+
  geom_point(aes(x= Fecha, y= Decesos.diarios), color = "white", fill= "#73264D", size = 0.9, stroke=0.4, alpha=0.65, shape = 21) +
  scale_fill_manual(name="", values= c("Decesos diarios" = "#73264D", "Tendencia promedio móvil 7 días" = "#D075A3")) + 
  scale_color_manual(name="", values= c("Decesos diarios" = "white","Tendencia promedio móvil 7 días" = "#73264D")) +
  scale_y_continuous(expand = c(0, 0), limits= c(0,10)) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2020-10-08"), max(as.Date(HMO.DF$Fecha))+5), date_breaks = "1 month", date_labels = "%B") +
  geom_text(aes(x = as.Date("2020-10-20"), y = estata.hoy$Decesos.diarios + 1.5,
                label = paste0("Nuevos ", estata.hoy$Decesos.diarios, " decesos")), stat = "unique", family = "Lato Black",
            size = 3, color = "red", hjust=0)+
  theme_bw() + temaejes +
  theme(legend.text = element_text(family = "Lato", size = 7), legend.background = element_rect(fill="transparent"),
        legend.position = c(0.02,0.95),  legend.justification="left", legend.margin=margin(t = 0, unit='cm'),
        legend.key = element_rect(fill="transparent", color="transparent")) +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo:</span><br><span style = 'color:#993366';>Decesos confirmados diariamente</span>", 
       subtitle= Fechahoy, caption =fuente)

DecesosSon

ggsave("Gráficos diarios/HMO/diariodecesos.png",DecesosSon,  width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)

PruebasSon <- ggplot(HMO.DF) +
  geom_area(aes(x= Fecha, y= Resultados.media.7d), fill= "#4BACC6", alpha=0.3)+
  geom_line(aes(x= Fecha, y= Resultados.media.7d, color= "Tendencia promedio móvil 7 días"), linetype= "solid", size=.75, arrow=arrow(type="open", length=unit(0.10,"cm")))+
  scale_fill_manual(name="", values= c("Tendencia promedio móvil 7 días" = "#4BACC6", "Resultados diarios" = "#31859C")) + 
  scale_color_manual(name="", values= c("Tendencia promedio móvil 7 días" = "#31859C", "Resultados diarios" = "white")) +
  scale_y_continuous(expand=c(0,0), limits=c(0, max(HMO.DF$Resultados.diarios,na.rm=TRUE)+20)) +
  scale_x_date(expand=c(0,10), date_breaks = "1 month", date_labels = "%B") +
  geom_hline(yintercept=estata.hoy$Resultados.diarios, linetype="dashed", color = "red") +
  geom_point(aes(x= Fecha, y= Resultados.diarios), color = "white", fill= "#31859C", size = 0.9, stroke=0.4, alpha=0.65, shape = 21) +
  geom_text(aes(x = as.Date("2020-10-20"), y = estata.hoy$Resultados.diarios+5,
               label = paste0(estata.hoy$Resultados.diarios, " nuevos resultados hoy\n", round(estata.hoy$Positividad.diaria,0), "% positivos")), stat = "unique", family = "Lato Black",
          size = 3, color = "red", hjust=0)+
  # geom_segment(aes(x = as.Date("2021-02-22"), y = 490, xend = as.Date("2021-03-11"), yend = 250),
  #              size = 1.5, color = "black",
  #              arrow = arrow(length = unit(0.02, "npc"))) +
  # geom_text(aes(x = as.Date("2021-02-22"), y = 520,
  #               label = "11/03/2021\n248 resultados"), stat = "unique", family = "Lato Black",
  #           size = 5, color = "black")+
  theme_bw() + temaejes +
  theme(legend.text = element_text(family = "Lato", size = 7), legend.background = element_rect(fill="transparent"),
        legend.position = c(0.02,0.85),  legend.justification="left", legend.margin=margin(t = 0, unit='cm'),
        legend.key = element_rect(fill="transparent", color="transparent")) +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo:</span><br><span style = 'color:#4BACC6';>Resultados informados diariamente</span>", 
       subtitle= Fechahoy, caption =fuente)

PruebasSon

ggsave("Gráficos diarios/HMO/diariopruebas.png",PruebasSon, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)




library(directlabels)
Letalidad <- HMO.DF %>% ggplot(aes(x= Fecha, y= Letalidad)) +
  geom_line(color= "#993366", linetype= "solid", size=1, alpha=0.6)+
  geom_point( data = subset(HMO.DF , Fecha == max(Fecha)), fill="#993366", size=2 , shape=21, color="white", stroke=1) +
  geom_dl( data = subset(HMO.DF , Fecha == max(Fecha)), aes(label = Letalidad), color="#993366", method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 1.5, fontfamily= "Lato Black")) +
  scale_y_continuous(expand = c(0, 0), limits= c(0,20), breaks=seq(0,20,2)) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2021-01-01"), max(HMO.DF$Fecha)+40), date_breaks = "1 month", date_labels = "%B") +
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_bw() +
  temaejes +
  labs(y = "Decesos por cada 100 casos", 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo:</span><br><span style = 'color:#993366';>Letalidad acumulada</span>", 
       subtitle= Fechahoy, caption =fuente)  

Letalidad

ggsave("Gráficos diarios/HMO/s13.png",Letalidad,  width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)


#Hospitalizados

Hospitalizados <- HMO.DF %>% ggplot(aes(x= Fecha, y= Hospitalizados.Activos)) +
  geom_line(color= "#F79646", linetype= "solid", size=1, alpha=0.8)+
  geom_area(color= "transparent", fill= "#F79646",alpha=0.1)+
  geom_point( data = subset(HMO.DF , Fecha == max(Fecha)), fill="white", size=1 , shape=21, color="#F79646", stroke=1) +
  geom_dl( data = subset(HMO.DF , Fecha == max(Fecha)), aes(label = Hospitalizados.Activos), color="#F79646", method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 1.5, fontfamily= "Lato Black")) +
  scale_y_continuous(expand = c(0, 0), limits= c(0,20), breaks=seq(0,20,5)) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2021-01-01"), max(as.Date(HMO.DF$Fecha))+20), date_breaks = "1 month", date_labels = "%B") +
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_bw() +
  temaejes +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo:</span><br><span style = 'color:#F79646';>Casos activos hospitalizados al corte</span>", 
       subtitle= Fechahoy, caption =fuente)  

Hospitalizados

ggsave("Gráficos diarios/HMO/Hospitalizados 2.png",Hospitalizados,  width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)



Activos <- HMO.DF %>% ggplot() +
  geom_point(aes(x= Fecha, y= Ambulatorios.Activos), fill= "#58BCBC", color= "white", size = 0.9, stroke=0.4, alpha=0.65, shape = 21)+
  geom_line(aes(x=Fecha, y=Ambulatorios.Activos.7d, color = "Tendencia promedio móvil 7 días"), linetype= "solid", size=.75, arrow=arrow(type="open", length=unit(0.10,"cm")))+
  geom_area(aes(x=Fecha, y=Ambulatorios.Activos.7d),color= "transparent", fill= "#58BCBC",alpha=0.1)+
  geom_point( data = subset(HMO.DF , Fecha == max(Fecha)),aes(x= Fecha, y= Ambulatorios.Activos), fill="white", size=1 , shape=21, color="#58BCBC", stroke=1) +
  geom_dl( data = subset(HMO.DF , Fecha == max(Fecha)), aes(x= Fecha, y= Ambulatorios.Activos, label = Ambulatorios.Activos), color="#58BCBC", method = list(dl.trans(x = x + 0.2), "last.bumpup", cex = 1.5, fontfamily= "Lato Black")) +
  scale_color_manual(values= c("Tendencia promedio móvil 7 días"= "#58BCBC"))+
  scale_y_continuous(expand = c(0, 0), limits= c(0,120), breaks=seq(0,120,20)) +
  scale_x_date(expand=c(0,0), limits = c(as.Date("2021-01-01"), max(as.Date(HMO.DF$Fecha))+30), date_breaks = "1 month", date_labels = "%B") +
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_bw() +
  temaejes + 
  theme(legend.text = element_text(family = "Lato", size = 7), legend.background = element_rect(fill="transparent"),
        legend.position = c(0.02,0.9),  legend.justification="left", legend.margin=margin(t = 0, unit='cm'),
        legend.key = element_rect(fill="transparent", color="transparent")) +
  labs(y = NULL, 
       x = NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo:</span><br><span style = 'color:#58BCBC';>Pacientes activos con síntomas leves al corte</span>", 
       subtitle= Fechahoy, caption =fuente)  

Activos
ggsave("Gráficos diarios/HMO/diariosActivos.png",Activos,  width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)



# O2A <- HMO.DF %>% filter(Etapa=="Ascenso", OLA=="B") 
# O2A <- O2A %>% mutate(día=rownames(O2A)) %>% select(día, Fecha, Pruebas, Pruebas.diarias,Pruebas.media.7d, Confirmados, Casos.diarios, Casos.media.7d, Decesos, Decesos.diarios, Decesos.media.7d, Hospitalizados) %>% 
#   mutate(Casos.acumulados=cumsum(Casos.diarios))
# 
# O3A <- HMO.DF %>% filter(Etapa=="Ascenso", OLA=="C") 
# O3A <- O3A %>% mutate(día=rownames(O3A)) %>% select(día, Fecha, Pruebas, Pruebas.diarias,Pruebas.media.7d, Confirmados, Casos.diarios, Casos.media.7d, Decesos, Decesos.diarios, Decesos.media.7d, Hospitalizados) %>% 
# mutate(Casos.acumulados=cumsum(Casos.diarios))
# 
# OLAS <- O2A %>% left_join(O3A, by="día") %>% mutate(día=as.numeric(día))





# OLASG <- ggplot(OLAS) +
#   geom_line(aes(x= día, y= Casos.acumulados.x, color= "OLA 2", group=1), linetype= "solid", size=.75, arrow=arrow(type="open", length=unit(0.10,"cm")))+
#   geom_line(aes(x= día, y= Casos.acumulados.y, color= "OLA 3", group=1), linetype= "solid", size=.75, arrow=arrow(type="open", length=unit(0.10,"cm")))+
#   scale_color_manual(name="", values= c("OLA 2" = "#01787E", "OLA 3" = "#F79646")) +
#   #scale_x_date(expand=c(0,5), date_breaks = "1 month", date_labels = "%B") +
# theme_bw() + temaejes +
#   theme(legend.text = element_text(family = "Lato", size = 7), legend.background = element_rect(fill="transparent"),
#         legend.position = "none",  legend.justification="left", legend.margin=margin(t = 0, unit='cm'),
#         legend.key = element_rect(fill="transparent", color="transparent")) +
#   labs(y = NULL, 
#        x = "Días desde el inicio de la ola", legend= NULL, title  = "<span style = 'font-size:14pt'>Covid-19 en Hermosillo:</span><br>Casos acumulados confirmados en las etapas de<br>ascenso de la <span style = 'color:#01787E';>primera</span> y <span style = 'color:#F79646';>segunda</span> ola</span>", 
#        subtitle= Fechahoy, caption =fuente)
# 
# OLASG
# 
# ggsave("Gráficos diarios/OLASG.png",OLASG, width = 5 * (16/9), height = 5, type = "cairo", dpi = 400)
# 
