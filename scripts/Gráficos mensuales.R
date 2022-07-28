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
library(directlabels)
library(ggtext)
library(patchwork)
library(magick)

logo <- image_read("Shapes/SEDFesp.png")
logoD <- image_read("Shapes/SEDdecesos.png")
logoH <- image_read("Shapes/SEDhosp.png")
logoP <- image_read("Shapes/SEDpruebas.png")
logoS <- image_read("Shapes/SEDsosp.png")


fuente <- "Elaboración Luis Armando Moreno con información de la Secretaría de Salud del Gobierno de la República\n*Por fecha de reporte. | www.luisarmandomoreno.com"
temames <-  theme(axis.line.x = element_line(linetype = "solid"), axis.line.y = element_blank(),
                      plot.margin = margin(10, 25, 10, 25),
                      plot.title = element_markdown(family = "Lato Black", size = 18),  
                      plot.subtitle = element_text(family = "Lato Light", size = 10, color = "black"), legend.title = element_blank(),
                      axis.text.x = element_text(family = "Lato", size =12),   panel.grid= element_blank(),
                      axis.text.y = element_blank(),
                      plot.background = element_rect(fill = "white", color = "white", size = 3),
                      axis.title.x = element_text(family = "Lato Light", size = 8, hjust=1),
                      axis.title.y = element_text(family = "Lato Light", size = 8, hjust=1), 
                      plot.caption = element_text(family = "Lato", size = 6, color = "black"),
                      legend.text = element_text(family = "Lato", size = 8),
                      legend.position = "none",  legend.justification="left", plot.title.position = 'plot', plot.caption.position = 'plot')

Casos <- read_csv("Bases/Casosdiarios_SSFED.csv", 
                  col_types = cols(CASOS = col_integer(), 
                                   CVEGEO = col_character(), Fecha = col_date(format = "%Y-%m-%d"), 
                                   MUNICIPIO = col_character(), NUEVOS = col_integer(), X1 = col_skip()), 
                  locale = locale(encoding = "ISO-8859-1"))
Decesos <- read_csv("Bases/Decesosdiarios_SSFED.csv", 
                    col_types = cols(DECESOS = col_integer(), 
                                     CVEGEO = col_character(), Fecha = col_date(format = "%Y-%m-%d"), 
                                     MUNICIPIO = col_character(), NUEVOS = col_integer(), X1 = col_skip()), 
                    locale = locale(encoding = "ISO-8859-1"))
dia <- max(Casos$Fecha)
Fechahoy <- paste0("Al reporte del ", day(dia), " de ", months.Date(dia)," de ", year(dia))
Fechames <- "Confirmados acumulados por mes"

Casosmes <- Casos %>%mutate(mesnum=month(Fecha), mes = months.Date(Fecha),  año = year(Fecha)) %>% select(año, mesnum, mes, Fecha, NUEVOS)
Casosmes <- Casosmes %>% group_by(año, mesnum, mes) %>% summarise(Fecha=min(Fecha), casos.mensuales=sum(NUEVOS)) %>% select (Fecha, año, mesnum, mes, casos.mensuales) %>% 
  filter(Fecha<as.Date("2022-05-01")) %>% filter(Fecha>=as.Date("2021-04-01"))


Decesosmes <- Decesos %>%mutate(mesnum=month(Fecha), mes = months.Date(Fecha),  año = year(Fecha)) %>% select(año, mesnum, mes, Fecha, NUEVOS)
Decesosmes <- Decesosmes %>% group_by(año, mesnum, mes) %>% summarise(Fecha=min(Fecha), decesos.mensuales=sum(NUEVOS)) %>% select (Fecha, año, mesnum, mes, decesos.mensuales)%>% 
  filter(Fecha<as.Date("2022-05-01")) %>% filter(Fecha>=as.Date("2021-04-01"))



Casosmesson <- ggplot(Casosmes) +
  geom_col(aes(x=Fecha, y= casos.mensuales, fill= casos.mensuales), color= "#005156", size=0.3, width=20) +
  scale_fill_gradient2(low = "#DEF2F2", mid= "#01A2AC", high = "#005156", midpoint = 5000) +
  geom_text( data = subset(Casosmes, casos.mensuales<4000), aes(x=Fecha, y= casos.mensuales, label= scales::comma(casos.mensuales)), family="Lato Black", size= 4, color="black", angle=90, hjust = -0.2) +
  geom_text( data = subset(Casosmes,casos.mensuales>=4000), aes(x=Fecha, y= casos.mensuales, label= scales::comma(casos.mensuales)), family="Lato Black", size= 4, color="white", angle=90, hjust = 1.1) +
  # geom_text(aes(x = as.Date("2021-06-01"), y = 8000,
  #            label = "69,936 casos acumulados"), stat = "unique", family = "Lato Black",
  #            size = 5, color = "black", hjust =1) +
  scale_x_date(expand=c(0,5), date_breaks = "1 month", labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                                                                    paste(month(x, label = TRUE), "\n", year(x)), 
                                                                                    paste(month(x, label = TRUE)))) +
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_minimal() +
  temames +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'color:#01A2AC';>Casos mensuales</span>", 
       subtitle= Fechames, caption =NULL)
Casosmesson

ggsave("Gráficos diarios/MesCas.png",Casosmesson, width = 5 * (16/9), height = 5, type = "cairo", dpi = 300)

Decesosmesson <- ggplot(Decesosmes) +
  geom_col(aes(x=Fecha, y= decesos.mensuales, fill= decesos.mensuales), color= "#4D1933", size=0.3, width=20) +
  scale_fill_gradient2(low = "#F0D1E0", mid= "#993366", high = "#4D1933", midpoint = 600) +
  geom_text( data = subset(Decesosmes, decesos.mensuales<100), aes(x=Fecha, y= decesos.mensuales, label= decesos.mensuales), family="Lato Black", size= 4, color="black", angle=90, hjust = -0.2) +
  geom_text( data = subset(Decesosmes,decesos.mensuales>=100), aes(x=Fecha, y= decesos.mensuales, label= decesos.mensuales), family="Lato Black", size= 4, color="white", angle=90, hjust = 1.2) +
  # geom_text(aes(x = as.Date("2020-03-30"), y = 800,
  #               label = "5,991 decesos acumulados"), stat = "unique", family = "Lato Black",
  #           size = 5, color = "black", hjust =1) +
  scale_x_date(expand=c(0,5), date_breaks = "1 month", labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                                                                    paste(month(x, label = TRUE), "\n", year(x)), 
                                                                                    paste(month(x, label = TRUE)))) +
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(expand = FALSE, clip = 'off') +
  theme_minimal() +
  temames +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'color:#993366';>Decesos mensuales</span>", 
       subtitle= Fechames, caption =NULL)
Decesosmesson

ggsave("Gráficos diarios/MesDes.png",Decesosmesson, width = 5 * (16/9), height = 5, type = "cairo", dpi = 300)

patchwork <- (Casosmesson / Decesosmesson)
p3 <- patchwork + plot_annotation(
  title = paste0("<span style = 'font-size:12pt'>Covid-19 en Sonora:</span><br>","Resumen mensual"),
  subtitle = Fechahoy,
  caption = fuente, theme= theme(
    plot.title = element_markdown(family = "Lato Black", size = 30),  
    plot.subtitle = element_text(family = "Lato Light", size = 10, color = "black"),
    plot.caption = element_text(family = "Lato", size = 8), plot.title.position = 'plot', 
    plot.caption.position = 'plot', plot.margin = margin(10, 25, 10, 25), 
    plot.background = element_rect(fill = "white", color = "white", size = 3)))

p4 <- cowplot::ggdraw() +
  cowplot::draw_plot(p3,x = 0, y = 0, width = 1, height = 1) +
  cowplot::draw_image(logo, x = 0.85, y = 0.89, width = 0.1, height = 0.1)
p4

ggsave("Gráficos diarios/Mensual.png",p4, width = 5 * (16/9), height = 10, type = "cairo", dpi = 400)


Fechahoy <- "Al reporte del 30 de abril de 2022."
Fechames <- "Confirmados acumulados por mes"
fuente <- "Elaboración Luis Armando Moreno con información de la Secretaría de Salud del Gobierno de la República.\n*Por fecha de reporte. | www.luisarmandomoreno.com"
temames <-  theme(axis.line.y = element_line(linetype = "solid"), axis.line.x = element_blank(),
                  plot.margin = margin(10, 25, 10, 25),
                  plot.title = element_markdown(family = "Lato Black", size = 22),  
                  plot.subtitle = element_text(family = "Lato Light", size = 14, color = "black"), legend.title = element_blank(),
                  axis.text.y = element_text(family = "Lato", size =10),   panel.grid= element_blank(),
                  axis.text.x = element_blank(),
                  plot.background = element_rect(fill = "white", color = "white", size = 3),
                  axis.title.x = element_text(family = "Lato Light", size = 8, hjust=1),
                  axis.title.y = element_text(family = "Lato Light", size = 8, hjust=1), 
                  plot.caption = element_text(family = "Lato", size = 6, color = "black"),
                  legend.text = element_text(family = "Lato", size = 8),
                  legend.position = "none",  legend.justification="left", plot.title.position = 'plot', plot.caption.position = 'plot')



Casosmes <- Casos %>%mutate(mesnum=month(Fecha), mes = months.Date(Fecha),  año = year(Fecha)) %>% select(año, mesnum, mes, Fecha, NUEVOS)
Casosmes <- Casosmes %>% group_by(año, mesnum, mes) %>% summarise(Fecha=min(Fecha), casos.mensuales=sum(NUEVOS)) %>% select (Fecha, año, mesnum, mes, casos.mensuales) %>% 
  filter(Fecha<as.Date("2022-05-01")) %>% filter(Fecha>=as.Date("2021-04-01"))


Decesosmes <- Decesos %>%mutate(mesnum=month(Fecha), mes = months.Date(Fecha),  año = year(Fecha)) %>% select(año, mesnum, mes, Fecha, NUEVOS)
Decesosmes <- Decesosmes %>% group_by(año, mesnum, mes) %>% summarise(Fecha=min(Fecha), decesos.mensuales=sum(NUEVOS)) %>% select (Fecha, año, mesnum, mes, decesos.mensuales)%>% 
  filter(Fecha<as.Date("2022-05-01")) %>% filter(Fecha>=as.Date("2021-04-01"))

Casosmes <- Casosmes %>% left_join(Decesosmes, by=c("año", "Fecha", "mesnum", "mes"))
Casosmes[is.na(Casosmes)] <- 0
Casosmesson <- ggplot(Casosmes) +
  geom_col(aes(x=Fecha, y= casos.mensuales, fill= casos.mensuales), color= "#005156", size=0.3, width=20) +
  scale_fill_gradient2(low = "#DEF2F2", mid= "#01A2AC", high = "#005156", midpoint = 5000) +
  geom_text( data = subset(Casosmes, casos.mensuales<4000), aes(x=Fecha, y= casos.mensuales, label= scales::comma(casos.mensuales)), family="Lato Black", size= 4, color="black",  hjust = -0.2) +
  geom_text( data = subset(Casosmes, casos.mensuales>=4000), aes(x=Fecha, y= casos.mensuales, label= scales::comma(casos.mensuales)), family="Lato Black", size= 4, color="white", hjust = 1.1) +
  # geom_text(aes(x = as.Date("2021-06-01"), y = 8000,
  #            label = "69,936 casos acumulados"), stat = "unique", family = "Lato Black",
  #            size = 5, color = "black", hjust =1) +
  scale_x_date(expand=c(0,5), date_breaks = "1 month", labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                                                                    paste(month(x, label = TRUE), "\n", year(x)), 
                                                                                    paste(month(x, label = TRUE)))) +
  scale_y_continuous(expand=c(0,0))+
  theme_minimal() +
  temames +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'color:#01A2AC';>Casos mensuales</span>", 
       subtitle= Fechames, caption =NULL) +
  coord_flip() 
Casosmesson


Decesosmesson <- ggplot(Casosmes) +
  geom_col(aes(x=Fecha, y= decesos.mensuales, fill= decesos.mensuales), color= "#4D1933", size=0.3, width=20) +
  scale_fill_gradient2(low = "#F0D1E0", mid= "#993366", high = "#4D1933", midpoint = 600) +
  geom_text( data = subset(Casosmes, decesos.mensuales<100), aes(x=Fecha, y= decesos.mensuales, label= decesos.mensuales), family="Lato Black", size= 4, color="black", hjust = -0.2) +
  geom_text( data = subset(Casosmes,decesos.mensuales>=100), aes(x=Fecha, y= decesos.mensuales, label= decesos.mensuales), family="Lato Black", size= 4, color="white", hjust = 1.2) +
  # geom_text(aes(x = as.Date("2020-03-30"), y = 800,
  #               label = "5,991 decesos acumulados"), stat = "unique", family = "Lato Black",
  #           size = 5, color = "black", hjust =1) +
  scale_x_date(expand=c(0,5), date_breaks = "1 month", labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                                                                    paste(month(x, label = TRUE), "\n", year(x)), 
                                                                                    paste(month(x, label = TRUE)))) +
  scale_y_continuous(expand=c(0,0))+
  theme_minimal() +
  temames +
  labs(y = NULL, 
       x = NULL,legend= NULL, title  = "<span style = 'color:#993366';>Decesos mensuales</span>", 
       subtitle= Fechames, caption =NULL) +
  coord_flip() 
Decesosmesson



patchwork <- (Casosmesson | Decesosmesson)
p3 <- patchwork + plot_annotation(
  title = paste0("<span style = 'font-size:12pt'>Covid-19 en Sonora:</span><br>","Resumen mensual"),
  subtitle = Fechahoy,
  caption = fuente, theme= theme(
    plot.title = element_markdown(family = "Lato Black", size = 30),  
    plot.subtitle = element_text(family = "Lato Light", size = 12, color = "black"),
    plot.caption = element_text(family = "Lato", size = 8), plot.title.position = 'plot', 
    plot.caption.position = 'plot', plot.margin = margin(10, 25, 10, 25), 
    plot.background = element_rect(fill = "white", color = "white", size = 3)))

p3

p4 <- cowplot::ggdraw() +
  cowplot::draw_plot(p3,x = 0, y = 0, width = 1, height = 1) +
  cowplot::draw_image(logo, x = 0.85, y = 0.89, width = 0.1, height = 0.1)     

 
ggsave("Gráficos diarios/Mensual_flip.png",p4, width = 5 * (16/9), height = 10, type = "cairo", dpi = 400)


Casos <- read_csv("Bases/Casosdiarios_SSFED.csv", 
                  col_types = cols(CASOS = col_integer(), 
                                   CVEGEO = col_character(), Fecha = col_date(format = "%Y-%m-%d"), 
                                   MUNICIPIO = col_character(), NUEVOS = col_integer(), X1 = col_skip()), 
                  locale = locale(encoding = "ISO-8859-1")) %>% filter(MUNICIPIO=="Hermosillo")
Decesos <- read_csv("Bases/Decesosdiarios_SSFED.csv", 
                    col_types = cols(DECESOS = col_integer(), 
                                     CVEGEO = col_character(), Fecha = col_date(format = "%Y-%m-%d"), 
                                     MUNICIPIO = col_character(), NUEVOS = col_integer(), X1 = col_skip()), 
                    locale = locale(encoding = "ISO-8859-1")) %>% filter(MUNICIPIO=="Hermosillo")
dia <- max(Casos$Fecha)
Fechahoy <- paste0("Al reporte del ", day(dia), " de ", months.Date(dia)," de ", year(dia))
Fechames <- "Confirmados acumulados por mes"

Casosmes <- Casos %>%mutate(mesnum=month(Fecha), mes = months.Date(Fecha),  año = year(Fecha)) %>% select(año, mesnum, mes, Fecha, NUEVOS)
Casosmes <- Casosmes %>% group_by(año, mesnum, mes) %>% summarise(Fecha=min(Fecha), casos.mensuales=sum(NUEVOS)) %>% select (Fecha, año, mesnum, mes, casos.mensuales) %>% 
  filter(Fecha<as.Date("2022-05-01")) %>% filter(Fecha>=as.Date("2021-04-01"))


Decesosmes <- Decesos %>%mutate(mesnum=month(Fecha), mes = months.Date(Fecha),  año = year(Fecha)) %>% select(año, mesnum, mes, Fecha, NUEVOS)
Decesosmes <- Decesosmes %>% group_by(año, mesnum, mes) %>% summarise(Fecha=min(Fecha), decesos.mensuales=sum(NUEVOS)) %>% select (Fecha, año, mesnum, mes, decesos.mensuales)%>% 
  filter(Fecha<as.Date("2022-05-01")) %>% filter(Fecha>=as.Date("2021-04-01"))
Casosmes
sum(Casosmes$casos.mensuales)
