pacman::p_load("tidyverse", "ggtext", "here", "lubridate", "zoo")


rm(list=ls())
fuentenyt <- "Elaboración Luis Armando Moreno (@dogomoreno) siguiendo el código de @theneilrichards inspirado en The New York Times\ncon información de la Secretaría de Salud del Gobierno de la República y la Secretaría de Salud Estatal\n*Por fecha de reporte | www.luisarmandomoreno.com"

Sonora.DF <- read_csv("Bases/ST_SonoraReporte_SSFED.csv", 
                      col_types = cols(fecha_reporte = col_date(format = "%Y-%m-%d")))

dia<-max(as.Date(Sonora.DF$fecha_reporte))

Fechahoy <- paste0("Al reporte del ", day(dia), " de ", months.Date(dia)," de ", year(dia),".")
fuente <- "Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud del Gobierno de la República\n*Por fecha de reporte | www.luisarmandomoreno.com"
fuenteprueba <- "Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud del Gobierno de la República\n*Incluye resultados válidos de pruebas PCR y antigénicas. | www.luisarmandomoreno.com"
fuenteact <- "Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud del Gobierno de la República\n*Se consideran casos activos aquellos que iniciaron síntomas dentro de los 14 días previos al corte. | www.luisarmandomoreno.com"
subtitulo <- paste0("Casos confirmados en los últimos 7 días por 100 mil habitantes\nAl reporte del ",day(dia),"/",month(dia),"/",year(dia))
subtituloact <- paste0("Casos activos (que iniciaron síntimas dentro de los 14 días previos) por 100 mil habitantes.\nAl reporte del ",day(dia),"/",month(dia),"/",year(dia))

fuentefech <- "Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud del Gobierno de la República\n*El registro de los últimos 14 días aún se están alimentando en el sistema | www.luisarmandomoreno.com"


temasmap <-  theme(axis.line = element_blank(),
                   plot.margin = margin(10, 10, 10, 10),
                   plot.title = element_markdown(family = "Lato Black", size = 20),  
                   plot.subtitle = element_text(family = "Lato Light", size = 8, color = "black"), legend.title = element_blank(),
                   axis.text= element_blank(),
                   plot.background = element_rect(fill = "white", color = "white", size = 3),
                   axis.title= element_blank(), 
                   plot.caption = element_text(family = "Lato", size = 6, color = "black"),
                   legend.text = element_text(family = "Lato", size = 6),
                   legend.position = "none",  legend.justification="left", plot.title.position = 'plot', plot.caption.position = 'plot')


Sonora.DF <- read_csv("Bases/ST_SonoraReporte_SSFED.csv", 
                      col_types = cols(fecha_reporte = col_date(format = "%Y-%m-%d")))
Sonora.DF <- rename(Sonora.DF, Fecha=fecha_reporte)
Sonora.DF <- mutate(Sonora.DF, Casos.diarios= Casos - lag(Casos, default = Casos[1], order_by=Fecha))
Sonora.DF <- mutate(Sonora.DF, Confirmados.diarios= Confirmados_lab - lag(Confirmados_lab, default = Confirmados_lab[1], order_by=Fecha))
Sonora.DF <- mutate(Sonora.DF, Hospitalizados.diarios= Hospitalizados - lag(Hospitalizados, default = Casos[1], order_by=Fecha))
Sonora.DF <- mutate(Sonora.DF, Decesos.diarios= Decesos - lag(Decesos, default = Decesos[1], order_by=Fecha))
Sonora.DF <- mutate(Sonora.DF, Casos.media.7d=round(rollmeanr(x=Casos.diarios, 7, fill = NA),1))
Sonora.DF <- mutate(Sonora.DF, Decesos.media.7d=round(rollmeanr(x=Decesos.diarios,7, fill=NA),1))
Sonora.DF <- mutate(Sonora.DF, Hospitalizados.media.7d=round(rollmeanr(x=Hospitalizados.diarios, 7, fill = NA),1))
Sonora.DF <- mutate(Sonora.DF, Resultados.diarios= Resultados_lab - lag(Resultados_lab, default = Resultados_lab [1]))
Sonora.DF <- mutate(Sonora.DF, Resultados.media.7d=round(rollmeanr(x=Resultados.diarios,7, fill=NA),1))
Sonora.DF <- mutate(Sonora.DF, Ambulatorios.Activos.7d=round(rollmeanr(x=Ambulatorios.Activos,7, fill=NA),1))
Sonora.DF <- mutate(Sonora.DF, Sospechosos.7d=round(rollmeanr(x=Sospechosos,7, fill=NA),1))
Sonora.DF <- mutate(Sonora.DF, Sospechosos.diarios= Sospechosos - lag(Sospechosos, default = Sospechosos[1], order_by=Fecha))
Sonora.DF <- mutate(Sonora.DF, Sospechosos.diarios.7d=round(rollmeanr(x=Sospechosos.diarios,7, fill=NA),1))
Sonora.DF <- mutate(Sonora.DF, Hospitalizados.Activos.7d=round(rollmeanr(x=Hospitalizados.Activos,7, fill=NA),1))
Sonora.DF <- mutate(Sonora.DF, Incidencia= round((Casos / 30.74745),2))
Sonora.DF <- mutate(Sonora.DF, Letalidad= round((Decesos / Casos)*100,1))
Sonora.DF <- mutate(Sonora.DF, Mortalidad= round((Decesos / 30.74745)*100,2))
Sonora.DF <- mutate(Sonora.DF, Positividad.diaria= round((Confirmados.diarios / Resultados.diarios)*100,2))
Sonora.DF <- mutate(Sonora.DF, Positividad= round((Confirmados_lab / Resultados_lab )*100,2))
Sonora.DF <- mutate(Sonora.DF, Analizados.diarios= Analizados - lag(Analizados, default = Analizados[1], order_by=Fecha))
Sonora.DF <- mutate(Sonora.DF, Analizados.media.7d=round(rollmeanr(x=Analizados.diarios, 7, fill = NA),1))

estata.hoy <- Sonora.DF %>% filter(Fecha==max(as.Date(Fecha)))
estata.semana <- Sonora.DF %>% filter(Fecha==(max(as.Date(Fecha))-7))


#Casos diarios Estatal

Sonora.ED <- read_csv("Bases/ST_SonoraInformesCOVID_SSEDO.csv", 
                      col_types = cols(fecha_corte = col_date(format = "%d/%m/%Y")))
Sonora.ED <- rename(Sonora.ED, Fecha_reporte= Fecha)
Sonora.ED <- rename(Sonora.ED, Fecha=fecha_corte)
Sonora.ED <- mutate(Sonora.ED, Casos.diarios= Casos - lag(Casos, default = Casos[1], order_by=Fecha))
Sonora.ED <- mutate(Sonora.ED, Decesos.diarios= Decesos_imp - lag(Decesos_imp, default = Decesos_imp[1], order_by=Fecha))
Sonora.ED <- select(Sonora.ED, Fecha, Casos.diarios, Decesos.diarios)

CasosPos <- Sonora.DF %>% filter(Fecha>as.Date("2021-09-19")) %>% 
  select(Fecha, Casos.diarios, Decesos.diarios) 

Sonora.ED <- Sonora.ED %>% bind_rows(CasosPos)
Sonora.ED <- mutate(Sonora.ED, Casos.media.7d=round(rollmeanr(x=Casos.diarios, 7, fill = NA),1),
                    Decesos.media.7d=round(rollmeanr(x=Decesos.diarios, 7, fill = NA),1))
Sonora.ED[is.na(Sonora.ED)] <- 0

covid_cases <- Sonora.ED %>%
  arrange(Fecha) %>% 
  # Add the dates before the 1st confirmed case
  add_row(Fecha = as_date("2020-01-01"), Casos.diarios = 0, Casos.media.7d = 0, Decesos.diarios = 0, Decesos.media.7d = 0,
          .before = 1) %>% 
  complete(Fecha = seq(min(.$Fecha), max(.$Fecha), by = 1),
           fill = list(Casos.diarios = 0, Casos.media.7d = 0, Decesos.diarios = 0, Decesos.media.7d = 0)) %>% 
  mutate(day_of_year = yday(Fecha),
         year = year(Fecha)
  )

p <- covid_cases %>% 
  ggplot() +
  geom_segment(aes(x = day_of_year, xend = day_of_year + 1, 
                   y = as.POSIXct(Fecha), yend = as.POSIXct(Fecha))) +
  coord_polar()
p

p + theme_void()



size_factor <- 40000

# Colors
outline_color <- "#01A2AC"
fill_color <- alpha("#01A2AC",0.3)
base_grey <- "grey28"

p <- covid_cases %>% 
  ggplot() +
  # area to encode the number of cases
  geom_ribbon(aes(x = day_of_year, 
                  ymin = as.POSIXct(Fecha) - Casos.media.7d / 2 * size_factor,
                  ymax = as.POSIXct(Fecha) + Casos.media.7d / 2 * size_factor,
                  group = year),
              size = 0.3, col = outline_color, fill = fill_color, show.legend = FALSE) +
  # basic line
  geom_segment(aes(x = day_of_year, xend = day_of_year + 1, 
                   as.POSIXct(Fecha), yend = as.POSIXct(Fecha)),
               col = base_grey, size = 0.3) +
  coord_polar() +
  theme_void()
p


month_length <- c(31, 28, 31, 30, 31, 30,
                  31, 31, 30, 31, 30, 31)

month_breaks <- cumsum(month_length) - 30


p + scale_x_continuous(minor_breaks = month_breaks, 
                       breaks = month_breaks[c(1, 4, 7, 10)],
                       labels = c("Jan.", "April", "July", "Oct.")) +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    panel.grid.major.x = element_line(color = "grey70", size = 0.2, linetype = "dotted"),
    panel.grid.minor.x = element_line(color = "grey70", size = 0.2, linetype = "dotted"),
    axis.text.x = element_text(color = base_grey, size = 5, hjust = 0.5),
  )


text_color <- rgb(18, 18, 18, maxColorValue = 255)
base_family <- "Lato Black"
# base_family <- "Helvetica"
subtitle_date <- max(covid_cases$Fecha) %>% 
  format("%b %d, %Y")

# Annotations for the years in a list (used in annotate())
year_annotations <- list(
  year = 2020:2022,
  x = rep(3, 3),
  y = as.POSIXct(paste(2020:2022, "01", "01", sep = "-"))
)

size_factor <- 40000

p <- covid_cases %>% 
  # 2020 is a leap year, we could drop Feb 29, 2020 for the sake of 365-day years
  filter(Fecha != as_date("2020-02-29")) %>%
  group_by(year) %>%
  mutate(day_of_year = row_number()) %>%
  ungroup() %>%
  ggplot() +
  # area
  geom_ribbon(aes(x = day_of_year, 
                  ymin = as.POSIXct(Fecha) - Casos.media.7d / 2 * size_factor,
                  ymax = as.POSIXct(Fecha) + Casos.media.7d / 2 * size_factor,
                  group = year),
              color = outline_color, size = 0.3, fill = fill_color, show.legend = FALSE) +
  # basic line
  geom_segment(aes(x = day_of_year, xend = day_of_year + 1, 
                   y = as.POSIXct(Fecha), yend = as.POSIXct(Fecha)),
               col = base_grey, size = 0.3) +
  annotate("richtext", 
           label = "Promedio móvil<br>7 días",
           x = 20, y = as.POSIXct("2021-08-01"),
           family = base_family, size = 2, color = text_color,
           label.colour = NA, fill = NA) +
  annotate("segment",
           x = 20, xend = 22.5, 
           y = as.POSIXct("2021-06-01"), yend = as.POSIXct("2021-03-15"),
           color = text_color, size = 0.3) +
    # annotation: years
  annotate("text", label = paste0(year_annotations$year, "\u2192"), x = year_annotations$x, 
           y = year_annotations$y, 
           family = "Arial",
           size = 1.5, vjust = -0.6, hjust = 0.1) +   
  #' set the lower limit of the y-axis to a date before 2020 
  #' so that the spiral does not start in the center point
  labs(
    subtitle = subtitle_date, caption = fuentenyt
  ) +
  theme_void(base_family = base_family) +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    panel.grid.major.x = element_line(color = "grey70", size = 0.2, linetype = "dotted"),
    panel.grid.minor.x = element_line(color = "grey70", size = 0.2, linetype = "dotted"),
    axis.text.x = element_text(color = base_grey, size = 5, hjust = 0.5),
    text = element_text(color = text_color),
    plot.subtitle = element_text(hjust = 0.5, size = 5),
    plot.caption = element_text(family = "Lato", size =5), 
    plot.caption.position = 'plot',plot.margin = margin(10, 10, 10, 10)
  ) +
  scale_x_continuous(minor_breaks = month_breaks,
                     breaks = month_breaks[c(1,2,3,4,5,6,7,8,9,10,11,12)],
                     labels = c("Ene.","","", "Abr.","","", "Jul","","", "Oct.","",""),
                     limits = c(1, 365),
                     expand = c(0, 0)
  ) +
  #' set the lower limit of the y-axis to a date before 2020 
  #' so that the spiral does not start in the center point
  scale_y_continuous(limits = c(as.POSIXct("2019-07-01"), NA),
                     expand = c(0, 0)) +
  coord_polar()
p



library(patchwork)

p_legend <- 
  tibble(
    cases = c(0, 300),
    ymin = c(0, -150),
    ymax = c(0, 150),
  ) %>% 
  ggplot(aes(cases)) +
  geom_ribbon(aes(ymin = size_factor * ymin, ymax = size_factor * ymax),
              color = outline_color, fill = fill_color, size = 0.3) +
  geom_line(aes(y = 1), color = base_grey, size=0.2) +
  geom_text(aes(label = ifelse(cases == 0, 0, "300 casos"), 
                y = 1, hjust = ifelse(cases == 0, 1.5, -0.1)),
            size = 2) +
  coord_cartesian(xlim = c(0,600), 
                  ylim = c(-as.numeric(as.POSIXct("1971-01-01")), NA), 
                  clip = "off") + 
  labs(title = "<span style = 'font-size:8pt'>Covid-19 en Sonora</span><br><span style = 'color:#01A2AC';>Casos confirmados por día<br></span>") +
  theme_void() +
  theme(plot.title = element_markdown(color = text_color, 
                                      family = "Lato Black",
                                      face = "bold", size = 10,
                                      lineheight = 1.1))

plb <- p + inset_element(p_legend, left = 0.05, bottom = 0.725, right = 0.25, top = 0.97)
ggsave("Gráficos diarios/diarionyte.png",plb, width = 5 , height = 6, type = "cairo", dpi = 400)


size_factor <- 32000

# Colors
outline_color <- "#01A2AC"
fill_color <- alpha("#01A2AC",0.3)
base_grey <- "grey28"

outline_color_d <- "#993366"
fill_color_d <- alpha("#993366",0.3)
base_grey <- "grey28"
p <- covid_cases %>% 
  # 2020 is a leap year, we could drop Feb 29, 2020 for the sake of 365-day years
  filter(Fecha != as_date("2020-02-29")) %>%
  group_by(year) %>%
  mutate(day_of_year = row_number()) %>%
  ungroup() %>%
  ggplot() +
  # area
  geom_ribbon(aes(x = day_of_year, 
                  ymin = as.POSIXct(Fecha),
                  ymax = as.POSIXct(Fecha) + Casos.media.7d * size_factor,
                  group = year),
              color = outline_color, size = 0.3, fill = fill_color, show.legend = FALSE) +
  geom_ribbon(aes(x = day_of_year, 
                  ymin = as.POSIXct(Fecha) - Decesos.media.7d * 10  * size_factor,
                  ymax = as.POSIXct(Fecha),
                  group = year),
              color = outline_color_d, size = 0.3, fill = fill_color_d, show.legend = FALSE) +
  # basic line
  geom_segment(aes(x = day_of_year, xend = day_of_year + 1, 
                   y = as.POSIXct(Fecha), yend = as.POSIXct(Fecha)),
               col = base_grey, size = 0.3) +
  # annotate("richtext", 
  #          label = "Promedio móvil<br>7 días",
  #          x = 20, y = as.POSIXct("2021-08-01"),
  #          family = base_family, size = 2, color = text_color,
  #          label.colour = NA, fill = NA) +
  # annotate("segment",
  #          x = 20, xend = 22.5, 
  #          y = as.POSIXct("2021-06-01"), yend = as.POSIXct("2021-03-15"),
  #          color = text_color, size = 0.3) +
  # annotation: years
  
  annotate("text", label = paste0(year_annotations$year, "\u2192"), x = year_annotations$x, 
           y = year_annotations$y, 
           family = "Arial",
           size = 1.5, vjust = -0.6, hjust = 0.1) +   
  #' set the lower limit of the y-axis to a date before 2020 
  #' so that the spiral does not start in the center point
  labs(
    subtitle = subtitle_date, caption = fuentenyt
  ) +
  theme_void(base_family = base_family) +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    panel.grid.major.x = element_line(color = "grey70", size = 0.2, linetype = "dotted"),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(color = base_grey, size = 5, hjust = 0.5, family="Lato Black"),
    text = element_text(color = text_color),
    plot.subtitle = element_text(hjust = 0.5, size = 5),
    plot.caption = element_text(family = "Lato", size =5), 
    plot.caption.position = 'plot'  ) +
  scale_x_continuous(minor_breaks = month_breaks,
                     breaks = month_breaks[c(1,4,7,10)],
                     labels = c("Enero", "Abril","Julio", "Octubre"),
                     limits = c(1, 365),
                     expand = c(0, 0)
  ) +
  #' set the lower limit of the y-axis to a date before 2020 
  #' so that the spiral does not start in the center point
  scale_y_continuous(limits = c(as.POSIXct("2019-07-01"), NA),
                     expand = c(0, 0)) +
  coord_polar()
p



library(patchwork)

p_legend <- 
  tibble(
    cases = c(0, 200),
    ymin = c(0, -100),
    ymax = c(0, 100),
  ) %>% 
  ggplot(aes(cases)) +
  geom_ribbon(aes(ymin = 0, ymax = size_factor * ymax),
              color = outline_color, fill = fill_color, size = 0.3) +
  geom_ribbon(aes(ymin = size_factor * ymin, ymax = 0),
              color = outline_color_d, fill = fill_color_d, size = 0.3) +
  geom_line(aes(y = 1), color = base_grey, size=0.2) +
  geom_text(aes(label = ifelse(cases == 0, 0, "100 casos\n10 decesos"), 
                y = 1, hjust = ifelse(cases == 0, 1.5, -0.1)),
            size = 2, family="Lato Light") +
  coord_cartesian(xlim = c(0,800), 
                  ylim = c(-as.numeric(as.POSIXct("1971-01-01")), NA), 
                  clip = "off") + 
  labs(title = "<span style = 'font-size:8pt'>Covid-19 en Sonora</span><br><span style = 'font-size:18pt; color:#01A2AC';>Casos</span> y <span style = 'font-size:18pt;color:#993366';>decesos</span>",
       subtitle="Promedio móvil a 7 días de<br>confirmados por día de reporte<br>") +
  theme_void() +
  theme(plot.title = element_markdown(color = text_color, 
                                      family = "Lato Black",
                                      face = "bold", size = 10,
                                      lineheight = 1.1),
        plot.subtitle = element_markdown(color = text_color, 
                                      family = "Lato",
                                      size = 6,
                                      lineheight = 1.1))

plb <- p + inset_element(p_legend, left = 0.03, bottom = 0.6, right = 0.25, top = 1)
plb
ggsave("Gráficos diarios/diariocasosdesnyt.png",plb, width = 5 , height = 5, type = "cairo", dpi = 400)

outline_color <- "#01A2AC"
fill_color <- alpha("#01A2AC",0.3)
base_grey <- "grey28"

outline_color_d <- "#993366"
fill_color_d <- alpha("#993366",0.3)
base_grey <- "grey28"
p <- covid_cases %>% 
  # 2020 is a leap year, we could drop Feb 29, 2020 for the sake of 365-day years
  filter(Fecha != as_date("2020-02-29")) %>%
  group_by(year) %>%
  mutate(day_of_year = row_number()) %>%
  ungroup() %>%
  ggplot() +
  # area
  geom_ribbon(aes(x = day_of_year, 
                  ymin = as.POSIXct(Fecha),
                  ymax = as.POSIXct(Fecha) + Casos.media.7d * size_factor,
                  group = year),
              color = "black", size = 0.3, fill = "black", show.legend = FALSE) +
  geom_ribbon(aes(x = day_of_year, 
                  ymin = as.POSIXct(Fecha) - Decesos.media.7d * 10  * size_factor,
                  ymax = as.POSIXct(Fecha),
                  group = year),
              color = "black", size = 0.3, fill = "black", show.legend = FALSE) +
  # basic line

  #' set the lower limit of the y-axis to a date before 2020 
  #' so that the spiral does not start in the center point
  theme_void(base_family = base_family) +
  scale_x_continuous(minor_breaks = month_breaks, 
                     breaks = month_breaks[c(1, 4, 7, 10)],
                     labels = c("Enero", "Abril", "Julio", "Octubre"),
                     limits = c(1, 365),
                     expand = c(0, 0)
  ) +
  #' set the lower limit of the y-axis to a date before 2020 
  #' so that the spiral does not start in the center point
  scale_y_continuous(limits = c(as.POSIXct("2019-07-01"), NA),
                     expand = c(0, 0)) +
  coord_polar()
p
ggsave("Gráficos diarios/espblack.png",p, width = 5 , height = 5, type = "cairo", dpi = 400, bg="transparent")





library(tidyverse)
library(curl)
library(ggtext)
library(RcppRoll)
library(paletteer)
library(lubridate)


data2 <- Sonora.ED %>% 
  rename(date=Fecha) %>% 
  arrange(date) %>% 
  mutate(adm_roll=roll_mean(Casos.diarios, 7, align="right", fill=NA),
         #Create variable to represent the base of each bar - change the number to tighten/relax the spiral
         increment=1.7*c(1:n()),
         #Add cases to the base to get the top of each bar
         incrementadm=increment+adm_roll,
         year=year(date)) %>% 
  #Calculate the number of days since the start of the year
  group_by(year) %>% 
  mutate(yeardays=as.numeric(difftime(date ,as.Date(paste0(year, "-01-01")) , units = c("days")))) %>% 
  ungroup() %>% 
  #2020 being a leap year messes things up, so remove 31st December 2020 (arbitrarily) 
  #to make all the years the same length
  filter(!yeardays==365)

fuentenyt2 <- "Elaboración Luis Armando Moreno (@dogomoreno) siguiendo el código de @VictimOfMaths inspirado en The New York Times\ncon información de la Secretaría de Salud del Gobierno de la República y la Secretaría de Salud Estatal\n*Por fecha de reporte | www.luisarmandomoreno.com"
subtitulo <- paste0("Promedio móvil a 7 días de los casos confirmados diariamente\nAl reporte del ",day(dia),"/",month(dia),"/",year(dia))
ggplot()+
  #Need to plot each year separately
  geom_boxplot(data=data2 %>% filter(year=="2020" & ! is.na(adm_roll)), 
               aes(x=yeardays, ymin=increment, ymax=incrementadm, colour=adm_roll, 
                   fill=adm_roll, lower=increment, upper=incrementadm, middle=increment, 
                   group=date), stat = 'identity', show.legend=FALSE)+
  geom_boxplot(data=data2 %>% filter(year=="2021"), 
               aes(x=yeardays, ymin=increment, ymax=incrementadm, colour=adm_roll, 
                   fill=adm_roll, lower=increment, upper=incrementadm, middle=increment, 
                   group=date), stat = 'identity', show.legend=FALSE)+
  geom_boxplot(data=data2 %>% filter(year=="2022" & ! is.na(adm_roll)), 
               aes(x=yeardays, ymin=increment, ymax=incrementadm, colour=adm_roll, 
                   fill=adm_roll, lower=increment, upper=incrementadm, middle=increment, 
                   group=date), stat = 'identity', show.legend=FALSE)+
  #negative offset is to cover up the base of the bars
  geom_line(data=data2 %>% filter(year=="2020" & ! is.na(adm_roll)),
            aes(x=yeardays, y=increment-50), colour="black")+
  geom_line(data=data2 %>% filter(year=="2021"),
            aes(x=yeardays, y=increment-50), colour="black")+
  geom_line(data=data2 %>% filter(year=="2022"),
            aes(x=yeardays, y=increment-50), colour="black")+
  scale_x_continuous(breaks=c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334),
                     labels=c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep",
                              "Oct", "Nov", "Dec"))+
  scale_fill_gradient2(low = "#DEF2F2", mid= "#01A2AC", high = "black", midpoint = 300) +
  scale_color_gradient2(low = "#DEF2F2", mid= "#01A2AC", high = "black", midpoint = 300) +
  coord_polar()+
  theme_void()+
  temasmap +
  theme(panel.grid.major.x=element_line(color = "grey70", size = 0.2, linetype = "dotted"),
        axis.text.x=element_text(colour="black", family="Lato Black", size=5,hjust = 0.5))+
  #Add low key legend for a bit of context
  geom_segment(aes(y=1145.8, yend=1145.8+1000, x=yday(Sys.Date()+2), xend=yday(Sys.Date()+2)), colour="Grey30",
               arrow = arrow(length=unit(0.10,"cm"), ends="both", type = "closed"))+
  annotate("text", x=yday(Sys.Date()+6), y=1145.8+500, label="1000\ncasos\ndiarios", hjust=0, colour="Grey30",
           size=rel(2.5), family="Lato")+
  labs(title= "<span style = 'font-size:12pt'>Covid-19 en Sonora</span><br><span style = 'color:#01A2AC';>Casos confirmados diariamente</span>",
       subtitle=subtitulo,
       caption=fuentenyt2)
ggsave("Gráficos diarios/diarionyte2.png", width = 5.8/2 * (16/9), height = 6, type = "cairo", dpi = 400)

