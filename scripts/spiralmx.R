pacman::p_load("tidyverse", "ggtext", "lubridate", "zoo", "patchwork")


rm(list=ls())
fuentenyt <- "Elaboración Luis Armando Moreno (@dogomoreno) siguiendo el código de @_ansgar inspirado en The New York Times\ncon información de la Secretaría de Salud del Gobierno de la República\nwww.luisarmandomoreno.com"


Casosdiarios <- read_csv("Bases/Nacional_confirmados.csv",
                         locale = locale(encoding = "ISO-8859-1"))
Casosdiarios[is.na(Casosdiarios)] <- 0
Casosdiarios<- Casosdiarios %>%
  gather( key= "Fecha", value= "Casos.diarios", ends_with(c("2020","2021","2022"))) %>%
  mutate(Fecha = as.Date(Fecha,format = "%d/%m/%Y")) %>% 
  group_by(Fecha, cve_ent) %>%
  select(Fecha, cve_ent, Casos.diarios)

# Decesos Diarios
Decesosdiarios <- read_csv("Bases/Nacional_defunciones.csv",
                         locale = locale(encoding = "ISO-8859-1"))
Decesosdiarios[is.na(Decesosdiarios)] <- 0
Decesosdiarios<- Decesosdiarios %>%
  gather( key= "Fecha", value= "Decesos.diarios", ends_with(c("2020","2021","2022"))) %>%
  mutate(Fecha = as.Date(Fecha,format = "%d/%m/%Y")) %>% 
  group_by(Fecha, cve_ent) %>%
  select(Fecha, cve_ent, Decesos.diarios)



Confirmadosdiarios <-Casosdiarios %>% 
  left_join(Decesosdiarios, by=c("Fecha", "cve_ent"))
Confirmadosdiarios[is.na(Confirmadosdiarios)] <- 0

Confirmadosdiarios <- Confirmadosdiarios %>% 
  group_by(cve_ent) %>% 
  mutate(Casos.media.7d=round(rollmeanr(x=Casos.diarios, 7, fill = NA),1),
         Decesos.media.7d=round(rollmeanr(x=Decesos.diarios, 7, fill = NA),1)) %>% 
    ungroup()
Confirmadosdiarios[is.na(Confirmadosdiarios)] <- 0

Estatal_factor <- read_csv("Bases/Estatal_factor.csv", 
                           col_types = cols(cve_ent = col_character()), 
                           locale = locale(encoding = "ISO-8859-1"))


plot_espiral <- function(edo = "99") {

factor <- Estatal_factor %>% filter(cve_ent==edo)
size_factor <- factor$size_factor
nombre <- factor$nombre

confirmados <-Confirmadosdiarios %>% 
  filter(cve_ent==edo) %>%
  arrange(Fecha) %>% 
  # Add the dates before the 1st confirmed case
  add_row(Fecha = as_date("2020-01-01"), Casos.diarios = 0, Casos.media.7d = 0, Decesos.diarios = 0, Decesos.media.7d = 0,
          .before = 1) %>% 
  complete(Fecha = seq(min(.$Fecha), max(.$Fecha), by = 1),
           fill = list(Casos.diarios = 0, Casos.media.7d = 0, Decesos.diarios = 0, Decesos.media.7d = 0, cve_ent=edo)) %>% 
  mutate(day_of_year = yday(Fecha),
         year = year(Fecha)
  )

# Colors
outline_color <- "#01A2AC"
fill_color <- alpha("#01A2AC",0.3)
base_grey <- "grey28"

outline_color_d <- "#993366"
fill_color_d <- alpha("#993366",0.3)
base_grey <- "grey28"


text_color <- rgb(18, 18, 18, maxColorValue = 255)
base_family <- "Lato Black"
# base_family <- "Helvetica"
subtitle_date <- max(confirmados$Fecha) %>% 
  format("%b %d, %Y")

# Annotations for the years in a list (used in annotate())
year_annotations <- list(
  year = 2020:2022,
  x = rep(3, 3),
  y = as.POSIXct(paste(2020:2022, "01", "01", sep = "-"))
)

month_length <- c(31, 28, 31, 30, 31, 30,
                  31, 31, 30, 31, 30, 31)

month_breaks <- cumsum(month_length) - 30
casosmax <- max(confirmados$Casos.media.7d)
p <- confirmados %>% 
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
  annotate("text", label = paste0(year_annotations$year, "\u2192"), x = year_annotations$x, 
           y = year_annotations$y, 
           family = "Arial",
           size = 1.5, vjust = -0.6, hjust = 0.1) +   
  labs(
    subtitle = subtitle_date, caption = fuentenyt, 
  ) +
  theme_void(base_family = base_family) +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    panel.grid.major.x = element_line(color = "grey70", size = 0.2, linetype = "dotted"),
    panel.grid.minor.x = element_line(color = "grey70", size = 0.2, linetype = "dotted"),
    axis.text.x = element_text(color = base_grey, size = 5, hjust = 0.5, family="Lato Black"),
    text = element_text(color = text_color),
    plot.subtitle = element_text(hjust = 0.5, size = 5),
    plot.caption = element_text(family = "Lato", size =5), 
    plot.caption.position = 'plot'  ) +
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


leg_casos <- plyr::round_any(casosmax, 100, f = ceiling) 
legmax <-leg_casos/2


p_legend <- 
  tibble(
    cases = c(0, leg_casos),
    ymin = c(0, -legmax),
    ymax = c(0, legmax),
  ) %>% 
  ggplot(aes(cases)) +
  geom_ribbon(aes(ymin = 0, ymax = size_factor * ymax),
              color = outline_color, fill = fill_color, size = 0.3) +
  geom_ribbon(aes(ymin = size_factor * ymin, ymax = 0),
              color = outline_color_d, fill = fill_color_d, size = 0.3) +
  geom_line(aes(y = 1), color = base_grey, size=0.2) +
  geom_text(aes(label = ifelse(cases == 0, 0, paste0(legmax, " casos\n\n", legmax/10," decesos")), 
                y = 1, hjust = ifelse(cases == 0, 1.5, -0.1)),
            size = 1.8, family="Lato Light") +
  coord_cartesian(xlim = c(0,leg_casos*2), 
                  ylim = c(-as.numeric(as.POSIXct("1971-01-01")), NA), 
                  clip = "off") + 
  labs(title = paste0("<span style = 'font-size:8pt'>Covid-19 en</span><br>",nombre),
       subtitle="Promedio móvil a 7 días de<br><span style = 'color:#01A2AC';>casos confirmados por ingreso</span><br><span style = 'color:#993366';>decesos confirmados por ocurrencia</span><br>") +
  theme_void() +
  theme(plot.title = element_markdown(color = text_color, 
                                      family = "Lato Black",
                                      face = "bold", size = 18,
                                      lineheight = 1.1),
        plot.subtitle = element_markdown(color = text_color, 
                                         family = "Lato",
                                         size = 6,
                                         lineheight = 1.1))

plb <- p + inset_element(p_legend, left = 0.03, bottom = 0.65, right = 0.25, top = 1)

ggsave(paste0("C:/Users/luism/OneDrive/R/COVID/Sonora_Federal/Espirales/Estados/", nombre,".png"),plb, width = 5 , height = 5, type = "cairo", dpi = 400)

}

for (k in unique(Estatal_factor$cve_ent)) {
  plot_espiral(k)
}

