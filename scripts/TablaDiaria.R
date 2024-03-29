rm(list = ls())

if(!require('pacman')) install.packages('pacman')
pacman::p_load(ggtext, tidyverse, extrafont, scales, 'Cairo', gt, gtExtras, zoo, lubridate, magick)

logo <- image_read("Shapes/SEDFesp.png")

Casos <- read_csv("C:/Users/luism/OneDrive/R/COVID/Sonora_Federal/Bases/Casosdiarios_SSFED.csv", 
                  col_types = cols(CASOS = col_integer(), 
                                   CVEGEO = col_character(), Fecha = col_date(format = "%Y-%m-%d"), 
                                   MUNICIPIO = col_character(), NUEVOS = col_integer(), X1 = col_skip()), 
                  locale = locale(encoding = "ISO-8859-1")) %>% filter(Fecha>as.Date("2020-10-08"))
casosacumdia <- filter(Casos,Fecha==max(as.Date(Fecha)))
casosacumdiaorder <- arrange(casosacumdia,CASOS, desc(MUNICIPIO))
casosacumdia2 <- mutate(casosacumdiaorder,id=CVEGEO)

Decesos <- read_csv("C:/Users/luism/OneDrive/R/COVID/Sonora_Federal/Bases/Decesosdiarios_SSFED.csv", 
                    col_types = cols(DECESOS = col_integer(), 
                                     CVEGEO = col_character(), Fecha = col_date(format = "%Y-%m-%d"), 
                                     MUNICIPIO = col_character(), NUEVOS = col_integer(), X1 = col_skip()), 
                    locale = locale(encoding = "ISO-8859-1")) %>% filter(Fecha>as.Date("2020-10-08"))
decesosacumdia <- filter(Decesos,Fecha==max(as.Date(Fecha)))
decesosacumdiaorder <- arrange(decesosacumdia,DECESOS, desc(MUNICIPIO))
decesosacumdia2 <- mutate(decesosacumdiaorder,id=CVEGEO)


decesosacumdia3 <- rename(decesosacumdia,'DECESOS NUEVOS'=NUEVOS)
casosacumdia3 <- rename(casosacumdia,'CASOS NUEVOS'=NUEVOS)
Diario <-left_join(casosacumdia3, decesosacumdia3,by = c("CVEGEO","Fecha","MUNICIPIO"))


CDSELECT<- Diario %>% select("MUNICIPIO", "CASOS NUEVOS", "DECESOS NUEVOS") %>% 
  rename( "CASOS"= "CASOS NUEVOS", "DECESOS" = "DECESOS NUEVOS")
CDSELECT1 <- CDSELECT %>% mutate(DECESOS = coalesce(DECESOS,0L), CASOS = coalesce(CASOS,0L)) 
dia<-max(as.Date(Casos$Fecha))


CDSELECT2 <- CDSELECT1 %>% filter(CASOS!=0 | DECESOS!=0)



gt1 = CDSELECT2 %>% 
  arrange(desc(CASOS)) %>%
  gt(rowname_col = "MUNICIPIO") %>%
  gt_theme_espn() %>%
    # Spanner
  tab_spanner(label="VARIACIÓN DIARIA", columns=CASOS:DECESOS) %>%
  # Column labels
  cols_label(
    CASOS = html("<span style = 'color:#01A2AC; weight:bold; '>Casos</span>"),
    DECESOS = html("<span style = 'color:#993366;weight:bold;'>Decesos</span>"),
    MUNICIPIO=html("Municipio")
  ) %>%
    # Format first column text 
  tab_style(
    style=list(cell_text(weight="normal")),
    location=cells_body(columns=MUNICIPIO)
  ) %>%
  grand_summary_rows(
    columns = c(CASOS, DECESOS),
    fns = list(
      "TOTAL ESTATAL" = ~sum(., na.rm = TRUE)),
      formatter= fmt_number, decimals = 0
  ) %>% 
  # Header and source note
  tab_header(title=md("<span style = 'font-size:12pt'> Covid-19 en Sonora</span><br> Reporte diario <img src='https://www.luisarmandomoreno.com/wp-content/uploads/2022/04/cropped-header.png' style='height:35px;'>"),
             subtitle=md(paste0("Variación diaria en el acumulado, corte al ", day(dia),"/", month(dia), "/",year(dia)))) %>%
  tab_stubhead(label= "MUNICIPIO")%>% 
  tab_source_note(source_note = gt::html("*Variaciones negativas derivadas de ajustes y correcciones de captura.<br>Elaboración Luis Armando Moreno (@dogomoreno) con información de la Secretaría de Salud Federal. <br> www.luisarmandomoreno.com")) %>%
  # Adjust sub-title font
  tab_style(
    style = list(
      cell_text(
        weight="normal", size="18px", font= "Lato Light"
      )
    ),
    locations = list(
      cells_title(groups = "subtitle")
    )
  )  %>%
  tab_style(
    style = list(
      cell_text(
        weight="bolder", font= "Lato Black", size="36px"
      )
    ),
    locations = list(
      cells_title(groups = "title")
    )
  )  %>%
  tab_style(
    style = list(
      cell_text(
        weight="bolder", font= "Lato Black", size="40px"
      )
    ),
    locations = list(
      cells_title(groups = "grand.summary")
    )
  )  %>%
  # Adjust source note font size
  tab_options(source_notes.font.size = "11px")

gt1
gt1 %>% gtsave(
  "Gráficos diarios/tablacasoshoy.png", expand = 20
)
