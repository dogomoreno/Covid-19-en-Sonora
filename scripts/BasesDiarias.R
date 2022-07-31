rm(list=ls())

# Paquetes
library(tidyverse)
library(lubridate)

setwd("C:/Users/luism/OneDrive/R/COVID/Sonora_Federal")

# Casos Diarios
Casosdiarios <- read_csv("Bases/ST_CasosMunicipalesSonora_SSFED.csv",
                         locale = locale(encoding = "UTF-8"))
Casosdiarios[is.na(Casosdiarios)] <- 0
Casosdiarios<- Casosdiarios %>%
  rename(CVEGEO=CVE_MUN, MUNICIPIO = NOM_MUN) %>% 
  gather( key= "Fecha", value= "CASOS", starts_with(c("2020","2021","2022"))) %>%
  mutate(Fecha = as.Date(Fecha,format = "%Y-%m-%d")) %>% 
  group_by(MUNICIPIO, CVEGEO) %>% 
  mutate(NUEVOS= (CASOS - lag(CASOS, default = 0, order_by=Fecha)))%>% 
  select (Fecha, CVEGEO, MUNICIPIO, CASOS, NUEVOS) %>% 
  write.csv('Bases/Casosdiarios_SSFED.csv')

# Decesos Diarios
Decesosdiarios <- read_csv("Bases/ST_DecesosMunicipalesSonora_SSFED.csv",
                         locale = locale(encoding = "UTF-8"))
Decesosdiarios[is.na(Decesosdiarios)] <- 0
Decesosdiarios<- Decesosdiarios %>%
  rename(CVEGEO=CVE_MUN, MUNICIPIO = NOM_MUN) %>% 
  gather( key= "Fecha", value= "DECESOS", starts_with(c("2020","2021","2022","2022"))) %>%
  mutate(Fecha = as.Date(Fecha,format = "%Y-%m-%d")) %>% 
  group_by(MUNICIPIO, CVEGEO) %>% 
  mutate(NUEVOS= DECESOS - lag(DECESOS, default = 0, order_by=Fecha)) %>% 
  select (Fecha, CVEGEO, MUNICIPIO, DECESOS, NUEVOS) %>% 
  write.csv('Bases/Decesosdiarios_SSFED.csv')



Casosdiarios <- read_csv("Bases/ST_CasosMunicipalesSonora_FSINTOMAS_SSFED.csv",
                         locale = locale(encoding = "UTF-8"))
Casosdiarios[is.na(Casosdiarios)] <- 0
Casosdiarios<- Casosdiarios %>%
  rename(CVEGEO=CVE_MUN, MUNICIPIO = NOM_MUN) %>% 
  gather( key= "Fecha", value= "CASOS_SINTOMAS", starts_with(c("2020","2021","2022"))) %>%
  mutate(Fecha = as.Date(Fecha,format = "%Y-%m-%d")) %>% 
  select (Fecha, CVEGEO, MUNICIPIO, CASOS_SINTOMAS) %>% 
  write.csv('Bases/CasosdiariosSINT_SSFED.csv')


Activosdiarios <- read_csv("Bases/ST_ActivosMunicipalesSonora_SSFED.csv",
                         locale = locale(encoding = "UTF-8"))
Activosdiarios[is.na(Activosdiarios)] <- 0
Activosdiarios<- Activosdiarios %>%
  rename(CVEGEO=CVE_MUN, MUNICIPIO = NOM_MUN) %>% 
  gather( key= "Fecha", value= "CASOS_ACTIVOS", starts_with(c("2020","2021","2022"))) %>%
  mutate(Fecha = as.Date(Fecha,format = "%Y-%m-%d")) %>% 
  select (Fecha, CVEGEO, MUNICIPIO, CASOS_ACTIVOS) %>% 
  write.csv('Bases/Activosdiarios_SSFED.csv')


# Decesos Diarios
Decesosdiarios <- read_csv("Bases/ST_DecesosMunicipalesSonora_FDEFUNCION_SSFED.csv",
                           locale = locale(encoding = "UTF-8"))
Decesosdiarios[is.na(Decesosdiarios)] <- 0
Decesosdiarios<- Decesosdiarios %>%
  rename(CVEGEO=CVE_MUN, MUNICIPIO = NOM_MUN) %>% 
  gather( key= "Fecha", value= "DECESOS_OCURRIDOS", starts_with(c("2020","2021","2022"))) %>%
  mutate(Fecha = as.Date(Fecha,format = "%Y-%m-%d")) %>% 
  select (Fecha, CVEGEO, MUNICIPIO, DECESOS_OCURRIDOS) %>% 
  write.csv('Bases/DecesosdiariosDEF_SSFED.csv')


