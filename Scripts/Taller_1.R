## Taller 1 - GEIH ##
## Equipo: Laura Almario, Daniel Orjuela, Daniela Rojas, Anamaria Rodriguez ##

## 1. Llamado de los paquetes de datos y librerias ##
library(pacman)
p_load("tidyverse", "rvest", "writexl", "stargazer", "ggplot2", "reshape2", "dplyr", "datasets", "skimr", "gridExtra", "datapasta")
library(data.table)

## 2. Sraping de los datos (Chunks 1 a 10)
url <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_"
data <- data.frame()
for (i in 1:10) {
  url_i <- paste0(url, i, ".html")
  tablas <- url_i %>%
    read_html() %>%
    html_table() %>% .[[1]]
  data <- rbind.data.frame(data, tablas)
}
ave.image("C:/Users/lalmari/OneDrive - FTI Consulting/Documentos/MeCa/Taller_1/Taller1_GEIH_2023/Stores/RawData.RData")
