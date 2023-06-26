#PROBLEM SET 1
#BIG DATA Y MACHINE LEARNING
#LAURA ALMARIO, DANIELA ROJAS, ANAMARIA RODRIGUEZ Y DANIEL ORJUELA



setwd("C:/Users/IPA/Desktop/Big Data/Taller 1")
#PUNTO 1
#limpiamos el ambiente
rm(list = ls())
## 1. Llamado de los paquetes de datos y librerias ##
library(pacman)
p_load("tidyverse", "rvest", "writexl", "stargazer", "ggplot2", "reshape2", "dplyr", "datasets", "skimr", "gridExtra")
library(data.table)

## 2. Sraping de los datos (Chunks 1 a 10)
base_url = "https://ignaciomsarmiento.github.io/GEIH2018_sample/"
base_webpage <- read_html(base_url)
lists_pages = html_nodes(base_webpage, "ul li")

df = data.frame()
for (row in lists_pages){
  a = html_node(row, "a")
  val <- html_attr(a, "href")
  if (val != "index.html") {
    df_temp = data.frame()
    data_base_url <- paste0(base_url,val)
    webpage <- read_html(data_base_url)
    hidden_page = html_nodes(webpage, "div[w3-include-html]")
    hidden_page_value <- html_attr(hidden_page, "w3-include-html")
    data_page = paste0(base_url,hidden_page_value)
    data_html <- read_html(data_page)
    rows <- html_nodes(data_html, "table tr")
    text <- html_text(rows)
    cleaned_text <- gsub("\\s+", " ", text)
    df_temp = read.table(text=cleaned_text, header=TRUE)
    df = rbind(df_temp, df)
  }
}

#Data Cleaning
#filtramos la base de datos para tener los ocupados mayores de 18 años y seleccionamos las variables de interés
df <- df[df$age > 18 & df$ocu == 1, c("age", "cuentaPropia", "formal", "hoursWorkUsual", "maxEducLevel", "ocu", "oficio", "estrato1", "informal","p6050", "relab", "sex", "sizeFirm", "wap", "y_total_m")]
df <- subset(df, !is.na(y_total_m) & y_total_m != 0)
df$maxEducLevel[is.na(df$maxEducLevel)] <- 0

df <- df %>%
  mutate(JHOGAR = recode(p6050, 
                         `1` = "1", 
                         `2:9` = "0"))
df$JHOGAR <- ifelse(is.na(df$JHOGAR), "0", df$JHOGAR)

df <- df %>%
  mutate(mujer = recode(sex, 
                        `0` = "1", 
                        `1` = "0"))
df$mujer <- ifelse(is.na(df$mujer), "0", df$mujer)

#Nos quedamos con 16397 observaciones despues de restringir la edad

#Visualizamos la estructura de la base de datos
glimpse(df)
#view(df)

#Cargar la librería openxlsx para exportar a excel las estadísticas
library(openxlsx)  

# Calcular las estadísticas descriptivas
estadisticas <- skim(df)
class(estadisticas)
estadisticas_tbl <- as.data.frame(estadisticas)

#para exportar una tabla en excel
p_load(xlsx)
write.xlsx(estadisticas_tbl, file = "estadisticas_tbl.xlsx")

##Revisamos los datos faltantes para cada columna
max(colSums(is.na(df)))
colSums(is.na(df))

# Graficamos 

#histograma ingresos
ggplot(df, aes(x = df$y_total_m)) + geom_histogram() + 
  labs(x = "Ingreso total", y = "frequency", title = "Ingreso total Acumulado")

#histograma de edad
ggplot(df, aes(x = df$age)) + geom_histogram() + 
  labs(x = "Edad", y = "frequency", title = "Edad")

# Ingresos y género
ggplot(df, aes(x = df$sex, y = df$y_total_m)) +
  geom_col(colour = "blue4") +
  labs(x = "Sexo", y = "ingreso total", title = "Ingreso total por sexo")

# Ingresos y Nivel educativo
ggplot(df, aes(x = df$maxEducLevel, y = df$y_total_m)) +
  geom_col(colour = "blue") +
  labs(x = "Nivel educativo", y = "ingreso total", title = "Ingreso total por nivel educativo")

# Ingresos y formalidad
ggplot(df, aes(x = df$formal, y = df$y_total_m)) +
  geom_col(colour = "blue") +
  labs(x = "Formalidad", y = "ingreso total", title = "Ingreso total por formalidad laboral")

# Ingresos y jefatura de hogar
ggplot(df, aes(x = df$JHOGAR, y = df$y_total_m)) +
  geom_col(colour = "blue") +
  labs(x = "Jefatura del hogar", y = "ingreso total", title = "Ingreso total por jefatura del hogar")

# Ingresos y estrato
ggplot(df, aes(x = df$estrato1, y = df$y_total_m)) +
  geom_col(colour = "blue") +
  labs(x = "Estrato", y = "ingreso total", title = "Ingreso total por estrato")
