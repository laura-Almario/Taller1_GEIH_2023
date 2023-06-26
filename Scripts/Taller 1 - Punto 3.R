##PUNTO 3##
df$log_y_total_m <- log(df$y_total_m)


# Ajustar el modelo de regresión
modelo <- lm(log_y_total_m ~ age + I(age^2), data = df)

#sacamos la tabla de regresion
summary(modelo)
library(broom)
resultado1 <- bind_rows(tidy(modelo))
write.xlsx(resultado1, file="C:/Users/IPA/Desktop/Big Data/Taller 1/Resultadomodelo(Excel).xlsx")


# Obtenemos el valor del R-cuadrado
r_squared <- summary(modelo)$r.squared
# Crear un data frame con el valor del R-cuadrado
resultado2 <- data.frame(R_squared = r_squared)
# Guardar el data frame en un archivo de Excel
library(openxlsx)
write.xlsx(resultado2, file = "C:/Users/IPA/Desktop/Big Data/Taller 1/R_squared.xlsx")

#realizamos el plot de los ingresos promedio por edad
p_load(ggplot2)

ggplot(df, aes(x = df$age, y = df$y_total_m)) +
  geom_point() +
  labs(x = "Edad", y = "ingreso total", title = "Ingreso total por edad")

#cargamos el paquete para realizar el bootstrap
p_load(boot)

#calculamos los intervalos de confianza
boot.fn <- function(data , index)
  + coef(lm(log_y_total_m ~ age + I(age^2), data = df, subset = index))
boot.fn(df , 1:16397)

boot(data = df , statistic = boot.fn , R = 1000)
boot.fn <- function(df, index)
  coef (
    lm(log_y_total_m ~ age + I(age^2), data = df, subset = index)
    
  )
#ponemos una semilla para que sea replicable el ejemplo
set.seed(2023)
boot(df, boot.fn, 1000)

#presentamos los resultados de la regresion
summary(lm(log_y_total_m ~ age + I(age^2), data = df))$coef


library(stargazer)
regresion_modelo <- stargazer(modelo, type="text")
regresion_modelo
capture.output(regresion_modelo, file="regresion_modelo.doc")


resultado1 <- bind_rows(tidy(modelo))
write.xlsx(resultado1, file="C:/Users/IPA/Desktop/Big Data/Taller 1/Resultadomodelo(Excel).xlsx")
