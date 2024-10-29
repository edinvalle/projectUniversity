# Cargar las librerías necesarias
library(readxl) 
library(dplyr)   
library(knitr) 
library(tidyr)  
library(ggplot2)
library(lmtest)
library(tseries)


# Leer el archivo Excel 
df <- read_excel("~/Rstudio/Econometria I/Dataset.xlsx", sheet = "Hoja1")

# Cambiar los nombres de las columnas 
colnames(df) <- c("Años", "PGS", "PGR", "PD", 
                  "PPC", "CGS", "CGR", "CD", "REZAGOGS")

# Verificar los nombres de las columnas
print(colnames(df))

# Asegurarse de que la columna "Años" esté en formato Date
df <- df %>%
  mutate(Años = as.Date(Años))  


# Verificar los datos después de la limpieza
print(head(df))

# Resumen estadístico de los datos
summary(df %>% select(-Años))


# Graficar los precios de combustibles 
suppressWarnings({
  ggplot(df, aes(x = Años)) +
    geom_line(aes(y = PGS, color = "Gasolina Superior"), linewidth = 1) +
    geom_line(aes(y = PGR, color = "Gasolina Regular"), linewidth = 1) +
    geom_line(aes(y = PD, color = "Diésel"), linewidth = 1) +
    labs(title = "Evolución de los precios de combustibles",
         x = "Fecha", y = "Precio (Q)") +
    scale_color_manual(values = c("Gasolina Superior" = "blue", 
                                  "Gasolina Regular" = "green", 
                                  "Diésel" = "red")) +
    theme_minimal()
})




# MODELO 1 Dependiente de precios de gasolinas(PG), Independiente Consumo de gasolinas super (CGS), Precio de petroleo (PPC)
# Ajustar modelo de rmúltiple
modelo1 <- lm(PGS ~ PPC + CGS, data = df)
# Resumen del modelo
summary(modelo1)
# Obtener los residuos del modelo
residuos1 <- residuals(modelo1)

# 1. NORMALIDAD DE LOS RESIDUOS

# Histograma
hist(residuos1, breaks = 15, main = "Histograma de Residuos de GS", xlab = "Residuos")
# Prueba de Jarque-Bera
jarque.bera.test(residuos1)

# 2. AUTOCORRELACIÓN DE RESIDUOS

# Prueba de Durbin-Watson
dwtest(modelo1)
# Correlograma (ACF) de los residuos
acf(residuos1, main = "Correlograma de Residuos de GS")

# Ajuste de autocorrelación rezago
rezago1 <- lm(REZAGOGS ~ PPC + CGS, data = df)
summary(rezago1)
# Obtener los residuos del modelo
residuos_rezago1 <- residuals(rezago1)
dwtest(rezago1)
# Correlograma (ACF) de los residuos
acf(residuos_rezago1, main = "Correlograma de Residuos con Rezago de GS")

# 3. HETEROCEDASTICIDAD

# Prueba de Breusch-Pagan
bptest(modelo1)
# Prueba de White
bptest(modelo1, ~ PPC + CGS + I(PPC^2) + I(CGS^2), data = df)



# MODELO 2 Dependiente de precios de gasolinas(PG), Independiente Consumo de gasolinas Regular (CGR), Precio de petroleo (PPC)
# Ajustar modelo de rmúltiple
modelo2 <- lm(PGR ~ PPC + CGR, data = df)
summary(modelo2)
# Obtener los residuos del modelo
residuos2 <- residuals(modelo2)

# 1. NORMALIDAD DE LOS RESIDUOS
# Histograma 
hist(residuos2, breaks = 15, main = "Histograma de Residuos de GR", xlab = "Residuos")

# Prueba de Jarque-Bera 
jarque.bera.test(residuos2)

# 2. AUTOCORRELACIÓN DE RESIDUOS

# Prueba de Durbin-Watson
dwtest(modelo2)
# Correlograma (ACF) de los residuos
acf(residuos2, main = "Correlograma de Residuos de GR")

# Ajuste de autocorrelación rezago
rezago2 <- lm(REZAGOGS ~ PPC + CGR, data = df)
summary(rezago2)
# Obtener los residuos del modelo
residuos_rezago2 <- residuals(rezago2)
dwtest(rezago2)
# Correlograma (ACF) de los residuos
acf(residuos_rezago2, main = "Correlograma de Residuos con Rezago de GR")

# 3. HETEROCEDASTICIDAD

# Prueba de Breusch-Pagan
bptest(modelo2)
# Prueba de White
bptest(modelo2, ~ PPC + CGR + I(PPC^2) + I(CGR^2), data = df)



# MODELO 3 Dependiente de precios de gasolinas(PG), Independiente Consumo de diésel (CD), Precio de petroleo (PPC)

# Ajustar modelo de rmúltiple
modelo3 <- lm(PD ~ PPC + CD, data = df)
summary(modelo3)
# Obtener los residuos del modelo
residuos3 <- residuals(modelo3)

# 1. NORMALIDAD DE LOS RESIDUOS

# Histograma 
hist(residuos3, breaks = 15, main = "Histograma de Residuos de diésel", xlab = "Residuos")
# Prueba de Jarque-Bera 
jarque.bera.test(residuos3)

# 2. AUTOCORRELACIÓN DE RESIDUOS

# Prueba de Durbin-Watson
dwtest(modelo3)
# Correlograma (ACF) de los residuos
acf(residuos3, main = "Correlograma de Residuos de diésel")

# Ajuste de autocorrelación
rezago3 <- lm(REZAGOGS ~ PPC + CD, data = df)
summary(rezago3)
# Obtener los residuos del modelo
residuos_rezago3 <- residuals(rezago3)
# Prueba de Durbin-Watson
dwtest(rezago3)
# Correlograma (ACF) de los residuos
acf(residuos_rezago3, main = "Correlograma de Residuos con Rezago de diésel")


# 3. HETEROCEDASTICIDAD

# Prueba de Breusch-Pagan
bptest(modelo3)
# Prueba de White
bptest(modelo3, ~ PPC + CD + I(PPC^2) + I(CD^2), data = df)


##INDICE DE HERFINDAHL-HIRSCHMAN
# Crear el DataFrame 
datos <- data.frame(
  Año = c(2008, 2009, 2010, 2011, 2012),
  Gasolina_Super = c(19.8, 20.5, 21.9, 19.7, 20.5),
  Gasolina_Regular = c(11.2, 13.1, 13.5, 13.5, 12.5),
  Diesel = c(38.1, 37.9, 37.7, 39.1, 41.8)
)

# Calcular el índice de Herfindahl-Hirschman
datos$HHI <- (datos$Gasolina_Super / 100)^2 + 
  (datos$Gasolina_Regular / 100)^2 + 
  (datos$Diesel / 100)^2

# Multiplicar por 10,000 para obtener el HHI en escala tradicional
datos$HHI <- datos$HHI * 10000

# Mostrar el DataFrame con el índice de Herfindahl-Hirschman
library(knitr)
kable(datos, caption = "Índice de Herfindahl-Hirschman para los años 2008-2012")

# graficar el índice de Herfindahl-Hirschman 
suppressWarnings({
  ggplot(datos, aes(x = Año, y = HHI)) +
    geom_line(color = "blue", size = 1) +
    labs(title = "Índice de Herfindahl-Hirschman para los años 2008-2012",
         x = "Año", y = "HHI") +
    theme_minimal()
})
