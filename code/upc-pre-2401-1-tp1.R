#-------------------------CONFIGURACION PREVIA--------------------
# Limpiamos pantalla
rm(list=ls(all=TRUE))
graphics.off()
cat("\014")

# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)

# Definir la función checkingOutliers
checkingOutliers <- function(df) {
  par(mfrow=c(1,2))  # Configurar el diseño de los gráficos
  for(i in 1:ncol(df)) {  # Iterar sobre todas las columnas del dataframe
    if (is.numeric(df[,i])){  # Verificar si la columna es numérica
      boxplot(df[,i], main = colnames(df)[i], width = 100, col="gray")  # Crear un gráfico de caja
    }
  }
}

#-------------------------LECTURA DE DATOS --------------------

# Cargar los datos
# TODO: extraer funcion y crear un archivo con la data limpia
setwd("C:/Users/rodyv/source/repos/V ciclo/R/CC216-TP-2024-1")
datos <- read.csv("data/hotel_bookings.csv", header = TRUE, stringsAsFactors = FALSE)
View(datos)

# Identificación de datos faltantes (NA)
print("Cantidad de valores faltantes por columna:")
print(colSums(is.na(datos)))

# Explicación y aplicación de la técnica utilizada para eliminar o completar los datos faltantes
# Eliminar las filas que contienen valores faltantes
datos <- datos %>%
  na.omit()

# Llamar a la función checkingOutliers para identificar outliers en las columnas numéricas
checkingOutliers(datos)
sapply(datos, class)


#-------------------------1. RESERVAS POR TIPO DE HOTEL --------------------

# (i)¿Cuántas reservas se realizan por tipo de hotel?
# Contar las reservas por tipo de hotel
reservas_por_hotel <- datos %>%
  group_by(hotel) %>%
  summarise(Reservas = n())

print("Reservas por tipo de hotel:")
print(reservas_por_hotel)

# Visualización
ggplot(reservas_por_hotel, aes(x = hotel, y = Reservas, fill = hotel)) +
  geom_bar(stat = "identity") +
  labs(title = "Reservas por tipo de hotel",
       x = "Tipo de hotel",
       y = "Número de reservas") +
  theme_minimal()


#-------------------------2. COMPORTAMIENTO DE LA DEMANDA --------------------

# (ii)¿Está aumentando la demanda con el tiempo?
# Convertir arrival_date_year a factor para que se muestre en el gráfico correctamente
datos$arrival_date_year <- as.factor(datos$arrival_date_year)

# Contar las reservas por año
reservas_por_año <- datos %>%
  group_by(arrival_date_year) %>%
  summarise(Reservas = n())

print("Reservas por año:")
print(reservas_por_año)

# Visualización
ggplot(reservas_por_año, aes(x = arrival_date_year, y = Reservas, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Reservas por año",
       x = "Año",
       y = "Número de reservas") +
  theme_minimal()

#-------------------------3. ANALISIS DE TEMPORADAS --------------------

# (iii)¿Cuándo se producen las temporadas de reservas: alta, media y baja?
# Contar las reservas por mes
reservas_por_mes <- datos %>%
  group_by(arrival_date_month) %>%
  summarise(Reservas = n())

print("Reservas por mes:")
print(reservas_por_mes)

# Ordenar los meses en orden cronológico
meses_ordenados <- c("January", "February", "March", "April", "May", "June", 
                     "July", "August", "September", "October", "November", "December")
reservas_por_mes$arrival_date_month <- factor(reservas_por_mes$arrival_date_month, levels = meses_ordenados)

# Visualización
ggplot(reservas_por_mes, aes(x = arrival_date_month, y = Reservas, group = 1)) +
  geom_line(color = "green") +
  geom_point(color = "green") +
  labs(title = "Reservas por mes",
       x = "Mes",
       y = "Número de reservas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels = function(x) substr(x, 1, 3)) # Acortar los nombres de los meses para mejor visualización

#-------------------------4. MES MAS BAJO  --------------------

# (iv)¿Cuándo es menor la demanda de reservas?
# Encontrar el mes con el menor número de reservas
mes_menor_demanda <- reservas_por_mes$arrival_date_month[which.min(reservas_por_mes$Reservas)]

print("Mes con menor demanda de reservas:")
print(mes_menor_demanda)

# Visualización
ggplot(reservas_por_mes, aes(x = arrival_date_month, y = Reservas, group = 1)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  geom_point(data = filter(reservas_por_mes, arrival_date_month == mes_menor_demanda), color = "blue", size = 3) +
  labs(title = "Reservas por mes",
       x = "Mes",
       y = "Número de reservas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels = function(x) substr(x, 1, 3)) # Acortar los nombres de los meses para mejor visualización


#-------------------------5. RESERVAS CON NINOS Y BEBES  --------------------

# (v)¿Cuántas reservas incluyen niños y/o bebés?  (MEJORABLE, que porcentaje representan)
# Contar las reservas que incluyen niños y/o bebés
reservas_con_niños <- datos %>%
  filter(children > 0 | babies > 0) %>%
  summarise(Reservas_con_niños = n())

numero_reservas_con_niños <- sum(!is.na(datos$children) & datos$children > 0 | 
                                   !is.na(datos$babies) & datos$babies > 0, na.rm = TRUE)

print("Número de reservas que incluyen niños y/o bebés:")
print(numero_reservas_con_niños)

# Visualización
ggplot(reservas_con_niños, aes(x = "", y = Reservas_con_niños)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Reservas que incluyen niños y/o bebés",
       x = "",
       y = "Cantidad de reservas")

#-------------------------6. ESTACIONAMIENTO  --------------------


# (vi)¿Es importante contar con espacios de estacionamiento?
# Resumen estadístico
resumen_estadistico <- datos %>%
  summarise(Media = mean(required_car_parking_spaces, na.rm = TRUE),
            Mediana = median(required_car_parking_spaces, na.rm = TRUE),
            Desviacion_Estandar = sd(required_car_parking_spaces, na.rm = TRUE))

print("Resumen estadístico de los espacios de estacionamiento:")
print(resumen_estadistico)

# Visualización de la distribución de los espacios de estacionamiento
ggplot(datos, aes(x = required_car_parking_spaces)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribución de espacios de estacionamiento",
       x = "Número de espacios de estacionamiento",
       y = "Frecuencia")

#-------------------------5. CANCELACIONES POR MES  --------------------
# TODO: cant no tan relevante -> %de cancelacion (o comparacion hist cancelacion/reserva)

# (vii)¿En qué meses del año se producen más cancelaciones de reservas?
# Agrupar los datos por mes y contar las cancelaciones de reservas
cancelaciones_por_mes <- datos %>%
  filter(is_canceled == 1) %>%
  group_by(arrival_date_month) %>%
  summarise(Cancelaciones = n())

print("Cancelaciones de reservas por mes:")
print(cancelaciones_por_mes)

# Ordenar los meses en orden cronológico
meses_ordenados <- c("January", "February", "March", "April", "May", "June", 
                     "July", "August", "September", "October", "November", "December")
cancelaciones_por_mes$arrival_date_month <- factor(cancelaciones_por_mes$arrival_date_month, levels = meses_ordenados)

# Visualizar los datos
ggplot(cancelaciones_por_mes, aes(x = arrival_date_month, y = Cancelaciones)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Cancelaciones de reservas por mes",
       x = "Mes",
       y = "Número de cancelaciones") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
