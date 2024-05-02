#-------------------------CONFIGURACION PREVIA--------------------
# 1. Limpiamos pantalla
rm(list=ls(all=TRUE))
graphics.off()
cat("\014")

# 2. Cargamos las librerías necesarias
library(ggplot2)
library(dplyr)
library(lubridate)


# Definir la función checkingOutliers
checkingOutliers <- function(df) {
  par(mfrow=c(1,2))  # Configurar el diseño de los gráficos
  for(i in 1:ncol(df)) {  # Iterar sobre todas las columnas del dataframe
    if (is.numeric(df[,i])){  # Verificar si la columna es numérica
      boxplot(df[,i], main = colnames(df)[i], width = 100, col="gray")  # Crear un gráfico de caja
    }
  }
}



#-------------------------0. LIMPIEZA DE DATOS (Solo la primera vez) --------------------

# Logica de limpieza de datos

limpiar_datos <- function(path) {
  
  datos <- read.csv(path, header = TRUE, stringsAsFactors = FALSE)
  # Nuevo dataset para modificar
  datos_limpios <- datos
  
  # Eliminar filas con valores faltantes 
  datos_limpios <- datos_limpios %>%
    na.omit()
  
  # Filtrar valores atípicos para 
    # required_car_parking_spaces y total_of_special_requests
  datos_limpios <- datos_limpios %>%
    filter(required_car_parking_spaces >= 0 & required_car_parking_spaces <= 1,
           total_of_special_requests >= 0 & total_of_special_requests <= 2)
  
    # stays_in_weekend_nights 
  percentile_99_weekend <- quantile(datos_limpios$stays_in_weekend_nights, probs = 0.99)
  datos_limpios <- datos_limpios %>%
    filter(stays_in_weekend_nights <= percentile_99_weekend)
    # y stays_in_week_nights
  percentile_99_week <- quantile(datos_limpios$stays_in_week_nights, probs = 0.99)
  datos_limpios <- datos_limpios %>%
    filter(stays_in_week_nights <= percentile_99_week)
  
  return(datos_limpios)
}


# Ruta relativa del csv de origen
path <- "../data/hotel_bookings.csv"

# Limpiamos los datos
datos_limpios <- limpiar_datos(path)

# Guardamos nuestros datos limpios
write.csv(datos_limpios, "../data/CLEAN_hotel_bookings.csv", row.names = FALSE)

#------------------------ 0. CARGAR DATOS LIMPIOS----------------------

# Cargamos datos limpios (DEBEN EXISTIR)
path<-"../data/CLEAN_hotel_bookings.csv"
datos_limpios <- read.csv(path, header = TRUE, stringsAsFactors = FALSE)


#-------------------------OBSERVACION DE DATOS--------------------
str(datos_limpios)

#-------------------------1. RESERVAS POR TIPO DE HOTEL --------------------


# (i)¿Cuántas reservas se realizan por tipo de hotel?
# Contar las reservas por tipo de hotel
reservas_por_hotel <- datos_limpios %>%
  group_by(hotel) %>%
  summarise(Reservas = n())

print("Reservas por tipo de hotel:")
print(reservas_por_hotel)

# Visualización en gráfico circular

  # Calcular los porcentajes
reservas_por_hotel <- reservas_por_hotel %>%
  mutate(Porcentaje = Reservas / sum(Reservas) * 100)

  # Crear el gráfico circular con porcentajes
ggplot(reservas_por_hotel, aes(x = "", y = Porcentaje, fill = hotel)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  theme_void() +
  labs(title = "Reservas por tipo de hotel",
       fill = "Tipo de hotel")

#-------------------------2. COMPORTAMIENTO DE LA DEMANDA --------------------
# (ii)¿Está aumentando la demanda con el tiempo?

# (ii).a Llegada de clientes por periodo
datos_limpios$arrival_date_month_num <- match(datos_limpios$arrival_date_month, month.name)

# Combinar los campos en una columna de fecha
datos_limpios$arrival_date <- as.Date(paste(datos_limpios$arrival_date_year, 
                                            datos_limpios$arrival_date_month_num, 
                                            datos_limpios$arrival_date_day_of_month, 
                                            sep = "-"), 
                                      format = "%Y-%m-%d")

# revisar primera y ultima fecha de llegada (CLientes)

fecha_minima<-min(datos_limpios$arrival_date)
print("El primer cliente llego el:")
print(fecha_minima)

fecha_maxima<-max(datos_limpios$arrival_date)
print("El ultimo cliente llego el:")
print(fecha_maxima)

# Observamos que los anos 2015 y 2017 estan incompletos (les faltan entre 2 y 3 m)
# Propuesta: La temporada iniciara el mes 7 del ano actual y dura un ano

datos_limpios <- datos_limpios %>%
  mutate(Periodo = case_when(
    arrival_date_month >= 7 ~ paste(arrival_date_year, "-", arrival_date_year + 1, sep = ""),
    TRUE ~ paste(arrival_date_year, "-", arrival_date_year, sep = "")
  ))

# Contar las reservas efectivas por temporada
reservas_efectivas_por_periodo <- datos_limpios %>%
  group_by(Periodo) %>%
  summarise(Reservas = n())

print(reservas_efectivas_por_periodo)

# Visualización de reservas por periodo
ggplot(reservas_efectivas_por_periodo, aes(x = Periodo, y = Reservas, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Clientes por Periodo Anual",
       x = "Periodo",
       y = "Arribo de clientes") +
  theme_minimal()

# (iii).b Analisis de arribo de clientes para el mismo mes, para cada ano


datos_limpios <- datos_limpios %>%
  mutate(Mes = arrival_date_month_num,
         Ano = arrival_date_year)


h_arribos_mes_ano <- datos_limpios %>%
  group_by(Ano, Mes) %>%
  summarise(Clientes = n(), .groups = "drop")



# Visualización de reservas por mes y año
ggplot(h_arribos_mes_ano, aes(x = Ano, y = Clientes, group = Mes)) +
  geom_line(aes(color = factor(Mes))) +
  geom_point(aes(color = factor(Mes))) +
  labs(title = "Arribos por Mes y Año",
       x = "Año",
       y = "Clientes",
       color = "Mes") +
  theme_minimal()



# (ii).c Reserva de clientes por periodo

datos_limpios <- datos_limpios %>%
  mutate(Mes = month(reservation_status_date),
         Ano = year(reservation_status_date))

# Agrupa y resume los datos por mes y año
h_reservas_mes_ano <- datos_limpios %>%
  group_by(Ano, Mes) %>%
  summarise(Reservas = n())

# Visualización de reservas por mes y año
ggplot(h_reservas_mes_ano, aes(x = Ano, y = Reservas, group = Mes)) +
  geom_line(aes(color = factor(Mes))) +
  geom_point(aes(color = factor(Mes))) +
  labs(title = "Reservas por Mes y Año",
       x = "Año",
       y = "Número de Reservas",
       color = "Mes") +
  theme_minimal()




#-------------------------3. ANALISIS DE TEMPORADAS --------------------
# TODO: Eliminar zonas vacias: Definir donde inician y terminan 

# (iii)¿Cuándo se producen las temporadas de reservas: alta, media y baja?
# Contar las reservas por mes
reservas_por_mes <- datos_limpios %>%
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

# Visualización con segmentos resaltados post analicis
ggplot(reservas_por_mes, aes(x = arrival_date_month, y = Reservas, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  # Aqui configuras los meses de inicio, color y transparencia
  geom_rect(aes(xmin = "January", xmax = "February", ymin = -Inf, ymax = Inf), fill = "red", alpha = 0.01) +
  geom_rect(aes(xmin = "February", xmax = "June", ymin = -Inf, ymax = Inf), fill = "yellow", alpha = 0.01) +
  geom_rect(aes(xmin = "June", xmax = "August", ymin = -Inf, ymax = Inf), fill = "green", alpha = 0.01) +
  geom_rect(aes(xmin = "August", xmax = "October", ymin = -Inf, ymax = Inf), fill = "yellow", alpha = 0.01) +
  geom_rect(aes(xmin = "October", xmax = "December", ymin = -Inf, ymax = Inf), fill = "red", alpha = 0.01) +
  
  labs(title = "Reservas por mes",
       x = "Mes",
       y = "Número de reservas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels = function(x) substr(x, 1, 3)) # Acortar los nombres de los meses para mejor visualización


#-------------------------4. MES MAS BAJO  --------------------
# TODO: Realizar histograma y ordenar ascendente (5 menor dem (muestra))

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
# (v)¿Cuántas reservas incluyen niños y/o bebés?  

reservas_totales <- datos_limpios %>%
  summarise(Reservas = n())

#print("Numero de reservas totales: ")
#print(reservas_totales)

# Contar las reservas que incluyen niños y/o bebés
reservas_con_niños <- datos_limpios %>%
  filter(children > 0 | babies > 0) %>%
  summarise(Reservas_con_niños = n())

numero_reservas_con_niños <- sum(!is.na(datos_limpios$children) & datos_limpios$children > 0 | 
                                   !is.na(datos_limpios$babies) & datos_limpios$babies > 0, na.rm = TRUE)

print("Número de reservas que incluyen niños y/o bebés:")
print(numero_reservas_con_niños)

# Combianamos los datos
resume <- data.frame(Tipo = c("Total", "Con ninos y bebes"),
                     Cantidad = c(reservas_totales$Reservas, reservas_con_niños$Reservas_con_niños))
# Visualizacion
ggplot(resume, aes(x = Tipo, y = Cantidad, fill = Tipo)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Cantidad), position = position_dodge(width = 0.9), vjust = +2) +
  scale_y_log10() + # Escala logaritmica 
  scale_fill_manual(values = c("#C767CB", "salmon"))
  labs(title = "Reservas totales vs. Reservas con niños y/o bebés",
       x = "Tipo de reserva",
       y = "Cantidad de reservas")

# informacion porcentual 
percent = round(reservas_con_niños$Reservas_con_niños / reservas_totales$Reservas * 100, 2)
  
print("Del total, las reservas con ninos y/o bebes representan un: ")
print(percent)

#-------------------------6. ESTACIONAMIENTO  --------------------

# todo: otro grafico (cant personas) averiguar porcentaje
# Cada cuantas personas debemos tener un espacio de estacionamiento???

# (vi)¿Es importante contar con espacios de estacionamiento?
# Resumen estadístico
resumen_estadistico <- datos_limpios %>%
  summarise(Media = mean(required_car_parking_spaces, na.rm = TRUE),
            Mediana = median(required_car_parking_spaces, na.rm = TRUE),
            Desviacion_Estandar = sd(required_car_parking_spaces, na.rm = TRUE))

print("Resumen estadístico de los espacios de estacionamiento:")
print(resumen_estadistico)

# Visualización de la distribución de los espacios de estacionamiento
ggplot(datos_limpios, aes(x = required_car_parking_spaces)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribución de espacios de estacionamiento",
       x = "Número de espacios de estacionamiento",
       y = "Frecuencia")

#-------------------------5. CANCELACIONES POR MES  --------------------
# TODO: cant no tan relevante -> %de cancelacion (o comparacion hist cancelacion/reserva)

# (vii)¿En qué meses del año se producen más cancelaciones de reservas?
# Agrupar los datos por mes y contar las cancelaciones de reservas
cancelaciones_por_mes <- datos_limpios %>%
  filter(is_canceled == 1) %>%
  group_by(arrival_date_month) %>%
  summarise(Cancelaciones = n())

print("Cancelaciones de reservas por mes:")
print(cancelaciones_por_mes)

# Ordenar los meses en orden cronológico
meses_ordenados <- c("January", "February", "March", "April", "May", "June", 
                     "July", "August", "September", "October", "November", "December")
cancelaciones_por_mes$arrival_date_month <- factor(cancelaciones_por_mes$arrival_date_month, levels = meses_ordenados)

# ejecutar analisis de temporadas

# Visualizar los datos
ggplot(cancelaciones_por_mes, aes(x = arrival_date_month, y = Cancelaciones)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Cancelaciones de reservas por mes",
       x = "Mes",
       y = "Número de cancelaciones") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Visualización percent cancelaciones por mes
