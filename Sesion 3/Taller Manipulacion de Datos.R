# ******************************************************** #
#                  Manipulacion de Datos                   #
# ******************************************************** #

## Todos los comentarios marcados como #* son ejercicios para el lector.

### Declaracion del ambiente de trabajo ----

# setwd("ruta de los archivos")

### Cargue de datos ----

## Paquete base

#Aeropuertos
aeropuertos <- read.csv("Sesion 2/data/aeropuertos.csv", sep="\t", header=T, dec=",", quote = "\"", 
                        colClasses = c("character", "character", "numeric","numeric", "integer", "integer", "character", "character"))
# Si es necesario los nombres de las columnas se especifican de manera analoga a la definicion de colClases con col.names = c()
str(aeropuertos) # Consulta la estructuta de un conjunto de datos.
names(aeropuertos) # Lista los nombres de las columnas de un conjunto de datos.

#Vuelos
#** Cargar base de datos de vuelos

# Paquete data.table
library(data.table)

#Aeropuertos
aeropuertos <- fread("Sesion 2/data/aeropuertos.csv", verbose = T)
#Vuelos
vuelos <- fread("Sesion 2/data/vuelos.csv", verbose = T)
#* Determine la esctructura de la tabla vuelos.


### Funciones Dplyr ----

library("dplyr")

# filter(): retorna los casos (filas) que satisfacen un criterio
# select(): selecciona las columnas (variables) por nombre
# arrange(): reordena los casos (filas)
# mutate(): agrega nuevas columnas (variables)

## Filtros

# Encontrar todos los vuelos para SFO o OAK

f1 <- filter(vuelos, dest=="SFO" | dest=="OAK") # Forma 1, operadores logicos
f1 <- filter(vuelos, dest %in% c("SFO", "OAK")) # Forma 2, operador %in%

f1<- vuelos %>% 
  filter(dest %in% c("SFO", "OAK"))  # Forma 3, operador pipe %>% 

#* Seleccione de los anteriores vuelos todos aquellos que salieron entre la medianoche y las 5 de la mauana

#* Y de los anteriores solo los vuelos cuyo retraso a la llegada fue mas que doble del retraso en la salida

## Seleccionar Variables

# Seleccione solo las columnas de las variables arr_delay y dep_delay

s1 <- select(vuelos, arr_delay, dep_delay)
s1 <- select(vuelos, c(arr_delay, dep_delay))
s1 <- select(vuelos, ends_with("delay"))
s1 <- select(vuelos, contains("delay"))

# Seleccionar todas las variables salvo la time_hour

s2 <- select(vuelos, -time_hour)

#* Seleccionar todas las variables desde year hasta carrier

## Ordenar variables

# Ordenar los vuelos por fecha y hora de salida

o1 <- arrange(vuelos, month, day, hour, minute)

# Cuales fueron los vuelos con mayores retrasosu

o2_1 <- arrange(vuelos, desc(dep_delay))
o2_2 <- arrange(vuelos, desc(arr_delay))

## Mutate

#Calcular la nueva variable velocidad y guardarla en la columna speed

m1 <- mutate(vuelos, speed=distance / (air_time))
m2 <- mutate(m1, speed60=speed/60)

#* Calular una variable de distancia con las etiquetas 'short' para vuelos con distancia menor a 700 y 'long' en otro caso


## group_by

# Mutate --

# Calcule una nueva variable que sea la distancia promedio por cada ano.
g1 <- group_by(vuelos, year) %>% mutate(Distancia_promedio=mean(distance))

#* Calcule una nueva variable que sea la mediana del tiempo de vuelo por ano .

# Summarize-

# Calcule el numero de vuelos y el numero de destinos unicos para cada origen.
g3 <- group_by(vuelos, origin) %>% summarise(N_Vuelos=n(), N_Destinos=n_distinct(dest)) 

#* Calcule el la distancia minima promedio y maxima por cada destino

### Encadenamiento de Funciones ----

# Construir una tabla que solo contenga los vuelos provenientes de JKF, donde se calcule 
# la velocidad promedio de vuelo para cada uno de los destinos ordenado de mayor a menor.

# Forma 1

res1 <- filter(vuelos, origin=="JFK")
res2 <- mutate(res1, speed=distance / (air_time), Distancia=ifelse(distance < 700, "Short", "Long"))
res3 <- select(res2, origin, dest, distance, speed, Distancia)
res4 <- arrange(res3, desc(speed))
res5 <- group_by(res4, origin, dest,distance, Distancia)
res6 <- summarise(res5, MeanSpeed=mean(speed, na.rm = T))

# Forma 2

Forma2 <- summarise(
  group_by(
    arrange(
      select(
        mutate(
          filter(
            vuelos, origin=="JFK"),
          speed=distance / (air_time),
          Distancia=ifelse(distance < 700, "Short", "Long")), 
        origin, dest, distance, speed, Distancia), desc(speed)), 
    origin, dest,distance, Distancia), MeanSpeed=mean(speed, na.rm = T))

# Forma 3

Forma3<- vuelos %>% 
  filter(origin=="JFK") %>% 
  mutate(speed=distance / (air_time), Distancia=ifelse(distance < 700, "Short", "Long")) %>% 
  select(origin, dest, distance, speed, Distancia) %>%  
  arrange(desc(speed)) %>% 
  group_by(origin, dest,distance, Distancia) %>%
  summarise(MeanSpeed=mean(speed, na.rm = T))

## Union de Tablas ---

# Inner Join

InnJoin <- inner_join(Forma3, aeropuertos, by=c("dest"="faa")) %>% 
  select(origin: lon)

# Left Join

LeftJoin <- left_join(Forma3, aeropuertos, by=c("dest"="faa")) %>% 
  select(origin: lon)
sum(is.na(LeftJoin$name))

#Semi Join

SemiJoin <- semi_join(Forma3, aeropuertos, by=c("dest"="faa"))

#Anti Join

AntiJoin <- anti_join(Forma3, aeropuertos, by=c("dest"="faa"))


#* Construya una consulta a partir ed la tabla InnJoin que contenga al menos 4 verbos dplyr.
  
