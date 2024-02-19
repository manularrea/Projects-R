# Nombre: Manuela 
# Apellidos: Larrea Gómez
# ====================================
# Ejercicio 1 - Práctica R
# ------------------------------------
  
" 1. Lee el fichero spotify.csv y se asigna a una variable llamada spotify" 

spotify <- read_csv("dat/spotify.csv", show_col_types = FALSE)

View(spotify)

" 2. Clase del objeto spotify"

class(spotify)

" 3. Tipo de cada columna y muestra de ejemplo"

str(spotify)

" 4. Muestra los primeros 18 registros de dataset"

head(spotify, 18)

" 5. Ultimos 25 registos del dataset"

tail(spotify, 25)

" 6. Dimensiones del dataset"

dim(spotify)

" 7. Nombres de las variables del dataset "

names(spotify)

" 8. Elimina la variable Index "

spotify <- spotify[, !(names(spotify) == "Index")]
names(spotify)

" 9. Renombra las variables para limpiarlas (elimina espacios, paréntesis, mayúsculas...)"

names(spotify) <- c("title", "artist", "top_genre", "year", "beats_per_minute", 
                    "energy", "danceability", "loudness", "liveness", "valence", 
                    "duration", "acousticness", "speechiness", "popularity")
names(spotify)

" 10. Comprueba si alguna de las variables contiene NAs."

any(is.na(spotify))

"  11. La variable Top Genre debería ser de tipo factor. Crea un factor SIN 
etiquetas para dicha columna y sin orden. Asígnalo a la columna de nuevo."

spotify$top_genre <- factor(spotify$top_genre, ordered = FALSE)

class(spotify$top_genre)

" 12. Selecciona las 30 primeras filas y todas las columnas menos las tres últimas 
(sólo con índices positivos tanto en filas como en columnas)."

spotify[1:30, 1:(ncol(spotify)-3)]

" 13. Selecciona las 30 primeras filas y todas las columnas menos las tres últimas 
(sólo con índices negativos tanto en filas como en columnas)."

spotify[-(31:nrow(spotify)), -(ncol(spotify) - 2):-ncol(spotify)]

" 14. ¿Cuántos artistas distintos aparecen en el dataset?"

length((unique(spotify$artist)))

" 15. ¿Cuántas canciones distintas hay de ABBA?"

length(unique(spotify$title[spotify$artist == "ABBA"]))

" 16. ¿Cuántos registros son de género pop?"

length(spotify$title[spotify$top_genre == "pop"])

" 17. Crea una función, llamada calculate_year_since_release que represente la 
antigüedad de una canción. La función recibe como argumento el año de lanzamiento 
de la canción y asigna a cada registro un número en función de la antigüedad de la 
siguiente forma:
  1. si la canción tiene una antigüedad de hasta 10 años (𝑥 ≤ 10)
  2. si la canción tiene una antigüedad de entre 10 y 20 años (10 < 𝑥 ≤ 20)
  3. si la canción tiene una antigüedad de entre 20 y 30 años (20 < 𝑥 ≤ 30)
  4. si la antigüedad de la canción es de más de 30 años ( 𝑥 > 30)
  
Ejemplifica en el código que funciona correctamente con varios casos.
PISTA: puedes ayudarte de la librería lubridate para extraer el año actual."

library(lubridate)

calculate_year_since_release <- function(year){ 
  current_year <- lubridate::year(Sys.Date())
  age_of_the_song <- current_year - year
  if (age_of_the_song <= 10){
    return(1)
  }
  if (age_of_the_song > 10 & age_of_the_song <=20 ){
    return(2)
  }
  if (age_of_the_song > 20 & age_of_the_song <= 30){
    return(3)
  }
  else {
    return(4)
  }
}

# ----------------------------------------
# Ejemplos de funcionamiento : 
# ----------------------------------------

# Calcular la antigüedad de una canción lanzada en 2023

print(calculate_year_since_release(2023))

# Calcular la antigüedad de una canción lanzada en 2012

print(calculate_year_since_release(2012))

# Calcular la antigüedad de una canción lanzada en 1997

print(calculate_year_since_release(1997))

# Calcular la antigüedad de una canción lanzada en 1983

print(calculate_year_since_release(1983))

" 18. Mediante una llamada a una de las funciones apply aplica la función anterior 
a toda la columna Year. El resultado obtenido debe ser almacenado en una nueva 
variable del dataframe llamada year_since_release."

spotify$year_since_release <- sapply(spotify$year, calculate_year_since_release)

" 19. Esta nueva variable year_since_release debería ser de tipo factor. Crea un factor con
etiquetas para dicha columna y asígnalo a la columna de nuevo. Es decir, el factor tendrá
tantas categorías como valores tiene la variable y cada una de esas categorías debe tener
una etiqueta que indique la antigüedad de dicha canción. El factor deberá tener orden
indicando la antigüedad, es decir:

Más de 30 años < entre 20-30 años < entre 10-20 años < máximo 10 años"

spotify$year_since_release <- factor(spotify$year_since_release,
                                     levels = c(1, 2, 3, 4),
                                     labels = c("máximo 10 años", "entre 10-20 años", 
                                                "entre 20-30 años", "Más de 30 años" ),
                                     ordered = TRUE)

unique(spotify$year_since_release)

" 20. ¿Cuántas canciones han sido publicadas en los últimos 10 años?"

length(unique(spotify$title[spotify$year_since_release =="máximo 10 años"]))

" 21. ¿De entre las canciones publicadas en los últimos 10 años hay alguna de
género “latin”? En caso afirmativo, ¿cuál es el título de la canción?"

latin_song_10year <- unique(spotify$title[(spotify$year_since_release =="máximo 10 años")
                                          & (spotify$top_genre == "latin")])

number_song_latin_10year <- length(latin_song_10year)

print(paste("Sí, hay", number_song_latin_10year, 
            "canción publicada en los últimos 10 años de género latino, su nombre es: ", latin_song_10year))

" 22. Pinta un histograma de la variable Year."

hist(spotify$year, main = "Histogram songs published by year", 
     xlab = "Year",
     ylab = "Song count",
     col = "lightblue")

" 23. Pinta el boxplot de la variable duration."

boxplot(spotify$duration, main = "Duration boxplot", ylab = " Song duration (sec)")

" 24. Crea un vector con nombre que contenga tres elementos: el mínimo de la duración, la
media de la duración y la duración máxima de las canciones."

duration_basic_stats <- c(Minima = min(spotify$duration),
                          Media = mean(spotify$duration),
                          Máximo = max(spotify$duration))
print(duration_basic_stats)

" 25. ¿Cuántas canciones tienen una duración mayor de 100 segundos?"

length(spotify$duration[spotify$duration > 100])

" 26. ¿Qué variables son numéricas? PISTA: utiliza sapply junto con la función is.numeric.
Guardarlo en una variable. Cuidado con aquellas variables que detecta como numéricas,
pero realmente no lo son."

numeric_variables <- sapply(spotify, function(x) is.numeric(x) && !is.factor(x))

numeric_variables <- names(spotify)[numeric_variables] #Me interesa imprimir solo el nombre de las variables.

print(numeric_variables)

" 27. Utilizando la variable con el resultado anterior, selecciona aquellas columnas
numéricas y calcula la media de aquellas en las que tenga sentido."

numeric_variables <- numeric_variables[-1]  #Elimino el elemento "year". No tiene sentido calcular su media

numeric_data <- spotify[numeric_variables] #Recupero la data de cada columna para calcular la media.

media_by_col <- sapply(numeric_data, mean)

print(media_by_col)

" 28. Obtén los cuartiles de la variable Year."

quantile(spotify$year, probs=c(0.25, 0.5, 0.75))

" 29. Obtén los deciles de la variable Year."

quantile(spotify$year, probs=seq(0, 1, by=0.1))

" 30. Obtén los estadísticos básicos de todas las variables en un solo comando."

summary(spotify)

" 31. ¿Qué género tiene la canción más bailable? ¿Y la menos?"

spotify$danceability[spotify$danceability == max(spotify$danceability)] #Verifico cuántos registros tienen el máx. 

genre_max_danceability <- matrix(spotify$top_genre[spotify$danceability == max(spotify$danceability)])

spotify$danceability[spotify$danceability == min(spotify$danceability)] #Verifico cuántos registros tienen el mín. 

genre_min_danceability <- matrix(spotify$top_genre[spotify$danceability == min(spotify$danceability)])

print (paste("Los géneros de las canciones más bailables son: ", 
             genre_max_danceability[1],"y ", 
             genre_max_danceability[2], ", con un valor de 96 dB cada uno" ))

print (paste("El género de la canción menos bailable es: ", 
             genre_min_danceability, "con un valor de 10 dB"))

" 32. Ordena el dataset por la variable Year de manera descendente. Inspecciona 
los primeros resultados para comprobar que se ha ordenado como se pide.

NOTA: Una vez realizadas todas las modificaciones, guarda el data frame en un csv 
para utilizarlo en los siguientes ejercicios."

spotify_ordenado_1 <- spotify[order(-spotify$year), ]

write.csv(spotify_ordenado_1, "dat/spotify_ordenado_1.csv", row.names = FALSE)

head(spotify_ordenado_1)

