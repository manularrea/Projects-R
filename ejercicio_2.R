# Nombre: Manuela 
# Apellidos: Larrea Gómez
# ====================================
# Ejercicio 2 - Práctica R
# ------------------------------------

library(readr)
library(tidyr)
library(reshape2)
library(dplyr)
library(data.table)

# Se trabajará con el archivo de salida del ejercicio 1 "spotify_ordenado_1"

spotify_2 <- read_csv("dat/spotify_ordenado_1.csv", show_col_types = FALSE)

# ====================================
# Preparación (tidyr, reshape2)
# ------------------------------------

" 1. Para cada tipo de antigüedad (usando la variable creada year_since_release), 
calcula cuántas canciones hay de cada uno. 
Guarda el resultado en una nueva variable que contendrá el dataset con las columnas: 
top_genre, year_since_release y n_songs. "

#Para imprimir el número de canciones por cada tipo de antigüedad. 
for (categoria in c("máximo 10 años", "entre 10-20 años", "entre 20-30 años", "Más de 30 años")) {
  n_canciones <- unique(length(spotify_2$title[spotify_2$year_since_release == categoria]))
  print(paste(categoria, ": ", n_canciones, "canciones"))
}

#Creación del dataset solicitado
spotify_by_age <- spotify_2 %>%      
  group_by(top_genre, year_since_release) %>%
  summarise(n_songs = n())

View(spotify_by_age)

"2. Crea un wide dataset que tenga las siguientes variables:
    - top_genre
    - Máximo 10 años
    - Entre 10-20 años
    - Entre 20-30 años
    - Más de 30 años "

# --------------------------
# Usando reshape2

wide_by_age_1 <- dcast(spotify_by_age, 
                       top_genre ~ year_since_release, 
                       value.var = "n_songs") 

# --------------------------
# Usando tidyr

wide_by_age_2 <- pivot_wider(spotify_by_age, 
                             names_from = year_since_release, 
                             values_from = n_songs)

" 3. Realiza el proceso inverso desde el long dataset anterior para obtener el dataset original 
con las siguientes columnas: 
    - top_genre
    - year_since_release
    - n_songs"

# --------------------------
# Usando reshape2

long_by_age_1 <- melt(wide_by_age_1, 
                     id.vars = "top_genre", 
                     variable.name = "year_since_release", 
                     value.name = "n_songs")

" Para regresar al dataset original a partir del dataset en formato wide, es necesario hacer un
na.omit() para eliminar los registros que tengan valores faltantes "

long_by_age_1 <- na.omit(long_by_age_1)

# --------------------------
# Usando tidyr

long_by_age_2 <- pivot_longer(wide_by_age_2,
                              cols = 2:5,
                              names_to = "year_since_release",
                              values_to = "n_songs" )

" Para regresar al dataset original a partir del dataset en formato wide, es necesario hacer un
na.omit() para eliminar los registros que tengan valores faltantes "

long_by_age_2 <- na.omit(long_by_age_2)

# ====================================
# Manipulación (dplyr, data.table)
# ------------------------------------

" 1. Calcula, para cada género la duración y el número de canciones (títulos)."

# --------------------------
# Usando dplyr

genre_by_duration_and_song_1 <- spotify_2 %>%
  group_by(top_genre) %>%
  summarise(average_duration = mean(duration, na.rm = TRUE), n_songs = n())

genre_by_duration_and_song_1

# --------------------------
# Usando data.table

genre_by_duration_and_song_2 <- data.table(spotify_2)[, .(average_duration = mean(duration, na.rm = TRUE),
                                              n_songs = .N),
                                          by = top_genre][order(top_genre)]

genre_by_duration_and_song_2

" 2. ¿Cuál es el género con más canciones?"

# --------------------------
# Usando dplyr

genre_with_more_songs_1 <- spotify_2 %>%
  group_by(top_genre) %>%
  summarise(n_canciones = n()) %>%
  filter(n_canciones == max(n_canciones))

genre_with_more_songs_1 

# --------------------------
# Usando data.table

spotify_2_dt <- data.table(spotify_2)
genre_with_more_songs_2 <- spotify_2_dt[, .(n_canciones = .N), by = top_genre]
max_songs <- max(genre_with_more_songs_2$n_canciones)
genre_with_more_songs_2[max_songs == n_canciones]

" 3. ¿Cuál es el artista con más canciones de los últimos 10 años?"

# --------------------------
# Usando dplyr

spotify_10years <- spotify_2 %>% filter(year_since_release == "máximo 10 años")

artist_counts_1 <- spotify_10years %>% count(artist)

artist_with_most_songs_1 <- artist_counts_1 %>% filter(n == max(n))

artist_with_most_songs_1

# --------------------------
# Usando data.table. 


spotify_10years <- data.table(spotify_2)[year_since_release == "máximo 10 años",]

artist_counts_2 <- spotify_10years[, list(n_songs = .N), by = artist]

artist_with_most_songs_2 <- artist_counts_2[n_songs == max(n_songs), .(artist, n_songs)]

artist_with_most_songs_2


" 4. De las canciones con una antigüedad mayor de 30 años de género pop, 
¿cuántas tenían una duración mayor de 100 segundos?"

# --------------------------
# Usando dplyr

spotify_30years_1 <- spotify_2 %>%
  filter(year_since_release == "Más de 30 años") %>%
  filter(top_genre == "pop") %>%
  filter(duration >= 100)

length(spotify_30years_1)

# --------------------------
# Usando data.table

spotify_30years_2 <- data.table(spotify_2)[year_since_release == "Más de 30 años" &
                                             top_genre == "pop" &
                                             duration > 100, ]

length(spotify_30years_2)

" 5. Ordena los artistas en función de lo bailable que es su canción dentro del género pop 
(ordena de mayor a menor)."

# --------------------------
# Usando dplyr

max_bailable_pop_1 <- spotify_2 %>%
  filter(top_genre == "pop") %>%
  arrange(desc(danceability)) %>%
  select(artist, danceability)

max_bailable_pop_1

# --------------------------
# Usando data.table

max_bailable_pop_2 <- data.table(spotify_2)[top_genre=="pop", .(artist, danceability)][order(-danceability)]

print(max_bailable_pop_2)
