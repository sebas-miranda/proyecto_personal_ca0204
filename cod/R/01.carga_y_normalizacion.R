#paquetes:

paquetes <- c("here", "srt", "dplyr", "stringr", "stringi", "textclean", "rex")

faltantes <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]

#descargamos los paquetes faltantes en el sistema:
lapply(faltantes, install.packages)

#cargamos todos los paquetes a la sesión
lapply(paquetes, library, character.only = TRUE)


#adicionalmente, se usa paquete "here" para poder usar rutas relativas al proyecto
#y asegurar que el código sea reproducible en otras máquinas

#asimismo, se usa el paquete "srt" para guardar el archivo .srt donde vienen
#los subtítulos como un df:
subtitulos.raw <- read_srt(here("data", "raw", "input", "subtitulos.srt"))


#se importa el lexicón (diccionario de emociones) inicial para poder limpiarlo, 
#dado que como originalmente se encontraba en inglés y se hizo una traducción
#a español, muchas palabras con varios significados resultaron duplicadad,
#por tanto esto debe ser eliminado (todo esto se documentará en el README) 
#posteriormente
lexicon.raw <- read.table(here("data","raw","Spanish-NRC-EmoLex.txt"),
                                    sep = "\t", header = TRUE)

mayoria <- function(columna, n.total) {
  n.positivos <- sum(columna, na.rm = TRUE) #sumar cuántos 1 hay por emoción en cada grupo
  umbral <- max(1, integer(n.total/2)) # ó 1 ó la mitad parseada de la cantidad de veces que sale la palabra
  resultado <- if_else(n.positivos >= umbral, 1, 0) # si la emoción sale varias veces entre las palabras, se deja
  return(resultado)
}


lexicon.normalizado <- lexicon.raw %>% 
  mutate(Spanish.Word = Spanish.Word %>% 
           str_to_lower() %>% #usamos paquete "stringr" para normalizar todo a minúscula
           stringi::stri_trans_general("Latin-ASCII")) %>% ##usamos "stringi" para quitar tildes y ñ (pasar letras latinas a su forma ASCII)
  group_by(Spanish.Word) %>% # se agrupa por palabras en español que quedaron con mismo significado por traducción inglés a español
  summarise(
    n = n(),
    joy = mayoria(joy, n()),
    anger = mayoria(anger, n()),
    anticipation = mayoria(anticipation, n()),
    disgust = mayoria(disgust, n()),
    fear = mayoria(fear, n()),
    sadness = mayoria(sadness, n()),
    surprise = mayoria (surprise, n()),
    trust = mayoria(trust, n()),
    negative = mayoria(negative, n()),
    positive = mayoria(positive, n()),
  ) %>% 
  rename(token = Spanish.Word)

limpieza <- function(linea) {
  x = linea
  x = textclean::replace_html(x, symbol = FALSE) #se usa el paquete "textclean" para quitar anotaciones que le dan un estilo de texto diferente a los subtítulo, tipo <i>
  x = replace_url(x) #se quita URL proveniente de publicidad del sitio de descarga de los .srt
  
  # ahora se usará "stringr" para reemplazos específicos de los archivos de subtitulos, el paquete "rex" para darle input legible a stringr
  #acá se crean las expresiones para que stringr las lea, pues deben ser en un formato especial llamado "regex"
  entre.corchetes = rex("[", except_any_of("]"), "]") #expresiones de la forma [cualquier cosa]
  entre.parentesis = rex("(", except_any_of(")"), ")") #expresiones de la forma (cualquier cosa)
  guiones = rex(some_of("-", "–", "—"), zero_or_more(spaces)) #expresiones de dashes seguidos de 0 o más espacios
  espacios = rex(one_or_more(spaces)) #uno o más espacios seguidos

  x = stringr::str_replace_all(x, entre.corchetes, " ")
  x = str_replace_all(x, entre.parentesis, " ")
  x = str_replace_all(x, guiones, " ")
  x = str_remove_all(x, "\\\\") 
  x = str_remove_all(x, '"')
  x = str_remove_all(x, "¿")
  x = str_remove_all(x, "¡")
  x = str_replace_all(x, espacios, " ")
  x = str_trim(x)
  x[x == ""] <- NA_character_ #si alguna línea entera queda vacía

  return(x)
}

subtitulos.normalizado <- subtitulos.raw %>% 
  mutate(subtitle = limpieza(subtitle))

#se guardan como csv los archivos para uso posterior
write.csv(subtitulos.normalizado, here("data", "processed", "subtitulos.normalizado.csv"), row.names = FALSE)
write.csv(lexicon.normalizado, here("data", "processed", "lexicon.normalizado.csv"), row.names = FALSE)


  

