#paquetes

paquetes <- c("here", "stringr", "stringi", "stopwords", "udpipe", "rex")

faltantes <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]

lapply(faltantes, install.packages)
lapply(paquetes, library, character.only = TRUE)

#leemos los archivos generados en el script anterior
subtitulos.normalizado <- read.csv(here("data", "processed", "subtitulos.normalizado.csv"))

lineas <- subtitulos.normalizado$subtitle

# TOKENIZACIÓN y LEMATIZACIÓN
#tokenizacióo es el proceso de dividir cada línea de subtítulo en palabras separadas
#lematización es el proceso de hacer que estos tokens coincidan con su forma base, e.g pasar verbos a infinitivo

#quitamos stopwords: palabras que a nivel semántico casi no aportan en la oración
stopwords.raw <- stri_trans_general(stopwords::stopwords("es"), "Latin-ASCII")
#quitamos cuantificadores pues se usarán luego como multiplicadores
stopwords.final <- setdiff(stopwords.raw, c("no", "sin", "ni", "muy", "mas"))

#paquetes:

paquetes <- c("udpipe", "dplyr") 

faltantes <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]

lapply(faltantes, install.packages)
lapply(paquetes, library, character.only = TRUE)

#se usará paquete "udpipe" para lematización (a pesar de haber mejores alternativas es el más sencillo y el único que encontré que no ocupaba python)
if(!file.exists(here("data", "raw", "spanish-gsd-ud-2.5-191206.udpipe"))) { #si el modelo para lematizar no está,lo instalamos
  modelo <- udpipe_download_model(language = "spanish", model_dir = here("data", "raw"))
}

modelo <- udpipe_load_model(file = here("data", "raw", "spanish-gsd-ud-2.5-191206.udpipe")) #cargamos modelo

lematizado.raw <- as.data.frame(udpipe_annotate(modelo, lineas, doc_id = subtitulos.normalizado$n)) #lematizamos

lematizado.normalizado <- lematizado.raw %>% 
  filter(upos != "PUNCT") %>% #quitamos puntuación (no se había hecho antes pues el lematizador la tomaba en cuenta)
  mutate(
    lemma = coalesce(lemma, token), #si hay NA en lemma, se usa el token
    lemma = str_to_lower(stri_trans_general(lemma, "Latin-ASCII")),
    doc_id = as.integer(doc_id) #lo dejaba como char así que se pasa a integer para el left_join de más adelante
  ) %>% 
  select(doc_id, lemma) %>% 
  rename( #nombres más representativos, sintaxis de dplyr nuevo.nombre = viejo.nombre
    subtitle.id = doc_id, 
    token = lemma
  ) %>% 
  left_join( #se hace un conteo de signos de "!","?" y palabras cuantificadoras por subtitle.id, para posteriormente usarlo como multiplicadores de emoción
    #unimos el conteo por subtitle.id
    data.frame(subtitle.id = subtitulos.normalizado$n,
               n.excl = str_count(lineas, "!"),
               n.interr = str_count(lineas, "\\?"),
               n.neg = str_count(lineas, rex(boundary, or("no","sin","ni","tampoco"), boundary)),
               n.afirm = str_count(lineas, rex(boundary, or("mas","muy","mucho","muchisimo"), boundary))
               ),
    by = "subtitle.id" 
  ) %>% 
  filter(!token %in% stopwords.final) #quitamos las stopwords anteriormente designadas

#se guardan como csv los archivos para uso posterior
write.csv(lematizado.normalizado, here("data", "processed", "lematizado.normalizado.csv"), row.names = FALSE)



