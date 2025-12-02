paquetes <- c("here", "dplyr", "textclean", "srt")

faltantes <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]

lapply(faltantes, install.packages)
lapply(paquetes, library, character.only = TRUE)

#leemos los archivos generados en el script anterior


lematizado.normalizado <- read.csv(here("data", "processed", "lematizado.normalizado.csv"))

#se cargan los subtitulos originales para adjuntarlos a cada ID puntuado
subtitulos.raw <- read_srt(here("data", "raw", "input", "subtitulos.srt"))
subtitulos.raw <- subtitulos.raw %>% 
  rename (subtitle.id = n)
subtitulos.raw$subtitle <- replace_html(subtitulos.raw$subtitle,symbol = FALSE) #se quitan las anotaciones para hacerlo legible como texto
subtitulos.raw$subtitle <- str_replace_all( subtitulos.raw$subtitle,"\\{[^}]*\\}", "")


lexicon.normalizado <- read.csv(here("data", "processed", "lexicon.normalizado.csv"))


#multiplicadores (pueden ser modificados a futuro según los resultados del modelo):

mult.exclamacion <- data.frame(
  emocion = c("joy", "anger", "anticipation", "disgust", "fear",
    "sadness", "surprise", "trust", "negative", "positive"),
  puntajes = c(1.15,1.15,1.10,1.10,1.10,0.9,1.3,1,1.10,1.15)
)

mult.interrogacion <- data.frame(
  emocion = c("joy", "anger", "anticipation", "disgust", "fear",
      "sadness", "surprise", "trust", "negative", "positive"),
  puntajes = c(0.95,0.9,1.15,0.95,1,0.9,1.25,0.95,1,1)
)

mult.neg <- data.frame(
  emocion = c("joy", "anger", "anticipation", "disgust", "fear",
              "sadness", "surprise", "trust", "negative", "positive"),
  puntajes = c(0.95,0.95,0.95,0.95,0.95,0.95,0.95,0.95,1.1,0.95)
)

mult.afirm <-  data.frame(
  emocion = c("joy", "anger", "anticipation", "disgust", "fear",
              "sadness", "surprise", "trust", "negative", "positive"),
  puntajes = c(1.1,1.1,1.1,1.1,1.1,1.1,1.1,1.1,1,1)
)

emociones <- c("joy", "anger", "anticipation", "disgust", "fear",
               "sadness", "surprise", "trust", "negative", "positive")

#asociamos cada palabra a sus puntajes de emoción base (sin cuantificadores)

puntajes.raw <- lematizado.normalizado %>% 
  left_join(lexicon.normalizado, by = "token") %>% #asociamos a cada token su emocion
  mutate(across(all_of(emociones), function(columna) {coalesce(columna, 0 )})) %>% #tratamos los NA de la operación anterior(si un token no tiene entrada en el lexicón se pone 0)
  group_by(subtitle.id, n.excl, n.interr, n.neg, n.afirm) %>% #agrupamos por los ID
  summarise(
    across(all_of(emociones), sum) #sumamos las emociones en cada una de las frases correspondientes a los ID
  ) %>% 
  left_join(subtitulos.raw, by = "subtitle.id") %>% #agregamos las frases en sí
  select(subtitle.id, subtitle, everything()) #se ordena de forma más natural
  
#aplicar multiplicadores

puntajes.multiplicados <- puntajes.raw
for (emo in emociones) {
  fac.ex <- mult.exclamacion$puntajes[mult.exclamacion$emocion == emo] #tomamos cada factor según la emoción correspondiente
  fac.in <- mult.interrogacion$puntajes[mult.interrogacion$emocion == emo]
  fac.neg <- mult.neg$puntajes[mult.neg$emocion == emo]
  fac.afi <- mult.afirm$puntajes[mult.afirm$emocion == emo]
  
  puntajes.multiplicados[[emo]] <- puntajes.raw[[emo]] * #multiplicación vectorizada por columna, para cada emoción
    (fac.ex ^ puntajes.raw$n.excl) * 
    (fac.in ^ puntajes.raw$n.interr) *
    (fac.neg ^ puntajes.raw$n.neg) *
    (fac.afi ^ puntajes.raw$n.afirm) 
}

#cuando la línea no tiene palabras del lexicón pero sí ! o ?, se le suma un valor base pequeño

mult.exclamacion.base <- data.frame(
  emocion = c("joy", "anger", "anticipation", "disgust", "fear",
              "sadness", "surprise", "trust", "negative", "positive"),
  puntajes = c(0.05, 0.07, 0.04, 0.03, 0.03, 0.04, 0.10, 0.03, 0.07, 0.04)
)

mult.interrogacion.base <- data.frame(
  emocion = c("joy", "anger", "anticipation", "disgust", "fear",
              "sadness", "surprise", "trust", "negative", "positive"),
  puntajes = c(0.02, 0.03, 0.12, 0.01, 0.04, 0.03, 0.9, 0.02, 0.03, 0.01)
)


#cuántas palabras con emoción tiene cada subtítulo
conteo.emociones <- lematizado.normalizado %>% 
  left_join(lexicon.normalizado, by = "token") %>% 
  mutate(tiene.emocion = if_any(all_of(emociones), ~ .x == 1)) %>% 
  count(subtitle.id, wt = tiene.emocion, name = "n.coincidencias")

puntajes.multiplicados <- puntajes.multiplicados %>% 
  left_join(conteo.emociones, by = "subtitle.id") %>% 
  mutate(n.coincidencias = coalesce(n.coincidencias, 0))

#aplicar los valores base
for (emo in emociones) {
  
  base.ex <- mult.exclamacion.base$puntajes[mult.exclamacion.base$emocion == emo]
  base.in <- mult.interrogacion.base$puntajes[mult.interrogacion.base$emocion == emo]
  
  ajuste.ex <- ifelse(
    puntajes.multiplicados$n.coincidencias == 0 & puntajes.multiplicados$n.excl > 0,
    base.ex * puntajes.multiplicados$n.excl,
    0
  )
  
  ajuste.in <- ifelse(
    puntajes.multiplicados$n.coincidencias == 0 & puntajes.multiplicados$n.interr > 0,
    base.in * puntajes.multiplicados$n.interr,
    0
  )
  
  puntajes.multiplicados[[emo]] <- puntajes.multiplicados[[emo]] + ajuste.ex + ajuste.in
}

write.csv(puntajes.multiplicados, here("data", "processed", "puntajes.multiplicados.csv"), row.names = FALSE)