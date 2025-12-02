paquetes <- c("ggplot2", "dplyr", "tidyr", "here")
faltantes <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]
lapply(faltantes, install.packages)
lapply(paquetes, library, character.only = TRUE)

# leemos el archivo
puntajes.multiplicados <- read.csv(here("data","processed","puntajes.multiplicados.csv"))

emociones <- c("joy","anger","anticipation","disgust","fear",
               "sadness","surprise","trust","negative","positive")

paleta <- c(joy="#FFB703", anger="#E5383B", anticipation="#FB8500", disgust="#3A5A40",
            fear="#023047", sadness="#577590", surprise="#8ECAE6", trust="#B5E48C",
            negative="#8338EC", positive="#FFD166")

etiquetas.emociones <- c(
  joy = "alegría",
  anger = "ira",
  anticipation = "anticipación",
  disgust = "asco",
  fear = "miedo",
  sadness = "tristeza",
  surprise = "sorpresa",
  trust = "confianza",
  negative = "negativo",
  positive = "positivo"
)


# filtra por los segundos deseados
filtro <- puntajes.multiplicados %>% 
  filter(start >= 1000, end <= 1318)

# suma emociones
emociones.suma <- filtro %>% 
  summarise(across(all_of(emociones), sum, na.rm = TRUE))

# gráfico 1: todas menos pos/neg
solo.emociones <- setdiff(emociones, c("positive","negative"))


# intervalos para el gráfico radial
max_val <- max(pivot_longer(emociones.suma, all_of(solo.emociones))$value)
breaks_rad <- pretty(c(0, max_val), n = 4)

radial_labels <- data.frame(
  emocion = solo.emociones[1],   # sobre una sola emoción
  valor = breaks_rad,
  label = breaks_rad
)

ggplot(
  pivot_longer(emociones.suma, all_of(solo.emociones),
               names_to = "emocion", values_to = "valor"),
  aes(emocion, valor, fill = emocion)) +
  geom_col() +
  coord_polar() +
  scale_fill_manual(values = paleta[solo.emociones]) +
  scale_x_discrete(labels = etiquetas.emociones) +
  geom_text(
    data = radial_labels,
    aes(x = emocion, y = valor, label = label),
    inherit.aes = FALSE,
    size = 3,
    vjust = -0.2
  ) +
  labs(
    x = "",
    y = "",
    title = "Resumen emociones"
  ) +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  )


# gráfico 2: solo pos/neg 
ggplot(
  pivot_longer(emociones.suma, c("positive","negative"),
               names_to = "emocion", values_to = "valor"),
  aes(emocion, valor, fill = emocion)) +
  geom_col() +
  scale_fill_manual(values = paleta[c("positive","negative")]) +
  scale_x_discrete(labels = etiquetas.emociones) +
  labs(
    title = "Resumen emociones"
  ) +
  theme(legend.position = "none")

