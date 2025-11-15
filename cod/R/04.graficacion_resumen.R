paquetes <- c("ggplot2", "dplyr", "tidyr", "here")
faltantes <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]
lapply(faltantes, install.packages)
lapply(paquetes, library, character.only = TRUE)

# leemos el archivo
puntajes.multiplicados <- read.csv(here("data","processed","puntajes.multiplicados.csv"))

emociones <- c("joy","anger","anticipation","disgust","fear",
               "sadness","surprise","trust","negative","positive")

paleta <- c(joy="#FFB703", anger="#E5383B", anticipation="#FB8500", disgust="#6A994E",
         fear="#023047", sadness="#577590", surprise="#8ECAE6", trust="#90BE6D",
         negative="#8338EC", positive="#FFD166")

# filtra por los segundos deseados
filtro <- puntajes.multiplicados %>% 
  filter(start >= 330, end <= 420)

# suma emociones
emociones.suma <- filtro %>% 
  summarise(across(all_of(emociones), sum, na.rm = TRUE))

# gráfico 1: todas menos pos/neg
solo.emociones <- setdiff(emociones, c("positive","negative"))

ggplot(
  pivot_longer(emociones.suma, all_of(solo.emociones),
               names_to = "emocion", values_to = "valor"),
  aes(emocion, valor, fill = emocion)) +
  geom_col() +
  coord_polar() +
  scale_fill_manual(values = paleta[solo.emociones]) +
  ggtitle("Resumen emociones")


# gráfico 2: solo pos/neg 
ggplot(
  pivot_longer(emociones.suma, c("positive","negative"),
               names_to = "emocion", values_to = "valor"),
  aes(emocion, valor, fill = emocion)) +
  geom_col() +
  scale_fill_manual(values = paleta[c("positive","negative")]) +
  ggtitle("Positivo vs Negativo")

