#paquetes

paquetes <- c("shiny", "ggplot2", "dplyr", "tidyr", "here")

faltantes <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]

lapply(faltantes, install.packages)
lapply(paquetes, library, character.only = TRUE)

#leemos datos procesados

puntajes.multiplicados <- read.csv(
  here("data", "processed", "puntajes.multiplicados.csv")
)

emociones <- c(
  "joy","anger","anticipation","disgust","fear",
  "sadness","surprise","trust","negative","positive"
)

paleta.emociones <- c(joy="#FFB703", anger="#E5383B", anticipation="#FB8500", disgust="#6A994E",
            fear="#023047", sadness="#577590", surprise="#8ECAE6", trust="#90BE6D",
            negative="#8338EC", positive="#FFD166")

duracion.total <- max(puntajes.multiplicados$end, na.rm = TRUE)

#funciones auxiliares

buscar.linea.por.tiempo <- function(datos, tiempo.segundos) {
  fila = datos %>%
    filter(start <= tiempo.segundos, end > tiempo.segundos)
  
  if (nrow(fila) == 0) {
    fila = datos %>%
      filter(start <= tiempo.segundos) %>%
      slice_tail(n = 1)
  }
  
  return(fila)
}

hacer.flor <- function(fila, emociones, paleta.emociones) {
  
  datos.largos = fila %>%
    select(all_of(emociones)) %>%
    pivot_longer(
      cols = everything(),
      names_to = "emocion",
      values_to = "valor"
    ) %>%
    mutate(
      valor = ifelse(is.na(valor), 0, valor),
      emocion = factor(emocion, levels = emociones)
    )
  
  etiquetas = datos.largos %>%
    filter(valor > 0)
  
  maximo = max(datos.largos$valor)
  margen = ifelse(maximo > 0, maximo * 0.10, 0.1)
  
  ggplot(datos.largos, aes(x = emocion, y = valor, fill = emocion)) +
    geom_col(width = 0.95, alpha = 0.9, show.legend = FALSE) +
    geom_text(
      data = etiquetas,
      aes(label = emocion, y = valor + margen),
      size = 4, vjust = 0
    ) +
    scale_fill_manual(values = paleta.emociones) +
    coord_polar() +
    labs(title = fila$subtitle) +
    theme_void(base_size = 12) +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        size = 16,
        margin = margin(b = 10)
      ),
      plot.background = element_rect(fill = "white", colour = NA)
    )
}

#UI

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background: #ffffff; }
      .contenedor { max-width: 1100px; margin: 0 auto; }
      #video { width: 100%; border-radius: 12px;
               box-shadow: 0 10px 30px rgba(0,0,0,0.08); }
      .linea-video { margin: 8px 0 2px 0; font-size: 0.95rem; }
    ")),
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function() {
        var video = document.getElementById('video');
        if (!video) return;

        function enviarTiempo() {
          var t = video.currentTime || 0;
          Shiny.setInputValue('tiempo_video', t, {priority: 'event'});
        }

        video.addEventListener('timeupdate', enviarTiempo);
        video.addEventListener('seeked', enviarTiempo);
        video.addEventListener('playing', enviarTiempo);

        var inputLocal = document.getElementById('video_local');
        if (inputLocal) {
          inputLocal.addEventListener('change', function(e) {
            var archivo = e.target.files && e.target.files[0];
            if (!archivo) return;
            var url = URL.createObjectURL(archivo);
            video.src = url;
            video.load();
            video.play().catch(function(){});
          });
        }
      });
    "))
  ),
  
  div(
    class = "contenedor",
    
    h2("Visualizador de emociones por subtítulo"),
    
    tags$video(
      id = "video",
      controls = TRUE,
      preload = "metadata",
      tags$source(src = "pelicula.mp4", type = "video/mp4"),
      tags$source(src = "pelicula.mkv", type = "video/x-matroska"),
      "Tu navegador no soporta video HTML5."
    ),
    
    div(
      class = "linea-video",
      tags$b("Cargar video: "),
      tags$input(
        id = "video_local",
        type = "file",
        accept = "video/*,.mkv,.mp4,.webm"
      )
    ),
    
    plotOutput("grafico.flor", height = 540)
  )
)

#SERVER

server <- function(input, output, session) {
  
  datos = puntajes.multiplicados %>%
    select(
      subtitle.id, subtitle, start, end,
      all_of(emociones)
    )
  
  output$grafico.flor <- renderPlot({
    tiempo = req(input$tiempo_video)
    
    # pequeño ajuste fijo para compensar el retraso natural de shiny/ggplot
    tiempo.ajustado = tiempo - 0.05
    tiempo.ajustado = max(0, min(duracion.total, tiempo.ajustado))
    
    fila = buscar.linea.por.tiempo(datos, tiempo.ajustado)
    
    # si estamos antes del primer subtítulo o hay silencio largo, no mostramos nada
    if (nrow(fila) == 0) return(NULL)
    
    silencio = tiempo.ajustado - fila$end
    
    if (silencio > 3) return(NULL) 
    
    hacer.flor(fila, emociones, paleta.emociones)
  })
}

shinyApp(ui, server)
