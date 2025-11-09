#paquetes

paquetes <- c("here", "stringr", "stringi", "stopwords")

faltantes <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]

lapply(faltantes, install.packages)
lapply(paquetes, library, character.only = TRUE)

#leemos los archivos generados en el script anterior
subtitulos.normalizado <- read.csv(here("data", "processed", "subtitulos.normalizado.csv"))

lineas <- subtitulos.normalizado$subtitle

# TOKENIZACIÓN (proceso de dividir cada línea de subtítulo en palabras separadas)


#quitamos stopwords: palabras que a nivel semántico casi no aportan en la oración
stopwords.raw <- stri_trans_general(stopwords::stopwords("es"), "Latin-ASCII")
#quitamos cuantificadores pues se usarán luego como multiplicadores
stopwords.final <- setdiff(stopwords.raw, c("no", "sin", "ni", "muy", "mas")) 


