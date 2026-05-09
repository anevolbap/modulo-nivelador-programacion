## auxiliares.R
##
## Funciones auxiliares para usar como caja negra en los ejercicios del curso.
## Cargar con: source("ejercicios/auxiliares.R")
##
## El objetivo es que el alumno pueda concentrarse en la lógica de cada clase
## sin distraerse con detalles de I/O o de gráficos. Cada función está
## documentada arriba con su firma y un ejemplo de uso.


## ---------------------------------------------------------------------------
## Carga de datasets
## ---------------------------------------------------------------------------

#' Carga datos/bikeshare.csv con tipos correctos.
#'
#' bikeshare.csv proviene del paquete ISLR2 y registra el uso por hora del
#' sistema de bicicletas de Washington DC durante 2011 y 2012. El archivo está
#' separado por espacios (no por comas), por lo que esta función envuelve
#' read.table con los argumentos correctos y convierte algunas columnas a
#' factor con etiquetas legibles.
#'
#' Devuelve un data.frame con columnas:
#'   season (factor), mnth (chr), day (int), hr (int),
#'   holiday (lgl), weekday (factor), workingday (lgl),
#'   weathersit (chr), temp, atemp, hum, windspeed (num normalizadas),
#'   casual, registered, bikers (counts).
#'
#' Ejemplo:
#'   bicis <- cargar_bikeshare()
#'   head(bicis)
#'   summary(bicis$bikers)
cargar_bikeshare <- function(ruta = "datos/bikeshare.csv") {
    df <- read.table(ruta, header = TRUE, stringsAsFactors = FALSE)
    df$season <- factor(df$season,
                        levels = 1:4,
                        labels = c("invierno", "primavera", "verano", "otono"))
    df$weekday <- factor(df$weekday,
                         levels = 0:6,
                         labels = c("dom", "lun", "mar", "mie", "jue", "vie", "sab"))
    df$holiday <- as.logical(as.numeric(df$holiday))
    df$workingday <- as.logical(df$workingday)
    df
}


## ---------------------------------------------------------------------------
## Gráficos
## ---------------------------------------------------------------------------

#' Grafica la distribución de un vector bootstrap con su intervalo del 95%.
#'
#' Dibuja un histograma de los valores bootstrap, marca la media con una línea
#' continua roja, y los percentiles 2.5% y 97.5% con líneas punteadas azules.
#' Devuelve (de forma invisible) el vector de extremos del intervalo.
#'
#' Ejemplo:
#'   medias_boot <- replicate(1000, mean(sample(airquality$Wind, replace = TRUE)))
#'   graficar_bootstrap(medias_boot, "media bootstrap del viento")
graficar_bootstrap <- function(boot, etiqueta = "estadistico bootstrap") {
    ic <- quantile(boot, c(0.025, 0.975), na.rm = TRUE)
    hist(boot,
         breaks = 30,
         main   = etiqueta,
         xlab   = etiqueta,
         col    = "gray85",
         border = "white")
    abline(v = mean(boot, na.rm = TRUE), col = "red",  lwd = 2)
    abline(v = ic,                       col = "blue", lwd = 2, lty = 2)
    legend("topright",
           legend = c("media", "IC 95%"),
           col    = c("red", "blue"),
           lwd    = 2,
           lty    = c(1, 2),
           bty    = "n")
    invisible(ic)
}

#' Grafica histograma + boxplot lado a lado para un vector numerico.
#'
#' Útil para mirar de un vistazo la distribución y los valores extremos al
#' mismo tiempo. Restablece el layout gráfico al salir.
#'
#' Ejemplo:
#'   graficar_hist_box(airquality$Wind, "viento (mph)")
graficar_hist_box <- function(x, etiqueta = "") {
    par_anterior <- par(mfrow = c(1, 2))
    on.exit(par(par_anterior))
    hist(x,
         main   = paste("Histograma:", etiqueta),
         xlab   = etiqueta,
         col    = "gray85",
         border = "white")
    boxplot(x,
            main       = paste("Boxplot:", etiqueta),
            horizontal = TRUE,
            col        = "lightblue")
}


## ---------------------------------------------------------------------------
## Utilidades
## ---------------------------------------------------------------------------

#' Descarga un archivo si no existe localmente.
#'
#' Útil para datasets externos. Si el archivo ya está bajado, no hace nada.
#' Crea el directorio destino si falta.
#'
#' Ejemplo:
#'   descargar_si_falta(
#'     "https://example.com/datos.csv",
#'     destino = "datos/externo.csv"
#'   )
descargar_si_falta <- function(url, destino) {
    if (file.exists(destino)) {
        message("Ya existe: ", destino)
    } else {
        dir.create(dirname(destino), showWarnings = FALSE, recursive = TRUE)
        utils::download.file(url, destino)
    }
    invisible(destino)
}
