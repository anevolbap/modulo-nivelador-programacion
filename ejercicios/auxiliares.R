## auxiliares.R
##
## Funciones auxiliares custom para los ejercicios del curso. Cada una
## reemplaza un bloque de codigo que seria largo o tedioso para escribir
## en el contexto de un ejercicio (cargar archivos, generar datos sinteticos,
## graficos elaborados, benchmarks).
##
## Cargar con: source("ejercicios/auxiliares.R")
##
## Cada funcion tiene una docstring con su firma y un ejemplo. El alumno
## puede leer el codigo si quiere entender como esta hecha cada una.


## ---------------------------------------------------------------------------
## Carga de datasets
## ---------------------------------------------------------------------------

#' Carga datos/bikeshare.csv con tipos correctos.
#'
#' bikeshare.csv proviene del paquete ISLR2 y registra el uso por hora del
#' sistema de bicicletas de Washington DC durante 2011 y 2012. El archivo
#' esta separado por espacios (no por comas), por lo que esta funcion
#' envuelve read.table con los argumentos correctos y convierte algunas
#' columnas a factor con etiquetas legibles.
#'
#' Devuelve un data.frame con columnas:
#'   season (factor: invierno, primavera, verano, otono)
#'   mnth (chr), day (int), hr (int)
#'   holiday (lgl), weekday (factor: dom..sab), workingday (lgl)
#'   weathersit (chr), temp, atemp, hum, windspeed (num normalizadas)
#'   casual, registered, bikers (counts).
#'
#' Ejemplo:
#'   bicis <- cargar_bikeshare()
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
## Generadores de datos sinteticos
## ---------------------------------------------------------------------------

#' Genera una lista de pacientes ficticios con campos heterogeneos.
#'
#' Sirve para ejercicios sobre listas y data.frame sin tener que tipear
#' los datos a mano. Usa una semilla fija para reproducibilidad.
#'
#' Cada paciente tiene: id (integer), edad (double), sintomas (vector chr),
#' presion_sistolica (double), tiene_diagnostico (logical).
#'
#' Ejemplo:
#'   pacientes <- generar_pacientes(5)
#'   pacientes[[1]]$edad
generar_pacientes <- function(n, semilla = 42) {
    set.seed(semilla)
    sintomas_posibles <- c("tos", "fiebre", "fatiga", "cefalea",
                           "dolor muscular", "nauseas", "vomitos", "anosmia")
    lapply(seq_len(n), function(i) {
        list(
            id                = as.integer(1000L + i),
            edad              = round(rnorm(1, mean = 50, sd = 15), 1),
            sintomas          = sample(sintomas_posibles, sample(0:3, 1)),
            presion_sistolica = round(rnorm(1, mean = 130, sd = 20)),
            tiene_diagnostico = sample(c(TRUE, FALSE), 1)
        )
    })
}

#' Genera una muestra de una distribucion bimodal.
#'
#' Mezcla de dos normales con probabilidad p y 1-p. Sirve para practicar
#' eleccion de bins en histogramas, donde una sola normal se ve trivial.
#'
#' Ejemplo:
#'   x <- generar_datos_bimodales(500)
#'   hist(x, breaks = 30)
generar_datos_bimodales <- function(n, p = 0.5, mu1 = -2, mu2 = 2,
                                    sd1 = 1, sd2 = 1, semilla = 42) {
    set.seed(semilla)
    componente <- sample(c(1, 2), n, replace = TRUE, prob = c(p, 1 - p))
    ifelse(componente == 1,
           rnorm(n, mean = mu1, sd = sd1),
           rnorm(n, mean = mu2, sd = sd2))
}


## ---------------------------------------------------------------------------
## Benchmark
## ---------------------------------------------------------------------------

#' Mide el tiempo de ejecucion de una funcion sobre la misma entrada.
#'
#' Repite la llamada n_rep veces y devuelve media y desvio estandar del
#' tiempo en segundos. Util para comparar alternativas (for vs vectorizado,
#' por ejemplo).
#'
#' Ejemplo:
#'   benchmark_funcion(function() sum(1:1e6))
#'   benchmark_funcion(function() {
#'     s <- 0
#'     for (i in 1:1e6) s <- s + i
#'     s
#'   })
benchmark_funcion <- function(fn, ..., n_rep = 10) {
    tiempos <- numeric(n_rep)
    for (i in seq_len(n_rep)) {
        t <- system.time(fn(...))
        tiempos[i] <- t["elapsed"]
    }
    list(media = mean(tiempos),
         sd    = sd(tiempos),
         n_rep = n_rep,
         crudos = tiempos)
}


## ---------------------------------------------------------------------------
## Graficos elaborados
## ---------------------------------------------------------------------------

#' Grafica la distribucion de un vector bootstrap con su intervalo del 95%.
#'
#' Histograma + linea roja en la media + lineas azules punteadas en los
#' percentiles 2.5% y 97.5%. Devuelve (de forma invisible) los extremos del IC.
#'
#' Ejemplo:
#'   medias <- replicate(1000, mean(sample(airquality$Wind, replace = TRUE)))
#'   graficar_bootstrap(medias, "media bootstrap del viento")
graficar_bootstrap <- function(boot, etiqueta = "estadistico bootstrap") {
    ic <- quantile(boot, c(0.025, 0.975), na.rm = TRUE)
    hist(boot, breaks = 30,
         main = etiqueta, xlab = etiqueta,
         col = "gray85", border = "white")
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

#' Histograma + boxplot lado a lado para un vector numerico.
#'
#' Util para mirar de un vistazo distribucion y outliers al mismo tiempo.
#' Restablece el layout grafico al salir.
#'
#' Ejemplo:
#'   graficar_hist_box(airquality$Wind, "viento (mph)")
graficar_hist_box <- function(x, etiqueta = "") {
    par_anterior <- par(mfrow = c(1, 2))
    on.exit(par(par_anterior))
    hist(x, main = paste("Histograma:", etiqueta),
         xlab = etiqueta, col = "gray85", border = "white")
    boxplot(x, main = paste("Boxplot:", etiqueta),
            horizontal = TRUE, col = "lightblue")
}

#' Barplot con barras de error (un desvio estandar de la media).
#'
#' Recibe un vector numerico x y un vector grupo de la misma longitud.
#' Calcula media y error estandar (sd / sqrt(n)) por grupo y dibuja un
#' barplot con barras de error verticales.
#'
#' Ejemplo:
#'   barplot_con_error(airquality$Ozone, airquality$Month,
#'                     etiqueta_x = "mes", etiqueta_y = "ozono (ppb)")
barplot_con_error <- function(x, grupo,
                              etiqueta_x = "grupo",
                              etiqueta_y = "valor",
                              titulo     = "") {
    resumen <- aggregate(x, by = list(grupo = grupo),
                         FUN = function(v) {
                             c(media = mean(v, na.rm = TRUE),
                               se    = sd(v, na.rm = TRUE) /
                                       sqrt(sum(!is.na(v))))
                         })
    medias  <- resumen$x[, "media"]
    errores <- resumen$x[, "se"]
    centros <- barplot(medias, names.arg = resumen$grupo,
                       ylim = c(0, max(medias + errores) * 1.1),
                       xlab = etiqueta_x, ylab = etiqueta_y,
                       main = titulo, col = "lightblue")
    arrows(x0 = centros, y0 = medias - errores,
           x1 = centros, y1 = medias + errores,
           code = 3, angle = 90, length = 0.05, lwd = 1.5)
    invisible(resumen)
}

#' Heatmap a partir de un cruce de dos variables categoricas y una numerica.
#'
#' Calcula el agregado por cada combinacion (fila, columna), arma una matriz
#' y la dibuja con image() y la paleta viridis.
#'
#' Ejemplo:
#'   bicis <- cargar_bikeshare()
#'   heatmap_grupo(bicis$bikers, bicis$weekday, bicis$hr,
#'                 etiqueta_x = "hora", etiqueta_y = "dia")
heatmap_grupo <- function(x, fila, columna, agg = mean,
                          etiqueta_x = "columna",
                          etiqueta_y = "fila",
                          titulo     = "") {
    tabla <- aggregate(x, by = list(fila = fila, columna = columna),
                       FUN = agg, na.rm = TRUE)
    matriz <- xtabs(x ~ fila + columna, data = tabla)
    image(t(matriz),
          axes = FALSE,
          xlab = etiqueta_x, ylab = etiqueta_y, main = titulo,
          col  = hcl.colors(20, "viridis"))
    axis(1,
         at     = seq(0, 1, length.out = ncol(matriz)),
         labels = colnames(matriz), las = 2)
    axis(2,
         at     = seq(0, 1, length.out = nrow(matriz)),
         labels = rownames(matriz), las = 1)
    invisible(matriz)
}


## ---------------------------------------------------------------------------
## Bootstrap especializado
## ---------------------------------------------------------------------------

#' Bootstrap del coeficiente de una variable en un modelo lineal.
#'
#' En cada repeticion: muestrea filas con reemplazo, reajusta el modelo
#' y extrae el coeficiente de la variable indicada. Devuelve el vector
#' de coeficientes bootstrap.
#'
#' Ejemplo:
#'   bicis <- cargar_bikeshare()
#'   coef_boot <- bootstrap_coef(bikers ~ temp, data = bicis,
#'                               variable = "temp", n_rep = 500)
#'   quantile(coef_boot, c(0.025, 0.975))
bootstrap_coef <- function(formula, data, variable, n_rep = 500) {
    replicate(n_rep, {
        idx <- sample(seq_len(nrow(data)), replace = TRUE)
        mod <- lm(formula, data = data[idx, ])
        coef(mod)[variable]
    })
}


## ---------------------------------------------------------------------------
## Utilidades
## ---------------------------------------------------------------------------

#' Descarga un archivo si no existe localmente.
#'
#' Util para datasets externos. Crea el directorio destino si falta.
#'
#' Ejemplo:
#'   descargar_si_falta("https://example.com/datos.csv",
#'                      destino = "datos/externo.csv")
descargar_si_falta <- function(url, destino) {
    if (file.exists(destino)) {
        message("Ya existe: ", destino)
    } else {
        dir.create(dirname(destino), showWarnings = FALSE, recursive = TRUE)
        utils::download.file(url, destino)
    }
    invisible(destino)
}
