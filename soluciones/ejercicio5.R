## transformar_columna(dataframe, nombre_columna, umbral){ 
##   # ... 
##   return(dataframe)
## }

transformar_columna <- function(dataframe, nombre_columna, umbral){
    aux = dataframe[, nombre_columna] > umbral
    dataframe[aux, nombre_columna] = "alto"
    dataframe[!aux, nombre_columna] = "bajo"
    return(dataframe)
}
