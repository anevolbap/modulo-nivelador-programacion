buscar_una_palabra <- function(palabra, lista_de_palabras){
  si_esta_la_palabra = palabra %in% lista_de_palabras
  return(si_esta_la_palabra)
}
