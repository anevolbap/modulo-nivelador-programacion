# Unicos

# 1. buscar_una_palabra devuelve TRUE o FALSE
buscar_una_palabra = function(palabra, lista_de_palabras){
  for (iter in seq(lista_de_palabras)){
    if (palabra == lista_de_palabras[[iter]]){
      return(TRUE)
    }
  }
  return(FALSE)
}

buscar_una_palabra = function(palabra, lista_de_palabras){
  for (p in lista_de_palabras){
    if (p == palabra){
      return(TRUE)
    }
  }
  return(FALSE)
}

test_buscar_una_palabra = function(){
  
  test_1 = list(palabra = "hola", 
                lista = list("hola", "qué", "tal"), 
                valor = TRUE)
  test_2 = list(palabra = "chau", 
                lista = list("hola", "qué", "tal"),
                valor = FALSE)
  
  out_1 = buscar_una_palabra(test_1$palabra, test_1$lista) == test_1$valor
  
  out_2 = buscar_una_palabra(test_2$palabra, test_2$lista) == test_2$valor
  
  return(out_1 & out_2)
  
}

borrar_duplicados = function(lista_de_palabras){
  unicos = lista_de_palabras[[1]]
  for (iter in 2:length(lista_de_palabras)){
    if (!buscar_una_palabra(lista_de_palabras[[iter]], unicos)) {
      unicos = append(unicos, lista_de_palabras[[iter]])
    }
  }
  return(unicos)
}

test_borrar_duplicados = function(){
  out_1 = (length(borrar_duplicados(c("Hola", "Hola", "Chau")) == 2))
  out_2 = (length(borrar_duplicados(c("No", "hay", "duplicados")) == 3))
  out_3 = (length(borrar_duplicados(c("Soloduplicados", "Soloduplicados", "Soloduplicados"))==1))
  return(out_1 & out_2 & out_3)
}

generar_lista = function(lista_de_palabras, repeticiones, seed=42){
  set.seed(seed)
  sample(rep(lista_de_palabras, repeticiones))
}

generar_listas(c("Hola", "que", "tal"), c(1,2,2))