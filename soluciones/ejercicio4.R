## sorteo_valido = function(n_personas){
##   # es_valido = ...  # logical
##   return(es_valido)
## }

sorteo_valido = function(n_personas){
  es_valido = !any(sample(n_personas) == 1:n_personas)
  return(es_valido)
}

