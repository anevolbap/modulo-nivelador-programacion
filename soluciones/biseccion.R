## Actividad - Bisección

biseccion_un_paso = function(fun, intervalo, tol = 1e-4){
    # Punto medio del intervalo
    medio = mean(intervalo)
    # Evaluo la función en los tres puntos
    fmedio = fun(medio)
    fizq = fun(intervalo[1])
    fder = fun(intervalo[2])
    # Defino el siguiente intervalo
    cual = which(fmedio * c(fizq, fder) < 0)
    intervalo_nuevo = sort(c(intervalo[cual], medio))
    ## Chequeo la tolerancia
    flag = (fun(intervalo_nuevo[2]) - fun(intervalo_nuevo[1])) < tol
    ## Devuelvo el
    return(list(fun = fun,
                intervalo = intervalo_nuevo,
                flag = flag))
}

iterar_funcion = function(fun_iterar, fun, intervalo_inicial, tol=1e-4) {
    intervalo = intervalo_inicial
    salida = FALSE
    while (!salida) {
        res = fun_iterar(fun, intervalo, tol)
        intervalo = res$intervalo
        salida = res$flag
    }
    return(res)
}
    
biseccion_recursiva = function(fun, intervalo, tol = 1e-4) {

  # calculo la condición para terminar
  flag = (intervalo[2]-intervalo[1])/2 < tol

  if (flag) {
    # si se cumple devuelvo lo que encontré
    ret = list(fun = fun, intervalo = intervalo, flag = flag)
    return(ret)
  } else { 
    # si no se cumple hago un paso más partimos el intervalo
    medio = (intervalo[1]+intervalo[2])/2
    f_medio = fun(medio)
    f_izq = fun(intervalo[1])
    f_der = fun(intervalo[2])
    if (((f_izq * f_medio) < 0) & ((f_der * f_medio) < 0)){
      intervalo_nuevo = c(intervalo[1], intervalo[2])
    } else if (((f_izq * f_medio) < 0) & ((f_der * f_medio) > 0)){
      intervalo_nuevo = c(intervalo[1], medio)
    } else if (((f_izq * f_medio) > 0) & ((f_der * f_medio) < 0)){
      intervalo_nuevo = c(medio, intervalo[2])
    } else {
      intervalo_nuevo = c(medio, medio)
    }
    # acá el paso propiamente recursivo, llamamos a la función de nuevo
    biseccion_recursiva(fun, intervalo_nuevo)
}




# 4. Modifique la función `iterar_funcion` para registrar todas las iteraciones # del método para hacer un gráfico de la trayectoria. 

iterar_funcion_track = function(fun_iterar, fun, intervalo_inicial, tol=1e-4) {
    intervalo = intervalo_inicial
    salida = FALSE
    trace = c()
    while (!salida) {
        res = fun_iterar(fun, intervalo, tol)
        intervalo = res$intervalo
        salida = res$flag
        trace = c(trace, c(mean(intervalo), fun(mean(intervalo))))
    }
    return(trace)
}

fun_verdad = function(x) {1+x-x^2+x^5}

trace = iterar_funcion_track(biseccion_un_paso, fun_verdad, c(-5,10), tol = 1e-15)

puntos = split(trace, 1:2)
xs = puntos$`1`
ys = puntos$`2`
plot(xs, ys, col=1:length(xs), pch=19)
curve(fun_verdad(x), min(xs), max(xs), add = TRUE)

library(ggplot2)
## En ggplot
p <- ggplot(
  data.frame(x=xs, y=ys, iter = 1:length(xs)), 
  aes(x = xs, y = ys)
  ) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
    labs(x = "x", y = "f(x)")
p

p_anim = p + transition_time(iter) +
    labs(title = "Iteración: {iter}")


