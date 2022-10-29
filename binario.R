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
    
biseccion_recursiva = function(fun, intervalo, tol = 1e-4){
    ## Punto medio del intervalo
    medio = mean(intervalo)
    ## Evaluo la función en los tres puntos
    fmedio = fun(medio)
    fizq = fun(intervalo[1])
    fder = fun(intervalo[2])
    ## Defino el siguiente intervalo
    cual = which(fmedio * c(fizq, fder) < 0)
    intervalo_nuevo = sort(c(intervalo[cual], medio))
    ## Chequeo la tolerancia
    flag = (fun(intervalo_nuevo[2]) - fun(intervalo_nuevo[1])) < tol
    ## Llamo a la misma función!
    if (!flag) {
        ret = biseccion_recursiva(fun, intervalo_nuevo)
    }
    # Cuando termina duvuelvo el último resultado
    return(ret)
}
