## Polinoio Interpolador de Lagrange

## El objetivo es implementar el polinomio interpolador de Lagrange.
## 1) Armá una función que calcule solamente el j-ésimo polinomio de Lagrange ele_j.
## Debe recibir
##
## x_train: vector con las coordenadas x de los puntos a interpolar. 
## idx: índice vector con las coordenadas x de los puntos a interpolar. 
##
## y devolver ele_j evaluado en x (un número). 
##
## Funciones útiles: sum, prod, 
##
## 2) Creá una función que reciba:
## 
## x_train: vector con las coordenadas x de los puntos a interpolar. 
## y_train: vector con las coordenadas y de los puntos a interpolar.
##
## y devuelva, como función, el polinomio interpolador de Lagrange.
## 
## Sugerencia: usá la función eval_pol_lagrange() del archivo helpers_clase1.R.
##
## 3) Escriba una función que reciba x_train, y_train y grafique su polinomio
## interpolador. Agrege título y etiquetas a los ejes.
##
## Funciones útiles: plot, seq
##

pol_lagrange = function(x, x_train, idx) {
    ## Polinomio idx-ésimo de Lagrange calculado para x_train evaluado en x.
    ##
    ## Input
    ##
    ## x: valor de x a evaluar.
    ## x_train: vetor de xs a interpolar.
    ## idx: índice del polinomio de Lagrange.
    ##
    ## Output
    ##
    ## res: el idx-ésimo polinomio de Lagrange evaluado en x.
    
    num = prod(x - x_train[-idx])
    den = prod(x_train[idx] - x_train[-idx])
    res =  num / den
    return(res)
    }

eval_pol_lagrange = function(x, x_train, pol_lagrange) {
    ## Evalua todos los polinomios de Lagrange calculados para x_train usando los
    ## polinomios generados por build_plot_lagrange()
    ##
    ## Input
    ##
    ## x: punto donde quiero evaluar.
    ## x_train: vector de xs a interpolar.
2    ## build_plot_lagrange: función que genera el j-ésimo polinomio
    ##                      de Lagrange calcularo para x_train (como función).
    ##
    ## Output
    ##
    ## res: vector de las evaluaciones en x de los polinomios de Lagrange.
    res = c()
    for (iter in 1:length(x_train)) {
        pol = pol_lagrange(x, x_train, iter)
        res = c(res, pol)
    }
    return(res)
}

interpolador = function(x_train, y_train) {
    f = function(x) sum(y_train * eval_pol_lagrange(x, x_train, pol_lagrange))
    return(f)
}

graficar_interpolador = function(x_train, y_train){
    ## Datos los puntos (x_train, y_train), grafica su polinomio interpolador de
    ## Lagrange.
    xs = seq(min(x_train), max(x_train), length = 1e2)
    ys = Map(interpolador(x_train, y_train), xs)
    plot(xs,  # coordenadas x del plot
         ys,  # coordenadas x del plot
         col = "skyblue3",  # color
         type = "l",  # línea en lugar de puntos
         lwd = 4,     # grosor del trazo
         xlab = "x",  # etiqueta del eje x 
         ylab = "y"   # etiqueta del eje y)
         points(x_train, y_train, col = "orange",
                pch = 16, # tipo de puntito
                cex = 2  # factor para aumentar el tamaño del puntito
                )  
}
