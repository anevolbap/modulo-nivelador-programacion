## Clase 1 - Ejercicio 5
	
c1e5 = function(N, n, nrep){
	mean(replicate(nrep, any(duplicated(sample(N, n, replace=TRUE)))))
}
