make_test_files = function(n,
                           file_main_name = "test_file",
                           dir_name = "test_dir",
                           extensions = c("png", "txt", "pdf", "csv"),
                           seed = 42) {
    ## La función crea 'n' archivos en el directorio 'test_dir' con extensiones elegidas al azar.

    ## Fijamos la semilla por reproducibilidad
    set.seed = 42
    
    ## Si no existe, creo el directorio
    if (!dir.exists(dir_name)) {dir.create(dir_name)}

    ## Repito 'n' veces
    for (iter in 1:n){
        ## A cada nombre de agrego un número
        file_name = paste(file_main_name, iter, sep="_")
        ## Le agrego una extensión al azar
        file_name_full = paste(file_name, sample(extensions, 1), sep = ".")
        ## Creo el archivo
        file.create(paste(dir_name, file_name_full, sep = "/"))
        ## Imprimo un mensaje 
        msg = paste("Creando el archivo", file_name_full)
        message(msg)        
    }
}

get_extension = function(tupla){
    ## A partir de un vector (nombre, extension) devuelve la extensión.

    ## Extraigo el segundo elemento del vector.
    extension = tupla[2]

    ## Retornamos la extensión extraída.
    return(extension)
}

get_unique_extensions = function(dir_name){
    ## Listamos todos los archivos del directorio 'dir_name'
    list_files = list.files(dir_name)

    ## Separamos el nombre de la extensión
    tuples = strsplit(list_files, split="\\.")

    ## Aplicamos nuestra función a todas las tuplas
    exts = sapply(tuples, get_extension)

    ## Nos quedamos con las extensiones únicas
    exts_uniques = unique(exts)
    
    return(exts_uniques)
}

move_one_file_by_extension = function(file_name) {
    ## Mueve un archivo a una carpeta según su extensión.
    target_folder = strsplit(basename(file_name), split="\\.")[[1]][2]
    if (!dir.exists(paste(dirname(file_name), target_folder, sep = "/"))) {
        dir.create(paste(dirname(file_name), target_folder, sep = "/"))
    }
    ## Movemos los archivos
    file_origin = paste(".", file_name, sep = "/")
    file_target = paste(".", dirname(file_name), target_folder, basename(file_name), sep = "/")
    out = tryCatch({
        file.rename(file_origin, file_target)
        ## Imprimo un mensaje 
        msg = paste("Se copió el archivo", file_origin, "a la ruta", file_target)
        message(msg)
    },
    error=function(e){
        message(paste("Pasaron cosas:", e))
    }  
    )
}

tidy_folder = function(dir_name) {
    ## Organiza los archivos de la carpeta 'dir_name' en subcarpetas según su
    ## extensión

    ## Buscamos las extensiones únicas
    exts = get_unique_extensions(dir_name)

    ## Creamos un directorio por cada extensión
    lapply(paste(dir_name, paste0(exts, "/"), sep = "/"), dir.create)

    ## Listamos todos los archivos del directorio
    list_files = list.files(dir_name, pattern="\\.", full.name=TRUE)

    ## Los copiamos 
    lapply(list_files, move_one_file_by_extension)
}
