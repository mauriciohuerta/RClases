MultiGsub <- function(pattern, replacement, x){
  if(length(pattern)==0){
    stop("El argumento pattern debe contener al menos un elemento")
  } else if(length(replacement)==0){
    stop("El argumento replacement debe contener al menos un elemento")
  } else if(length(x)==0){
    stop("El argumento x esta vacio")
  }
  if(length(pattern)!=length(replacement)){
    stop("La longitud de los argumentos pattern y replacement deben ser iguales")
  }
  if(length(pattern)!=length(unique(pattern))){
    stop("El argumento pattern no puede contener valores repetidos")
  }
  n <- length(x)
  cambio <- NULL
  id.notmatching <- as.numeric()
  for(i in 1:n){
    id <- which(pattern %in% x[i])
    if(sum(id)==0){
      cambio.i <- x[i]
      id.notmatching <- c(id.notmatching, i)
      warning(paste("No se encontro ningun emparejamiento para el elemento",i,"del argumento x. Ningun cambio fue efectuado"))
    } else{
      cambio.i <- gsub(pattern[id], replacement[id], x[i])
    }
    cambio <- c(cambio, cambio.i)
    rm(cambio.i)
  }
  return(cambio)
}
