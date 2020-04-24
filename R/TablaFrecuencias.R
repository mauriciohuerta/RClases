TablaFrecuencias <-function(datos, intervalos = NULL, ni = TRUE, Ni = TRUE, fi = TRUE, Fi = TRUE, n.intervalos = "sturges", round = NULL){
  if(!is.logical(intervalos) & !is.null(intervalos)){
    stop("El argumento 'intervalos' debe tener el valor 'TRUE' o 'FALSE'")
  }
  if(!is.logical(ni) & !is.null(ni)){
    stop("El argumento 'ni' debe tener el valor 'TRUE' o 'FALSE'")
  }
  if(!is.logical(Ni) & !is.null(Ni)){
    stop("El argumento 'Ni' debe tener el valor 'TRUE' o 'FALSE'")
  }
  if(!is.logical(fi) & !is.null(fi)){
    stop("El argumento 'fi' debe tener el valor 'TRUE' o 'FALSE'")
  }
  if(!is.logical(Fi) & !is.null(Fi)){
    stop("El argumento 'Fi' debe tener el valor 'TRUE' o 'FALSE'")
  }
  if(!is.numeric(datos) & !is.character(datos) & !is.data.frame(datos)){
    class <- class(datos)
    stop(paste("La clase del argumento 'datos' debe ser numérica o de carácteres.\n  La clase de 'datos' es ", class, "\n  Considere usar los comandos 'as.numeric(datos)' o 'as.character(datos)', según sea el caso",sep=""))
  }
  if(length(n.intervalos) != 1){
    stop("El argumento 'n.intervalos' debe contener sólo un elemento.")
  }
  if(is.numeric(n.intervalos)){
    if((n.intervalos - floor(n.intervalos)) !=0){
      stop("El argumento 'n.intervalos' debe ser entero.")
    }
  }
  if((n.intervalos!="sturges") & (!is.numeric(n.intervalos))){
    stop("El argumento 'n.intervalos' debe ser un número entero o indicar 'sturges'")
  }
  if((!is.numeric(round)) & (!is.null(round))){
    stop("El argumento 'round' debe ser un número entero")
  }
  if(is.numeric(n.intervalos)){
    if((n.intervalos - floor(n.intervalos)) !=0){
      stop("El argumento 'round' debe ser entero.")
    }
  }
  if(is.null(intervalos)){
    if(is.numeric(datos)){
      intervalos = TRUE
      message("- La clase de los datos es de tipo numérico. La variable será tratada automáticamente como cuantitativa y se calcularán los intervalos de clase.\n  - Si sus datos son cualitativos considere cambiar la clase del objeto a cadena de carácteres usando el comando 'as.character(datos)'.\n  - Si Ud desea evitar que se generen intervalos use el argumento 'intervalos = FALSE'.\n")
    }
  }
  if(is.null(intervalos)){
    if(is.character(datos)){
      intervalos = FALSE
      message("- La clase de los datos es de tipo cadena de carácteres. La variable será tratada automáticamente como cualitativa y se omitirán los intervalos de clase.\n  - Si sus datos son cuantitativos considere cambiar la clase del objeto a numérico usando el comando 'as.numeric(datos)'.\n")
    }
  }
  if(intervalos){
    n <- length(datos)
    r <- max(datos) - min(datos)
    if(n.intervalos == "sturges"){
      k <- floor((1 + 3.3*log10(n)))
    }
    if(n.intervalos != "sturges"){
      k <- n.intervalos
    }
    a <- r/k
    li <- min(datos) + ((0:(k-1))*a)
    ls <- min(datos) + ((1:(k))*a)
    mi <- ((li[1] + ls[1])/2) + ((0:(k-1))*a)
    if(!is.null(round)){
      li.tb <- formatC(li, digits = round, format = "f")
      ls.tb <- formatC(ls, digits = round, format = "f")
      mi <- formatC(mi, digits = round, format = "f")
    }
    if(is.null(round)){
      li.tb <- li
      ls.tb <- ls
    }
    limits <- paste(paste(rep("[", k),apply(data.frame(v1=li.tb, v2=ls.tb), MARGIN = 1, FUN = paste, collapse=";"), sep=""), c(rep("[", (k-1)),"]"), sep="")
    tabla <- data.frame(IntervaloClase = limits, mi = mi)
    breaks <- sort(as.numeric(unique(c(li.tb[-1], ls.tb[-k], min(datos), max(datos)))))
    fa <- data.frame(ni = hist(datos, breaks = breaks, plot = FALSE)$counts)
    aux <- data.frame(IntervaloClase = "Total", mi = "--")
  }
  if(!intervalos){
    n <- length(datos)
    tab <- as.data.frame(table(datos))
    tabla <- data.frame(Clase = tab[,1])
    k <- nrow(tabla)
    fa <- data.frame(ni = tab[,2])
    rm(tab)
    aux <- data.frame(Clase = "Total")
  }
  tabla <- rbind(tabla, aux)
  rm(aux)
  faa <- data.frame(Ni = cumsum(fa$ni))
  fr <- data.frame(fi = formatC(fa$ni/n, digits = 4, format = "f"))
  fra <- data.frame(Fi = formatC(faa$Ni/n, digits = 4, format = "f"))
  if(ni){
    fa <- rbind(fa, data.frame(ni = n))
    tabla <- cbind(tabla, fa)
  }
  if(Ni){
    faa <- rbind(faa, data.frame(Ni = "--"))
    tabla <- cbind(tabla, faa)
  }
  if(fi){
    fr <- rbind(fr, data.frame(fi = formatC(1, digits = 4, format = "f")))
    tabla <- cbind(tabla, fr)
  }
  if(Fi){
    fra <- rbind(fra, data.frame(Fi = "--"))
    tabla <- cbind(tabla, fra)
  }
  return(tabla)
}
