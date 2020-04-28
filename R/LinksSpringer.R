DescargarLibros <- function(carpeta){
  folder <- getwd()
  setwd(carpeta)
  data <- RClases::LinksSpringer
  for(i in 1:nrow(data)){
    try(download.file(as.character(data[i,1]), destfile = as.character(data[i,2])))
  }
  setwd(folder)
}
