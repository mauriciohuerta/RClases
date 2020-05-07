\name{TablaFrecuencias}
\alias{TablaFrecuencias}
\title{TablaFrecuencias}
\usage{
TablaFrecuencias(datos, intervalos = NULL, ni = TRUE, Ni = TRUE, fi = TRUE, Fi = TRUE,
                 n.intervalos = "sturges", round = NULL)
}
\arguments{
  \item{datos}{Un vector numerico o de caracteres.}
  \item{intervalos}{Valor logico [TRUE/FALSE]. El valor TRUE indica que se construiran intervalos y marcas de clase. El valor FALSE señala que solo se trabajara con las clases. El valor NULL por defecto hace que la funcion determine automaticamente la generacion o no de intervalos dependiendo del tipo de variable. Nota: si el objeto datos es de tipo cadena de caracteres, solo puede tomar el argumento intervalos = FALSE.}
  \item{ni}{Valor logico [TRUE/FALSE]. Si el valor es TRUE se tabularan las frecuencias absolutas. Por defecto es TRUE.}
  \item{Ni}{Valor logico [TRUE/FALSE]. Si el valor es TRUE se tabularan las frecuencias absolutas acumuladas. Por defecto es TRUE.}
  \item{fi}{Valor logico [TRUE/FALSE]. Si el valor es TRUE se tabularan las frecuencias relativas. Por defecto es TRUE.}
  \item{Fi}{Valor logico [TRUE/FALSE]. Si el valor es TRUE se tabularan las frecuencias relativas acumuladas. Por defecto es TRUE.}
  \item{n.intervalos}{Un valor entero o un metodo. Establece el numero de intervalos personalizados que se desea construir o bien se puede señalar el metodo para construir intervalos. Las opciones de metodos son c("sturges"). Por defecto se tiene el valor "sturges".}
  \item{round}{Un valor entero. Indica cuantos digitos se quiere redondear los intervalos y marca de clase. El valor por defecto es NA y no aplica ningun redondeo. Si el objeto datos no es numerico esta opcion es despreciada.}
}
\description{
Una función que genera una tabla de frecuencias a partir de datos de tipo caracteres o numerico.
}
\examples{

# Ejemplo para variables cuantitativas
DatosCuanti <- rnorm(100)
TablaFrecuencias(DatosCuanti)
TablaFrecuencias(DatosCuanti, round = 2)
TablaFrecuencias(DatosCuanti, round = 2, n.intervalos = 4)

# Ejemplo para variables cualitativas
DatosCuali <- sample(c("a","b","c"), size = 10, replace = TRUE)
TablaFrecuencias(DatosCuali, ni = TRUE, Ni = TRUE, fi = TRUE, Fi = TRUE)
TablaFrecuencias(DatosCuali, ni = TRUE, Ni = FALSE, fi = TRUE, Fi = FALSE)
}
