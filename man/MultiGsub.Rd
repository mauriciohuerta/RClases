\name{MultiGsub}
\alias{MultiGsub}
\title{MultiGsub}
\usage{
MultiGsub(pattern, replacement, x)
}
\arguments{
  \item{pattern}{Un vector de elementos a ser reemplazados.}
  \item{replacement}{Un vector de elementos de la misma longitud del argumento pattern. Representa los valores que reemplazarán a los valores de pattern.}
  \item{x}{Un vector de elementos en los cuales se aplicarán los cambios.}
}
\description{
Funcion que permite multiples cambios generalizando la funcion \code{gsub}
}
\seealso{\code{\link[base]{gsub}}}
\examples{
MultiGsub(1:3, c("a","b","c"), x=1:3)
}
