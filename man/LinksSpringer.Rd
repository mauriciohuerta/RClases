\name{LinksSpringer}
\alias{LinksSpringer}
%\docType{dataset}
\title{
LinksSpringer dataset
}
\description{
Dato con las URL's directas para la descarga de los textos liberados por Springer durante la contingencia del COVID-19.
}
\usage{data("LinksSpringer")}
\format{
  Base de datos con 407 observaciones con las siguientes variables.
  \describe{
    \item{\code{url}}{Character. Indica el link directo de descarga.}
    \item{\code{name}}{Character. Indica el nombre del archivo generado por el link.}
  }
}
\source{
Noticia original \url{https://www.springernature.com/gp/librarians/news-events/all-news-articles/industry-news-initiatives/free-access-to-textbooks-for-institutions-affected-by-coronaviru/17855960}
}
\examples{
data(LinksSpringer)
}
\keyword{datasets}
