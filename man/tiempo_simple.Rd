% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/basicfuncts.R
\name{tiempo_simple}
\alias{tiempo_simple}
\title{Cálculo del tiempo}
\usage{
tiempo_simple(monto, periodicidad, capital, interes)
}
\arguments{
\item{monto}{Monto esperado a recibir al final del plazo}

\item{periodicidad}{Amplitud de las unidades de tiempo. Ej: 1 equivale a un año, 2 a un semestre, 12 a un mes, 365 a un día.}

\item{capital}{Valor de la inversión inicial}

\item{interes}{Tasa de interés simple a la que se dejará la inversión inicial}
}
\value{

}
\description{
Cálculo del tiempo
}
\details{
Función para calcular el tiempo necesario para obtener un monto dado a un cierto tiempo, con una tasa de interés y capitalización dadas, considerando un capital inicial dado. Regresa una cadena de texto explicando el tiempo necesario.
}
\examples{
tiempo_simple(150,12,200,12)
}
