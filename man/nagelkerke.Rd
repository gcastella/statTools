% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statTools_code_OTHER.R
\name{nagelkerke}
\alias{adjNagelkerke}
\alias{nagelkerke}
\title{Nagelkerke's pseudo r-squared}
\usage{
nagelkerke(model, rounding = getOption("rounding"))

adjNagelkerke(model, rounding = getOption("rounding"))
}
\arguments{
\item{model}{glm object.}

\item{rounding}{Number of decimal places for the roundings.}
}
\value{
Nagelkerke's r-squared or adjusted r-squared.
}
\description{
Calculating Nagelkerke's r-squared (or adjusted r-squared) coefficient from a model (glm object). 
Can be passed as functions to the argument FUN.model from \code{\link{bivarTable}}.
}
\section{Functions}{
\itemize{
\item \code{adjNagelkerke}: Adjusted Nagelkerke's pseudo r-squared.
}}
\examples{
m <- glm(I(Sepal.Width > 3) ~ Species + Sepal.Length, iris, family = binomial)
nagelkerke(m, rounding = 2)
adjNagelkerke(m, rounding = 2)

}
\seealso{
\code{\link{getPval}}, \code{\link{GoF}}, \code{\link{getORCI}}, \code{\link{getBetaSd}},
\code{\link{verticalgetORCI}}, \code{\link{verticalgetBetaSd}}, \code{\link{verticalgetPval}}.
}

