\name{FruitList}
\alias{FruitList}
\docType{data}
\title{
FruitList
}
\description{
A ficticious dataset, in which we have asked people to list fruit. Generated using GenerateFakeFreelistData, and meant as an example for those who wish to know the format that CalculateSalience and other functions will be expecting.

NOTE TO BEN: If you want, we can put some real data here as an example. Your call.
}
\usage{data(FruitList)}
\format{
  A data frame with 58 observations on the following 3 variables.
  \describe{
    \item{\code{Subj}}{a numeric vector}
    \item{\code{Order}}{a numeric vector}
    \item{\code{CODE}}{a factor with levels \code{apple} \code{bannana} \code{Lemon} \code{orange} \code{peach} \code{pear} \code{plum} \code{strawberry}}
  }
}
\examples{
data(FruitList)
}
\keyword{datasets}
