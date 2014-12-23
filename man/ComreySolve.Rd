\name{ComreySolve}
\alias{ComreySolve}
\alias{ComreyIterate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ComreySolve
}
\description{
Given a square matrix, such that the matrix is the outer product of a single vector with itself (Except for the diagonal, which is ignored), determine the vector that was originally used.
ComreySolve also has a helper function called ComreyIterate. There isn't really any reason why you would use Comrey iterate by itself.
}
\usage{
ComreySolve(M, precision = 0.005)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{M}{
A square matrix.
}
  \item{precision}{
The level of error at which the process will stop iterating and return an answer. Error is measured as "change in output vector between two iteration steps."
}
}
\details{

}
\value{
A single vector of numbers representing the vector that was originally used to create M.
}
\references{
The Minimum Residual method of Factor Analysis.
Comrey, A.L.
Pychological reports, (1962) 11:15-18
}
\author{
Alastiar Jamiesone-Lane
}
\note{
NOTE TO SELF: I really need to go back and re-read Comrey's paper to make sure I have done this right!
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

x<- 1:5
mat<- x \%*\% t(x)
y<- ComreySolve(mat)

## The function is currently defined as
##function (M, precision = 0.005) 
##{
##    A1 <- apply(M, 2, max)
##    error <- 2
##    timeOut <- 300
##    while (timeOut > 0 && error > precision) {
##        A2 <- ComreyIterate(M, A1)
##        A3 <- ComreyIterate(M, A2)
##        A4 <- (A2 + A3)/2
##        A5 <- ComreyIterate(M, A4)
##        A1 <- (A4 + A5)/2
##        timeOut <- timeOut - 1
##        error <- max(abs(A5 - A4))
##    }
##    return(A5)
##  }
##}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
