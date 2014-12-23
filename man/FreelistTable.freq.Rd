\name{FreelistTable.freq}
\alias{FreelistTable.freq}
\alias{FreelistTable.MaxSal}
\alias{FreelistTable.Present}
\alias{FreelistTable.SumSal}

\title{
FreelistTable.*
}
\description{
This is a collection of helper functions for the function "FreelistTable", that determine the relationship between a particular subject, and a particular code. For example, FreelistTable.freq counts how often an individual has responded with the specified Code.
They are not intended for individual use.
}
\usage{
FreelistTable.freq(mydata, CODE = "CODE", Salience = "Salience", Subj = "Subj", subjNum, CODEnum)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mydata}{
Your data, as a data frame, with rows for each response gathered. For each response, you are expected to have the CODE, salience and Subj.
}
  \item{CODE}{
The Column of "mydata" that the CODE of the response can be found in.
}
  \item{Salience}{
The column of "mydata" that the calculated Salience is to be found in. Note that this will have been determined earlier using "CalculateSalience"
}
  \item{Subj}{
The column of "mydata" in which the subject number can be found.
}
  \item{subjNum}{
The subject number of interest.
}
  \item{CODEnum}{
The code you desire information about.
}
}
\details{

}
\value{
A number, representing either:
The presence/absence of the given code in the individuals responses (if "presence" was used).
The total number of times the code was given in response (for "freq")
The total Salience of a given code (summed over all responses with that code)
The Maximum Salience (effectively the Salience levels when the Code was first used.)
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Alastair Jamiesone-Lane
}
\note{
If you are using any of these functions directly yourself, something has probably gone wrong.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

test<- data(FruitList)
FreelistTable.freq(test,subjNum=7,CODEnum="apple") 

}

\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
