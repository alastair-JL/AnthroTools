\name{MakeAgreementMatrix}
\alias{MakeAgreementMatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
MakeAgreementMatrix
}
\description{
Given a bunch of answers to a survey, this function makes a matrix representing what fraction of the time each person agreed with each other person.
}
\usage{
MakeAgreementMatrix(SurveyResults)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SurveyResults}{
A matrix containing the answers to a bunch of survey questions. Each row represents a particular individual, each column represents a particular question.
}
}

\value{
A matrix M, where each entry represents the percentage of the time that individual i agreed with individual j.
}
\author{
Alastair Jamieson-Lane
}
\note{
This is probably already implemented elsewhere in R, and may be identical to a correlation matrix or something. I feel slightly silly putting it here to be honest. Oh well.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

FakeData<- GenerateConsensusData(8,8,4)
    Survey <- FakeData$Survey
    Ma <- MakeAgreementMatrix(Survey)    

## The function is currently defined as
##function (SurveyResults) 
##{
##    ReturnVal = matrix(0, nrow(SurveyResults), nrow(SurveyResults))
##    for (iii in 1:nrow(ReturnVal)) {
##        for (jjj in 1:ncol(ReturnVal)) {
##            ReturnVal[iii, jjj] = sum(SurveyResults[iii, ] == 
##                SurveyResults[jjj, ])/ncol(SurveyResults)
##        }
##    }
##    return(ReturnVal)
##  }
##}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
