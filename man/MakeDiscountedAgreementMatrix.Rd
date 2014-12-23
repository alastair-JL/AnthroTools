\name{MakeDiscountedAgreementMatrix}
\alias{MakeDiscountedAgreementMatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
MakeDiscountedAgreementMatrix
}
\description{
Given a bunch of answers to a survey, this function makes a matrix representing what fraction of the time each person agreed with each other person, and then discounts this value, based on how often you would expect this to happen by chance. 
}
\usage{
MakeDiscountedAgreementMatrix(SurveyResults, numAns)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SurveyResults}{
A matrix containing the answers to a bunch of survey questions. Each row represents a particular individual, each column represents a particular question.
}
  \item{numAns}{
This is a number, and represents the total number of answers avaliable for each question. Currently it is assumed that all questions have the same number of answers. If I get enough requests, I'll look into the programming and mathematics required to vectorise this.
}
}
\details{

}
\value{
A matrix, where each entry represents the probability that two individuals will both know the answer to some randomly selected question.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Alastair Jamieson-Lane
}
\note{
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
    M <- MakeDiscountedAgreementMatrix(Survey, 4)
    
    
## The function is currently defined as
##function (SurveyResults, numAns) 
##{
##    ReturnVal = matrix(0, nrow(SurveyResults), nrow(SurveyResults))
##    for (iii in 1:nrow(ReturnVal)) {
##        for (jjj in 1:ncol(ReturnVal)) {
##            ReturnVal[iii, jjj] = sum(SurveyResults[iii, ] == 
##                SurveyResults[jjj, ])/ncol(SurveyResults)
##        }
##    }
##    ReturnVal <- (ReturnVal * numAns - 1)/(numAns - 1)
##    diag(ReturnVal) <- 0
##    return(ReturnVal)
##  }
##}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
