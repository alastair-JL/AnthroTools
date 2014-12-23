\name{ConsensusPipeline}
\alias{ConsensusPipeline}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
ConsensusPipeline
}
\description{
The main function to use when trying to do consensus analysis on a selection of survey data. This function pretty much streamlines all the consensus analysis functions currently contained in the package into one function..
}
\usage{
ConsensusPipeline(SurveyResults, numQ)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SurveyResults}{
These are your survey results, written as a data frame. Rows are expected to represent participants, columns to represent individual questions. Each cell contains a particular individuals answer to a particular question- currently coded as a number (it is assumed things are somewhat multichoice)
}
  \item{numQ}{
Currently this is a single number representing the number of possible answers to each question (hence for a true/false question, 2, for a multi choice, four perhaps).
}
}
\details{
This method takes a set of survey results, along with a number representing the number of possible answers to each questions. It calculates the correlation between different peoples answers, corrects for random chance, estimate each individuals expertise and then determines the most likely answer for each question
}
\value{
This function returns a list with four components.
\item{Answers} {a list of the functions estimated answers for each question.}
\item{Competance}  {The estimated competance for each individual (the probability that they would KNOW the answer to some future question).} 
\item{TestScore} {The supposed test score of each inidividual, assuming the answer key determined by the method}
\item{Probs} {the probability that each answer is correct for a given question ASSUMING that the method has correctly determined each individuals competence. 
Given how Competence is calculated, (from the model) it may be reckless to take this "probability" too seriously.}
}
\references{
Culture as consensus: a Theory of Culture and Informant Accuracy
A. Kimball Romney, Susan C. Weller, William H. Batchelder.
American Anthropologist, New Series, Volume 88, No 2 (Jun 1986) pp 313-338
}
\author{
Alastair Jamieson-Lane
}
\note{
This function (and library) could probably use some additional features. If there are particular feature's you would like to see added, please email me, and I will see about adding.
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
ConsensusResult <- ConsensusPipeline(Survey,4)
View(ConsensusResult$Answers)
View(FakeData$Answers)

## The function is currently defined as
##function (SurveyResults, numQ) 
##{
##    M <- MakeDiscountedAgreementMatrix(SurveyResults, numQ)
##    Competance <- ComreySolve(M)
##    Probs <- BayesConsensus(SurveyResults, Competance, numQ)
##    AnsKey <- apply(Probs, 2, which.max)
##    names(Competance) <- rownames(SurveyResults)
##    TestScore <- rowMeans(SurveyResults == rep(1, nrow(SurveyResults)) \%*\% 
##        t(AnsKey))
##    ReturnThing <- list()
##    ReturnThing$Answers <- AnsKey
##    ReturnThing$Competance <- Competance
##    ReturnThing$TestScore <- TestScore
##    ReturnThing$Probs <- Probs
##    return(ReturnThing)
##  }
##}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
