\name{GenerateConsensusData}
\alias{GenerateConsensusData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
GenerateConsensusData
}
\description{
This function simulates a simple survey, and is useful for testing the "bayesian consensus" part of the <PackageName> package. It returns three objects- the most important being a hypothetical survey.
}
\usage{
GenerateConsensusData(numPeople, numQuestions, numAns)
}
\arguments{
  \item{numPeople}{
The number of "people" to be "surveyed".
}
  \item{numQuestions}{
The number of "questions" in your "survey".
}
  \item{numAns}{
The number of possible answers on each question. It is assumed that all questions have the same number of answers. 
}
}
\details{
The survey assumes that each question has N possible answers and one correct answer. Each Surveyee has a well defined competance (picked at random) which does not vary from question to question. Competance determines the probability that they KNOW the answer. 
Each question will also have a randomly assigned "correct" answer. 
For each question, if the survey recipient know the the correct answer (with probability equal to their competance) they will pick that answer. Otherwise they will pick at random.
This procedure is done for every "survey recipient" and every question, until a full batch of survey data is produced.

The purpose of this function is to allow false data to be easily created such that a given set of real data can be compared statistically to what would be expected assuming all the assumptions of the model (see paper in references) are true.

}
\value{
This function will return a list containing three items.
\item{Survey}{This is a matrix, containing each survey reciepiants answers to every quetion. This set of survey results gives good input for Consensus pipeline [MAKE LINK?].}
\item{Answers}{This gives a single row data frame containing the "Correct" answers to each survey question.}
\item{Competance}{This gives a list of the alledged skill level of each survey participant in your simulated survey. Please not that for small numbers of questions, you can expect a significant difference between this "true" competance measure and an individuals number of correct answers.}
}
\references{
Culture as Consensus: A theory of Culture and Informant Accuracy
A.Kimball Romnet, Susan C. Weller, William H. Batchelder
American Anthropologist, New Series, Vol 88, N0. 2 (Jun 1986) pp.313-338
}
\author{
Alastair Jamieson-Lane
}
\note{
HELP FILE STILL IN PROGRESS
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## NOTE: I feel like there's something I can/should do to make it so that this code IS EXECUTED in the 

FakeData<- GenerateConsensusData(5,5,3)
FakeSurvey<-FakeData$Survey
Results<- ConsensusPipeline(FakeSurvey,3)
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--  or do  help(data=index)  for the standard data sets.
%
%## The function is currently defined as
%function (numPeople, numQuestions, numAns) 
%{
%    PersonSkills <- runif(numPeople, 0, 1)
%    SurveyData <- data.frame()
%    CorrectAns <- rep(1, numPeople) \%*\% t(sample(1:numAns, numQuestions, 
%        replace = T))
%    GuessAns <- replicate(numQuestions, sample(1:numAns, numPeople, 
%        replace = T))
%    Knowledge <- replicate(numQuestions, (PersonSkills > runif(numPeople, 
%        0, 1)))
%    SurveyData <- ifelse(Knowledge, CorrectAns, GuessAns)
%    colnames(SurveyData) <- paste("Question", c(1:numQuestions))
%    rownames(SurveyData) <- paste("Person", c(1:numPeople))
%    names(PersonSkills) <- rownames(SurveyData)
%    CorrectAns <- CorrectAns[1, ]
%    names(CorrectAns) <- colnames(SurveyData)
%    ReturnVal <- list()
%    ReturnVal$Survey <- SurveyData
%    ReturnVal$Answers <- CorrectAns
%    ReturnVal$Competance <- PersonSkills
%return(ReturnVal)
%  }

} 

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
