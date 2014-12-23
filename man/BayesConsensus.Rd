\name{BayesConsensus}
\alias{BayesConsensus}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
BayesConsensus
}
\description{
Given a list of each persons answer, an estimate of each persons competance, and knowledge of the number of answers avaliable (which... is somewhat inferable, but oh well) determine the probability that any given answer is correct for a particular question.
}
\usage{
BayesConsensus(AnswersGiven, Competancy, numAnswers, Prior = -1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{AnswersGiven}{
A data frame with rows representing individuals, and columns representing questions. Individual cells represent an individuals answer to a particular question.
}
  \item{Competancy}{
A list containing numbers between zero and 1 representing the probability that a given individuals KNOWS the answer to a question in advance.
}
  \item{numAnswers}{
The total number of answers avaliable in each question. It is presently assumed that all questions have the same number of answers (I will investigate changing this later)
}
  \item{Prior}{
Do you have a prior distribution of probabilities over you answers? For example, in advance do you believe "A" is twice as likely as "B"? If so, please enter this as a matrix now. If not, the computer will assume a uniform distribution.
}
}
\details{
}
\value{
A matrix, where columns represent questions, and rows represent possible answers. Each entry represents the probability that a particular answer is "correct" for a particular question, within the cultural consensus framework.
}
\references{
For details on how this fits into consensus modelling, see appendix A of:
Culture as consensus: a Theory of Culture and Informant Accuracy
A. Kimball Romney, Susan C. Weller, William H. Batchelder.
American Anthropologist, New Series, Volume 88, No 2 (Jun 1986) pp 313-338
}
\author{
Alastair
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
    Competance <- ComreySolve(M)
    Probs <- BayesConsensus(Survey, Competance, 4)


## The function is currently defined as
##function (AnswersGiven, Competancy, numAnswers, Prior = -1) 
##{
##   if (Prior == -1) {
##    Prior <- matrix(1/numAnswers, numAnswers, ncol(AnswersGiven))
##  }
##  if ( !all(abs(colSum(Prior) -1)<0.001) ){
##    stop('Your Prior for every question must add to 1.')    
##    }
##    
##  if ( ncol(Prior)!=ncol(AnswersGiven) || nrow(Prior)!=numAnswers){
##    stop('Your Prior matrix is wrong shape.')    
##  }  
##     
##    
##    Probability <- matrix(0, numAnswers, ncol(AnswersGiven))
##    colnames(Probability) <- colnames(AnswersGiven)
##    rownames(Probability) <- c(1:numAnswers)
##    WrongVect <- (1 - Competancy)/numAnswers
##    RightVect <- WrongVect + Competancy
##    for (QQQ in 1:ncol(AnswersGiven)) {
##        for (aaa in 1:numAnswers) {
##            Probability[aaa, QQQ] <- prod(ifelse(AnswersGiven[, 
##                QQQ] == aaa, RightVect, WrongVect))
##        }
##        Probability[, QQQ] <- Probability[, QQQ] * Prior[, QQQ]
##        Probability[, QQQ] <- Probability[, QQQ]/sum(Probability[, 
##            QQQ])
##    }
##    return(Probability)
##  }
##}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
