#' GenerateConsensusData
#'
#' This function simulates a simple survey, and is useful for testing the "Bayesian consensus" part of the AnthroTools package. It returns three objects- the most important being a hypothetical survey.
#' 
#' @details The survey assumes that each question has N possible answers 
#' and one correct answer. Each survey has a well-defined competence (picked 
#' at random) which does not vary from question to question. Competence determines the probability that they KNOW the answer. Each question will also have a randomly assigned "correct" answer. For each question, if the survey recipient knows the correct answer (with probability equal to their competence) they will pick that answer. Otherwise they will pick at random. This procedure is done for every "survey recipient" and every question, until a full batch of survey data is produced.
#'
#'The purpose of this function is to allow false data to be easily created such that a given set of real data can be compared statistically to what would be expected assuming all the assumptions of the model (see paper in references) are true.
#' 
#' @return 
#' This function will return a list containing three items.
#' \item{Survey}{This is a matrix, containing each survey recipients answers to every quetion. This set of survey results gives good input for \code{\link{ConsensusPipeline}}.}
#' \item{Answers}{This gives a single row data frame containing the "Correct" answers to each survey question.}
#' \item{Competence}{This gives a list of the alleged skill level of each survey participant in your simulated survey. Please not that for small numbers of questions, you can expect a significant difference between this "true" competence measure and an individuals number of correct answers.}
#' @references 
#' Oravecz, Z., Vandekerckhove, J., & Batchelder, W. H. (2014). Bayesian Cultural Consensus Theory. Field Methods, 1525822X13520280. http://doi.org/10.1177/1525822X13520280 
#' @references 
#' Romney, A. K., Weller, S. C., & Batchelder, W. H. (1986). Culture as Consensus: A Theory of Culture and Informant Accuracy. American Anthropologist, 88(2), 313-338.
#' @param numPeople The number of people being surveyed
#' @param numQuestions The number of questions in your survey
#' @param numAns The number of possible answers on each question. It is assumed that all questions have the same number of answers. 
#' @keywords Consensus
#' @author Alastair Jamieson Lane. <aja107@@math.ubc.ca>
#' @author Benjamin Grant Purzycki. <bgpurzycki@@alumni.ubc.ca>
#' @export
#' @examples
#' FakeData<- GenerateConsensusData(5,5,3)
#' FakeSurvey<-FakeData$Survey
#' Results<- ConsensusPipeline(FakeSurvey,3)
GenerateConsensusData <-
function(numPeople,numQuestions, numAns,lockCompetance=NA){
  #This is a function that generates simulated results, given a particular number of people, questions and possible answers.
  #This is intended for use in testing consensus data functions.
  if(is.na(lockCompetance)){
    PersonSkills <- runif(numPeople, 0,1)
  }else if(is.numeric(lockCompetance) && lockCompetance>=0 && lockCompetance<=1 ){
    PersonSkills <- rep_len(lockCompetance, numPeople)
  }else{    
    stop("lockCompetance takes some non-valid value. Try giving a number in [0,1], or don't set it at all.")
  }  
  SurveyData<- data.frame()
  CorrectAns<- rep(1,numPeople) %*% t(sample(1:numAns, numQuestions,replace=T))
  GuessAns<- replicate(numQuestions,sample(1:numAns,numPeople,replace=T))
  Knowledge<- replicate(numQuestions, (PersonSkills>runif(numPeople, 0,1)) )  
  SurveyData<- ifelse(Knowledge,CorrectAns,GuessAns)
  colnames(SurveyData)<- paste( "Question", c(1:numQuestions))
  rownames(SurveyData)<- paste( "Person", c(1:numPeople))
  names(PersonSkills)<- rownames(SurveyData)
  CorrectAns<-CorrectAns[1,]
  names(CorrectAns)<-colnames(SurveyData)
  ReturnVal<-list()
  ReturnVal$Survey<-SurveyData
  ReturnVal$Answers<-CorrectAns
  ReturnVal$Competence<-PersonSkills
    
  return(ReturnVal)
}
