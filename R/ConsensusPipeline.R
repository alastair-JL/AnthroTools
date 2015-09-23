#' ConsensusPipeline
#'
#' This is the main function to use when trying to do consensus analysis on a selection of survey data. This function pretty much streamlines all the consensus analysis functions currently contained in the package into one function.
#' 
#' This method takes a set of survey results, along with a number representing the number of possible answers to each question. It calculates the correlation between different people's answers, corrects for random chance, estimates each individual's expertise, and then determines the most likely answer for each question.
#' @param SurveyResults These are your survey results, written as a data frame. Rows are expected to represent participants, columns to represent individual questions. Each cell contains a particular individuals answer to a particular question- currently coded as a number (it is assumed questions are multiple choice).
#' @param numQ Currently this is a single number representing the number of possible answers to each question (hence for a true/false question, enter "2", for a multiple-choice, "4" perhaps, depending on the number of options).
#' @return 
#' This function returns a list with four components.
#' \item{Answers}{A list of the functions estimated answers for each question.}
#' \item{Competence}{The estimated competence for each individual (the probability that they would KNOW the answer to some future question).} 
#' \item{TestScore}{The supposed test score of each individual, assuming the answer key determined by the method}
#' \item{Probs}{The probability that each answer is correct for a given question ASSUMING that the method has correctly determined each individuals competence. 
#'           Given how Competence is calculated, (from the model) it may be reckless to take this "probability" too seriously.}
#' @keywords Consensus
#' @export
#' @examples
#' FakeData<- GenerateConsensusData(8,8,4)
#' Survey <- FakeData$Survey
#' ConsensusResult <- ConsensusPipeline(Survey,4)
#' ConsensusResult$Answers
#' FakeData$Answers
#' 
#' @references 
#' Oravecz, Z., Vandekerckhove, J., & Batchelder, W. H. (2014). Bayesian Cultural Consensus Theory. Field Methods, 1525822X13520280. http://doi.org/10.1177/1525822X13520280 
#'  @references 
#'  Romney, A. K., Weller, S. C., & Batchelder, W. H. (1986). Culture as Consensus: A Theory of Culture and Informant Accuracy. American Anthropologist, 88(2), 313-338.
#' 
#' @author Alastair Jamieson Lane. <aja107@@math.ubc.ca>
#' @author Benjamin Grant Purzycki. <bgpurzycki@@alumni.ubc.ca>
#' @note This function (and library) could probably use some additional features. If there are particular features you would like to see added, please email Jamieson-Lane, and he will see about adding them.
#' 
ConsensusPipeline <-
function(SurveyResults,numQ){
  M<-MakeDiscountedAgreementMatrix(SurveyResults,numQ)  
  Competence<-ComreySolve(M)
  Probs<- BayesConsensus(SurveyResults,Competence,numQ)
  AnsKey<-  apply(Probs,2,which.max)  
  names(Competence)<-rownames(SurveyResults)
  TestScore<- rowMeans(SurveyResults==rep(1,nrow(SurveyResults)) %*% t(AnsKey) )
  
  ReturnThing<-list()
  ReturnThing$Answers<-AnsKey
  ReturnThing$Competence<-Competence
  ReturnThing$TestScore<-TestScore  
  ReturnThing$Probs<-Probs
  return(ReturnThing)
}
