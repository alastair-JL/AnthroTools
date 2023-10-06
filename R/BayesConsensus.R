#' BayesConsensus
#'
#' Given a list of each person's answer, an estimate of each person's competence, and knowledge of the number of answers available (which... is somewhat inferable, but oh well) determine the probability that any given answer is correct for a particular question.
#' @usage BayesConsensus(AnswersGiven, Competancy, numAnswers, Prior = -1)
#' @param AnswersGiven A data frame with rows representing individuals, and columns representing questions. Individual cells represent an individuals answer to a particular question.
#' @param Competancy A list containing numbers between zero and 1 representing the probability that a given individuals KNOWS the answer to a question in advance.
#' @param numAnswers The total number of answers available in each question. It is presently assumed that all questions have the same number of answers (I will investigate changing this later)
#' @param Prior Do you have a prior distribution of probabilities over you answers? For example, in advance do you believe "A" is twice as likely as "B"? If so, please enter this as a matrix now. If not, the computer will assume a uniform distribution.
#' @keywords Consensus
#' @return A matrix, where columns represent questions and rows represent possible answers. Each entry represents the probability that a particular answer is "correct" for a particular question, within the cultural consensus framework.
#' @references 
#' Oravecz, Z., Vandekerckhove, J., & Batchelder, W. H. (2014). Bayesian Cultural Consensus Theory. Field Methods, 1525822X13520280. http://doi.org/10.1177/1525822X13520280 
#' @references 
#' Romney, A. K., Weller, S. C., & Batchelder, W. H. (1986). Culture as Consensus: A Theory of Culture and Informant Accuracy. American Anthropologist, 88(2), 313-338.
#' @author Alastair Jamieson Lane. <aja107@@math.ubc.ca>
#' @author Benjamin Grant Purzycki. <bgpurzycki@@cas.au.dk>
#' @export
#' @examples
#' FakeData<- GenerateConsensusData(8,8,4)
#' Survey <- FakeData$Survey
#' M <- MakeDiscountedAgreementMatrix(Survey, 4)
#' Competance <- ComreySolve(M)
#' Probs <- BayesConsensus(Survey, Competance, 4)
BayesConsensus <-
function(AnswersGiven, Competancy, numAnswers, Prior=-1){
  if (Prior == -1) {
    Prior <- matrix(1/numAnswers, numAnswers, ncol(AnswersGiven))
  }
  if ( !all(abs(colSums(Prior) -1)<0.001) ){
    stop('Your Prior for every question must add to 1.')    
    }
  
  if ( !all(Prior>=0) ){
    stop('Your Prior Must assign non-negative probability to all possible outcomes (preferably positive).')    
  }
  
  if ( ncol(Prior)!=ncol(AnswersGiven) || nrow(Prior)!=numAnswers){
    stop('Your Prior matrix is wrong shape.')    
  }  
  
  Probability<- matrix(0,numAnswers, ncol(AnswersGiven))
  colnames(Probability)<-colnames(AnswersGiven)
  rownames(Probability)<-c(1:numAnswers)
  WrongVect<- (1-Competancy)*(numAnswers-1)/numAnswers
  RightVect<- 1-WrongVect
  for (QQQ in 1:ncol(AnswersGiven)){
    for(aaa in 1:numAnswers){
      Probability[aaa,QQQ]<- prod(ifelse(AnswersGiven[,QQQ]==aaa,RightVect,WrongVect) )      
    }
    Probability[,QQQ]<-Probability[,QQQ]*Prior[,QQQ]    
    Probability[,QQQ] <- Probability[,QQQ]/sum(Probability[,QQQ])    
  }  
    return(Probability)
}
