#' ConsensusPipeline
#'
#' This is the main function to use when trying to do consensus analysis on a selection of survey data. This function pretty much streamlines all the consensus analysis functions currently contained in the package into one function.
#' 
#' This method takes a set of survey results, along with a number representing the number of possible answers to each question. It calculates the correlation between different people's answers, corrects for random chance, estimates each individual's expertise, and then determines the most likely answer for each question.
#' @param SurveyResults These are your survey results, written as a data frame. Rows are expected to represent participants, columns to represent individual questions. Each cell contains a particular individuals answer to a particular question- currently coded as a number (it is assumed questions are multiple choice).
#' @param numQ Currently this is a single number representing the number of possible answers to each question (hence for a true/false question, enter "2", for a multiple-choice, "4" perhaps, depending on the number of options).
#' @param safetyOverride This function has a variety of checks that will outright prevent it being applied to inapproriate data. Setting the safetyOveride to true will allow the function to run to completion, even when important assumptions are violated. WARNING: It is probably unwise to mess with this parameter.
#' @return 
#' This function returns a list with four components.
#' \item{Answers}{A list of the function's estimated answers for each question.}
#' \item{Competence}{The estimated competence for each individual (the probability that they would KNOW the answer to some future question).} 
#' \item{TestScore}{The supposed test score of each individual, assuming the answer key determined by the method}
#' \item{Probs}{The probability that each answer is correct for a given question ASSUMING that the method has correctly determined each individuals competence. 
#'           Given how Competence is calculated, (from the model) it may be reckless to take this "probability" too seriously.}
#' @note If you wish to stress test this function, use \code{\link{ConsensusStressTest}}. For a discussion of the limitations of the methods, and potential pitfalls of the programme, examine \code{\link{ConsensusCaveats}}.
#' @keywords Consensus
#' @export
#' @examples
#' FakeData<- GenerateConsensusData(16,16,4)
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
function(SurveyResults,numQ,safetyOverride=FALSE){
  M<-MakeDiscountedAgreementMatrix(SurveyResults,numQ)  
  ComResult<-ComreySolve(M)
  Competence<-ComResult$main
  
  
  if(all(Competence==-2) ){
    if(safetyOverride){
      warning(paste("Comrey Solver failed to find ANY valid Competence vector. We don't know why, but this is probably very bad. You should almost certainly ignore all results now given. Please turn the Safety override back off. Using uniform competance as default, you monster."  ))
      Competence=rep(0.5,nrow(SurveyResults))
    }else{
      stop(paste("Comrey Solver failed to find ANY valid competence vector. We don't know why. Function Aborting."  ))      
    }
  }
  
  
  origCompetence<-Competence
        
  if( sum(Competence>=1)>1 && nrow(unique(SurveyResults[Competence>=1,]))>1){        
    if(safetyOverride){
      warning(paste("Somehow we calculate multiple individuals with competence over 1... and they disagree. Reducing there competence to reasonable levels, but seriously, you shouldn't be using the safety override."  ))
      Competence[Competence>=1]<- 0.9+0.1*(max(Competence[Competence<1]))        
    }else{
      stop(paste("Somehow we calculate multiple individuals with competence over 1... and they disagree. Function Aborting."  ))      
    }
  }
  
  Competence[Competence>1]<-1  
  Competence[Competence<0]<-0  
  
  
  if(ComResult$ratio<3 &&ComResult$ratio>0){
    if(safetyOverride){
    warning(paste("Ratio of major eigenvectors is only ", ComResult$ratio , ". The assumptions of consensus analysis are questionable, given this result. Results currently output are liable to be wrong in a variety of ways."  ))
    }else{
      stop(paste("Ratio of major eigenvectors is only ", ComResult$ratio , "The assumptions of consensus analysis are questionable, given this result. Function halted"  ))      
    }
  }
  if(ComResult$ratio<5 &&ComResult$ratio>0){
      warning(paste("Ratio of major eigenvectors is only ", ComResult$ratio , "This is possible evidence that one of the assumptions of consensus analysis MAY have been violated. Proceed carefully."  ))   
  }else if(ComResult$ratio<7 &&ComResult$ratio>0){
    warning(paste("Ratio of major eigenvectors is ", ComResult$ratio , "This is weak evidence that one of the assumptions of consensus analysis could possibly have been violated, but probably everything if fine."  ))   
  }
  
  Probs<- BayesConsensus(SurveyResults,Competence,numQ)
  AnsKey<-  apply(Probs,2,which.max)  
  names(Competence)<-rownames(SurveyResults)
  TestScore<- rowMeans(SurveyResults==rep(1,nrow(SurveyResults)) %*% t(AnsKey) )

  reportback<- paste("We encounterd ", sum(origCompetence<0)," individuals with ``negative'' competance. 
                     We found ", sum(origCompetence>1), " individuals with competance over one.
                     The main eigenValue found was ", sum(ComResult$main*ComResult$main), ". The second was "
                     , sum(ComResult$second*ComResult$second),", giving a ration of ",ComResult$ratio,".")
  
  ReturnThing<-list()
  ReturnThing$Answers<-AnsKey
  ReturnThing$Competence<-Competence
  ReturnThing$origCompetence<-origCompetence
  ReturnThing$TestScore<-TestScore  
  ReturnThing$Probs<-Probs
  ReturnThing$reportback<-reportback
  return(ReturnThing)
}

##NOTE: We should implent something akin to Daniel J. Hruschka1 and Jonathan N. Maupin test.
#It seems like a good plan And is totally worth implementing. We should also probably cite them, in the paper, 
#Or at the very least once we have implemented the function. ... Actually, I could probably just do it now.