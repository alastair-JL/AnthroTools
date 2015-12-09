#' ConsensusStressTest
#' 
#' A function for simulating a large number of surveys, running consensus analysis on each, and then comparing the 
#' true and calculated results to determine the algorithms error rate. Used to test \code{\link{ConsensusPipeline}}.
#' @param numPeople How many "People" do you wish to simulate answering each survey?
#' @param NumQuestions How many questions are there on each of your simulated surveys?
#' @param numAns How many possible answers are their to the questions on your virtual survey?
#' @param Iterations How many surveys do you wish to simulate?
#' @return 
#' This function vector with two values: the number of errors, and the expected number of errors.
#' @keywords Consensus
#' @export
#' @examples
#' ConsensusStressTest(15,15,4,500)
#' 
#' @author Alastair Jamieson Lane. <aja107@@math.ubc.ca>
#' @author Benjamin Grant Purzycki. <bgpurzycki@@alumni.ubc.ca>
#' @note This function (and library) could probably use some additional features. If there are particular features you would like to see added, please email Jamieson-Lane, and he will see about adding them.
#' 
ConsensusStressTest<-function(numPeople,NumQuestions,numAns,Iterations){
  if(class(numPeople)!="numeric"||class(NumQuestions)!="numeric"||class(numAns)!="numeric"||class(Iterations)!="numeric"){
    stop("All your inputs should be numbers!")
  }
  if(class(numPeople)<2||class(NumQuestions)<2||class(numAns)<2||class(Iterations)<2){
    stop("All your inputs should be at least 2!")
  }  
  errors<-0
  expErrors<-0  
  for(iii in 1:Iterations){
    fakeData<-GenerateConsensusData(numPeople,NumQuestions,numAns)
    fakeResults<-ConsensusPipeline(fakeData$Survey,numAns,safetyOverride=TRUE)
    errors<-errors+sum(fakeData$Answers!=fakeResults$Answers)
    expErrors<-expErrors+NumQuestions-sum(apply(fakeResults$Probs,2,max))
    print(paste(errors, "errors.",expErrors ," expected errors.", iii ,"Iterations."))
  }    
 print(paste("In this stress test, the method incorrectly determined ",errors ," answers."))
 print(paste("The method expect to have ",expErrors ," such mistakes."))
 print(paste("The method correctly determined ",(NumQuestions*Iterations-errors) ," answers"))
  
  return(c(errors,expErrors))
}