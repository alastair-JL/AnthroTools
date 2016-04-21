#' ConsensusStressTest
#' 
#' A function for simulating a large number of surveys, running consensus analysis on each, and then comparing the 
#' true and calculated results to determine the algorithm's error rate. Used to test \code{\link{ConsensusPipeline}}.
#'  Also produces statistics on the mean and variance of the calculated variance scores, which can be used to determine how far
#'   off you might expect your estimates to be, and whether or not any competence variance you have encountered is likely to be due to purely statistical fluctuations.
#' @param numPeople How many "People" do you wish to simulate answering each survey?
#' @param NumQuestions How many questions are there on each of your simulated surveys?
#' @param numAns How many possible answers are their to the questions on your virtual survey?
#' @param lockCompetence Give a number between zero and one here to set all participants true competences to that value. Alternative give a vector here to set them equal to the vector.
#' @param Iterations How many surveys do you wish to simulate?
#' @return 
#' A list. First entry contains vector with two values: the number of errors, and the expected number of errors.
#' Second entry contains a vector of the calculated mean competencies, and the third entry is a vector giving the variance in
#' the competence for each iteration. We then have the Comrey Ratio, number of errors and expected number of errors (per survey), and 
#' then finally a vector containing the number of "anomolous" competence values outside the acceptable [0,1] range.
#' @keywords Consensus
#' @export
#' @examples
#' StressSummary<- ConsensusStressTest(15,15,4,5000,lockCompetence=0.6) 
#' ##15 individuals, 15 questions, answers A,B,C,D, 5000 surveys simulated, all individuals have competence 0.6.
#' StressSummary[[1]] ##True and expected error rate.
#' mean(StressSummary[[2]]) ##The mean of the mean of calculated competence (should be near 0.6).
#' sum((StressSummary[[3]])>0.1)/length(StressSummary[[3]]) 
#'  ## The proportion of simulations with Competence variance calculated above 0.1.
#'  ## Note that the true value for this is 0, so all variance found is noise.
#'quantile(StressSummary[[3]],0.95)  
#'  ##95% of simulations detected variance below this value- even when true variance is 0. 
#'  ##If your variance is below this level, there probably isn't much evidence for competence variability.
#' quantile(StressSummary[[3]],c(0.5,0.95,0.99,0.999) )     
#' sum(StressSummary[[4]]<3.0)
#'  ##This last number is the number of surveys with Comrey ratio less than 3- these are datasets
#'  ##that the function would refuse to analyse unless the safety override was used.
#'  ##Please understand that this is the number of "Good" datasets that the function believes are bad.
#'  ##This value tells you nothing about what the Comrey ratio is likely to look like on "bad" datasets where
#'  ##important assumptions are violated.
#' 
#' 
#' @author Alastair Jamieson Lane. <aja107@@math.ubc.ca>
#' @author Benjamin Grant Purzycki. <bgpurzycki@@alumni.ubc.ca>
#' @note This function (and library) could probably use some additional features. If there are particular features you would like to see added, please email Jamieson-Lane, and he will see about adding them.
#' 
ConsensusStressTest<-function(numPeople,NumQuestions,numAns,Iterations,lockCompetence=NA){
  if(class(numPeople)!="numeric"||class(NumQuestions)!="numeric"||class(numAns)!="numeric"||class(Iterations)!="numeric"){
    stop("All your inputs should be numbers!")
  }
  if(class(numPeople)<2||class(NumQuestions)<2||class(numAns)<2||class(Iterations)<2){
    stop("All your inputs (except lockCompetence) should be at least 2!")
  }  
  if(is.na(lockCompetence)){
  }else if(is.numeric(lockCompetence) && lockCompetence>=0 && lockCompetence<=1 ){
  }else{    
    stop("lockCompetence takes some non-valid value. Try giving a number in [0,1], or don't set it at all.")
  }  
  
    
  errors<-0
  expErrors<-0  
  AvgComp<- rep_len(0, Iterations)
  CompVary<- rep_len(0, Iterations)
  ComRatio<- rep_len(0, Iterations)  
  errorBySurvey<- rep_len(0, Iterations)  
  expErrorBySurvey<-rep_len(0, Iterations)  
  anomolousCompetence<-rep_len(0, Iterations)  
  
  for(iii in 1:Iterations){
    fakeData<-GenerateConsensusData(numPeople,NumQuestions,numAns,lockCompetence)
    fakeResults<-ConsensusPipeline(fakeData$Survey,numAns,safetyOverride=TRUE)
    errors<-errors+sum(fakeData$Answers!=fakeResults$Answers)
    expErrors<-expErrors+NumQuestions-sum(apply(fakeResults$Probs,2,max))
    print(paste(errors, "errors.",expErrors ," expected errors.", iii ,"Iterations."))
    AvgComp[iii]<- mean(fakeResults$origCompetence)
    CompVary[iii]<- var(fakeResults$origCompetence)
    ComRatio[iii]<-(fakeResults$reportNumbers)[5];
    errorBySurvey[iii]<-sum(fakeData$Answers!=fakeResults$Answers);
    expErrorBySurvey[iii]<-NumQuestions-sum(apply(fakeResults$Probs,2,max))
    anomolousCompetence[iii]<-(fakeResults$reportNumbers)[1]+(fakeResults$reportNumbers)[2]
  }    
 print(paste("In this stress test, the method incorrectly determined ",errors ," answers."))
 print(paste("The method expects to have ",expErrors ," such mistakes."))
 print(paste("The method correctly determined ",(NumQuestions*Iterations-errors) ," answers"))
 print(paste("The Average competence across the whole sample was",mean(AvgComp) ,"."))
 print(paste("The variance in competence across numerous iterations was ",mean(CompVary) ,"."))
 print(paste("The method encountered ", sum(anomolousCompetence)," Competence values outside the acceptable [0,1] range.")) 
 
  return(list(c(errors,expErrors),AvgComp,CompVary,ComRatio,errorBySurvey,expErrorBySurvey,anomolousCompetence))
}