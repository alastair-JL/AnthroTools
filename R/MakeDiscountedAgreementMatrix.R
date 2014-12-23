MakeDiscountedAgreementMatrix <-
function(SurveyResults,numAns){
  ReturnVal= matrix(0, nrow(SurveyResults),nrow(SurveyResults))
  for(iii in 1:nrow(ReturnVal)){
    for(jjj in 1:ncol(ReturnVal)){
      ReturnVal[iii,jjj]= sum(SurveyResults[iii,]==SurveyResults[jjj,])/ncol(SurveyResults)      
    }             
  }
  ReturnVal<- (ReturnVal*numAns -1)/(numAns-1)
  diag(ReturnVal)<-0
  
  return(ReturnVal)
}
