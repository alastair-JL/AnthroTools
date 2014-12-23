ConsensusPipeline <-
function(SurveyResults,numQ){
  M<-MakeDiscountedAgreementMatrix(SurveyResults,numQ)  
  Competance<-ComreySolve(M)
  Probs<- BayesConsensus(SurveyResults,Competance,numQ)
  AnsKey<-  apply(Probs,2,which.max)  
  names(Competance)<-rownames(SurveyResults)
  TestScore<- rowMeans(SurveyResults==rep(1,nrow(SurveyResults)) %*% t(AnsKey) )
  
  ReturnThing<-list()
  ReturnThing$Answers<-AnsKey
  ReturnThing$Competance<-Competance
  ReturnThing$TestScore<-TestScore  
  ReturnThing$Probs<-Probs
  return(ReturnThing)
}
