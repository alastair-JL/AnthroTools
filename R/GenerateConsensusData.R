GenerateConsensusData <-
function(numPeople,numQuestions, numAns){
  #This is a function that generates simulated results, given a particular number of people, questions and possible answers.
  #This is intended for use in testing consensus data functions.
  PersonSkills <- runif(numPeople, 0,1)
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
  ReturnVal$Competance<-PersonSkills
    
  return(ReturnVal)
}
