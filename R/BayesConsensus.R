BayesConsensus <-
function(AnswersGiven, Competancy, numAnswers, Prior=-1){
  if (Prior == -1) {
    Prior <- matrix(1/numAnswers, numAnswers, ncol(AnswersGiven))
  }
  if ( !all(abs(colSums(Prior) -1)<0.001) ){
    stop('Your Prior for every question must add to 1.')    
    }
    
  if ( ncol(Prior)!=ncol(AnswersGiven) || nrow(Prior)!=numAnswers){
    stop('Your Prior matrix is wrong shape.')    
  }  
  
  Probability<- matrix(0,numAnswers, ncol(AnswersGiven))
  colnames(Probability)<-colnames(AnswersGiven)
  rownames(Probability)<-c(1:numAnswers)
  WrongVect<- (1-Competancy)/numAnswers
  RightVect<- WrongVect+ Competancy
  for (QQQ in 1:ncol(AnswersGiven)){
    for(aaa in 1:numAnswers){
      Probability[aaa,QQQ]<- prod(ifelse(AnswersGiven[,QQQ]==aaa,RightVect,WrongVect) )      
    }
    Probability[,QQQ]<-Probability[,QQQ]*Prior[,QQQ]    
    Probability[,QQQ] <- Probability[,QQQ]/sum(Probability[,QQQ])    
  }  
    return(Probability)
}
