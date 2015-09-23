#' MakeAgreementMatrix
#'
#' Given a bunch of answers to a survey, this function makes a matrix representing what fraction of the time each person agreed with each other person.
#' @param SurveyResults A matrix containing the answers to a bunch of survey questions. Each row represents a particular individual and each column represents a particular question.
#' @keywords CIB
#' @export
#' @return A matrix M, where each entry represents the percentage of the time that individual i agreed with individual j.
#' @note This is probably already implemented elsewhere in R, and may be identical to a correlation matrix or something. I feel slightly silly putting it here to be honest. Oh well.
#' @author Alastair Jamieson Lane. <aja107@@math.ubc.ca>
#' @author Benjamin Grant Purzycki. <bgpurzycki@@alumni.ubc.ca>
#' @examples
#' FakeData<- GenerateConsensusData(8,8,4)
#' Survey <- FakeData$Survey
#' Ma <- MakeAgreementMatrix(Survey)    
#' 
MakeAgreementMatrix <-
function(SurveyResults){
  ReturnVal= matrix(0, nrow(SurveyResults),nrow(SurveyResults))
  for(iii in 1:nrow(ReturnVal)){
    for(jjj in 1:ncol(ReturnVal)){
      ReturnVal[iii,jjj]= sum(SurveyResults[iii,]==SurveyResults[jjj,])/ncol(SurveyResults)
    }         
  }
  
  return(ReturnVal)
}
