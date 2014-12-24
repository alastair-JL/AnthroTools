#' A Cat Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()
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
