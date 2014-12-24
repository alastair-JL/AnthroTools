#' A Cat Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()
#' 
FreelistTable.Present <-
function(mydata,CODE="CODE",Salience="Salience", Subj="Subj",subjNum,CODEnum){
  if(any(mydata[,Subj]==subjNum & mydata[,CODE]==CODEnum) ){    
    return(1)
  }else{
    return(0)
  }
  
}
