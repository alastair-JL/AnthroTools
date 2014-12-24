#' A Cat Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()

FreelistTable.freq <-
function(mydata,CODE="CODE",Salience="Salience", Subj="Subj",subjNum,CODEnum){
  if(any(mydata[,Subj]==subjNum & mydata[,CODE]==CODEnum) ){    
    ret<- nrow(mydata[which(mydata[,Subj]==subjNum & mydata[,CODE]==CODEnum),])
    return(ret)
  }else{
    return(0)
  }  
}
