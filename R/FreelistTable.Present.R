#' @export
FreelistTable.Present <-
function(mydata,CODE="CODE",Salience="Salience", Subj="Subj",subjNum,CODEnum){
  if(any(mydata[,Subj]==subjNum & mydata[,CODE]==CODEnum) ){    
    return(1)
  }else{
    return(0)
  }
  
}
