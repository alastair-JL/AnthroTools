#' @export
FreeListTable.Present <-
function(mydata,CODE="CODE",Order="Order",Salience="Salience", Subj="Subj",subjNum,CODEnum){
  if(any(mydata[,Subj]==subjNum & mydata[,CODE]==CODEnum) ){    
    return(1)
  }else{
    return(0)
  }
  
}
