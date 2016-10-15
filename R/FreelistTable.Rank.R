#' @export
#' 
FreeListTable.Rank <- function(mydata,CODE="CODE",Salience="Salience",Order="Order", Subj="Subj",subjNum,CODEnum){
  if(any(mydata[,Subj]==subjNum & mydata[,CODE]==CODEnum) ){    
    return(min(mydata[which(mydata[,Subj]==subjNum & mydata[,CODE]==CODEnum),Order]))
  }
  else{
    return(0)
  }
}