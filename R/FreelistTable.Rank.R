#' @export
#' 
FreeListTable.Rank <- function(mydata,CODE="CODE",Salience="Salience", Subj="Subj",subjNum,CODEnum){
  if(any(mydata[,Subj]==subjNum & mydata[,CODE]==CODEnum) ){    
    return(1 + sum(mydata[which(mydata[,Subj]==subjNum),Salience]>max(mydata[which(mydata[,Subj]==subjNum & mydata[,CODE]==CODEnum),Salience])))
  }
}