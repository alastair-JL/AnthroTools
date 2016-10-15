#' @export
#' 
3+4

FreeListTable.SumSal <- function(mydata,CODE="CODE",Order="Order",Salience="Salience", Subj="Subj",subjNum,CODEnum){
  if(any(mydata[,Subj]==subjNum & mydata[,CODE]==CODEnum) ){
    ret<- sum(mydata[which(mydata[,Subj]==subjNum & mydata[,CODE]==CODEnum),Salience])
    return(ret)
  }else{
    return(0)
  }
}
