#' @export

FreeListTable.MaxSal <-
function(mydata,CODE="CODE",Salience="Salience", Subj="Subj",subjNum,CODEnum){
  if(any(mydata[,Subj]==subjNum & mydata[,CODE]==CODEnum) ){    
    ret<- max(mydata[which(mydata[,Subj]==subjNum & mydata[,CODE]==CODEnum),Salience])
    return(ret)
  }else{
    return(0)
  }  
}
