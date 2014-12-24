#' A Cat Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function() 

ComreySolve <-
function(M, precision=0.005){
  A1<- apply(M, 2, max)
  error<-2
  timeOut<-300
  while (timeOut>0 && error>precision){
    A2<- ComreyIterate(M,A1)
    A3<- ComreyIterate(M,A2)
    A4<- (A2+A3)/2
    A5<-ComreyIterate(M,A4)
    A1<-(A4+A5)/2
    timeOut<-timeOut-1
    error<- max(abs(A5-A4))    
  }
  
  FinalError<- M- A5 %*% t(A5)
  FinalError<-FinalError * (matrix(1,nrow(M),ncol(M))- diag(1,ncol(M)) )
  FinalError<- abs(FinalError)
  if(mean(FinalError) >0.1){
   warning(paste('Unclear if M matrix is truly outer product of a vector with itself. Mean error',mean(FinalError)) )     
   ##NOTE to self. Note really sure how useful this error message is.
  }    
  
  return(A5)
}
