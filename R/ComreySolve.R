#' ComreySolve
#'
#' Given a square matrix, such that the matrix is the outer product of a single vector with itself (except for the diagonal, which is ignored), determine the vector that was originally used.
#' ComreySolve also has a helper function called ComreyIterate. There isn't really any reason why you would use Comrey iterate by itself.
#' @usage ComreySolve(M, precision = 0.005)
#' @aliases ComreySolve ComreyIterate
#' @param M A square matrix.
#' @param precision The level of error at which the process will stop iterating and return an answer. Error is measured as "change in output vector between two iteration steps."
#' @keywords Consensus
#' @export
#' @return A single vector of numbers representing the vector that was originally used to create M.
#' @references The Minimum Residual method of Factor Analysis.
#' Comrey, A.L.
#' Psychological Reports, (1962) 11:15-18
#' @author Alastair Jamieson Lane. <aja107@@math.ubc.ca>
#' @author Benjamin Grant Purzycki. <bgpurzycki@@alumni.ubc.ca>
#' @note NOTE TO SELF: I really need to go back and re-read Comrey's paper to make sure I have done this right!
#' @examples
#' x<- 1:5
#' mat<- x \%*\% t(x)
#' y<- ComreySolve(mat)
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
   warning(paste('Unclear if M matrix is truly outer product of a vector with itself. Mean error',mean(FinalError), ". If you are using this function for Consensus Analysis, then this may indicate that one of the mathematical assumptions of Consensus analysis is violated (for example, you potentially have two or more seperate domains of knowledge being tested).") )     
   ##NOTE to self. Note really sure how useful this error message is.
  }    
  
  return(A5)
}
