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
    A2<- ComreyIterate(M,A1,MinZero=FALSE)
    A3<- ComreyIterate(M,A2,MinZero=FALSE)
    A4<- (A2+A3)/2
    A5<-ComreyIterate(M,A4,MinZero=FALSE)
    A1<-(A4+A5)/2
    timeOut<-timeOut-1
    error<- max(abs(A5-A4))        
    if(error>10^8){
      ReturnThing<-list()
      ReturnThing$main<- -2
      ReturnThing$ratio<- 0
      ReturnThing$second<- -2      
      return(ReturnThing) ##NOTE: this is purely an escape clause for non-convergent matrices. It is handled by the consensus pipeline.
    }
  }
  
  N= M- A5 %*% t(A5)
  B1<- apply(N, 2, max)
  error<-2
  timeOut<-300
  
  while (timeOut>0 && error>precision){
    B2<- ComreyIterate(N,B1,MinZero=FALSE)
    B3<- ComreyIterate(N,B2,MinZero=FALSE)
    B4<- (B2+B3)/2
    B5<-ComreyIterate(N,B4,MinZero=FALSE)
    B1<-(B4+B5)/2
    timeOut<-timeOut-1
    error<- max(abs(B5-B4))      
  }
  
    
  ReturnThing<-list()
  ReturnThing$main<-A5  ##Okay this is what I THINK they mean by eigenvalue, but I'm not sure, will need to check back once have full info.
  ReturnThing$ratio<-sum(A5*A5)/sum(B5*B5)
  ReturnThing$second<-B5
  
  return(ReturnThing)
}
