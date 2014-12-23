ComreyIterate <-
function(M,A,MinZero=TRUE){ 
  A2<- rep(0,length(A))  
  for (iii in 1:length(A) ){
    A2[iii]<-sum(M[iii,-iii]*A[-iii])/sum(A[-iii]*A[-iii])    
  }
  if (MinZero){
    A2[which(A2<0)]<-0     
  }
  return (A2)  
}
