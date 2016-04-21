x<-runif(7)
M<- x %*% t(x)
Agreements<-0*M
Q<-27
N<-5;

for(iii in 1:(length(x)-1) ){  
  for(jjj in (iii+1):length(x)){    
    A<-M[iii,jjj]*(1-1/N)+1/N;
    Agreements[iii,jjj] <- rbinom(1,Q,A) 
  }    
}
Agreements<-Agreements+t(Agreements)


probCalc<- function(x,Agreements,Q,N){
  result<- -1;
  len= length(x);  
  if( any(x>1)|any(x<0) ){
    return(0)
  }
  
  for(iii in 1:(len-1)) {  
    for(jjj in (iii+1):len){          
        A= 1/N+ x[iii]*x[jjj]*(1-1/N);
        
        result<-result * A^(Agreements[iii,jjj])* (1-A)^(Q-Agreements[iii,jjj]);      
    }    
  }    
  return(result*10^200)
}

[
QMCIntProbs<-function(x,Agreements,Q,N){
  numPep<-length(x);
  numPoints<-5^numPep;
  listPrimes<- c(2,3, 5,7,11,13,17,19,23,29,31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 
                 97, 101,  103,107, 109, 113, 127, 131,137,139,149, 151, 157,163,167,173)
  if (numPep>length(listPrimes)){
    stop('This is too computationally expensive. What were you thinking. You idiot.');    
  }
  listPrimes<-listPrimes[1:numPep];
  listPrimes<-sqrt(listPrimes)
      
  CurrentPoint<-runif(numPep)
  
  sumBase<-0  
  sumEach<-0*x;
  for(iii in 1:numPoints){
    prob<-probCalc(CurrentPoint,Agreements,Q,N)
    sumBase <- sumBase+prob;
    sumEach<-sumEach+prob*CurrentPoint;
    if((iii%%500)==0){
      print(paste(iii,'out of',numPoints))
    } 
    CurrentPoint<-(CurrentPoint+listPrimes)%%1
  }      
  
  sumEach<- sumEach/sumBase;
  sumBase<- sumBase/numPoints;  
  
  return(list(sumEach,sumBase) )
}][]



MCIntProbs<-function(x,Agreements,Q,N){
  numPep<-length(x);
  numPoints<-5^numPep;
  rands <- runif(numPep*numPoints);
  rands<-matrix(rands,numPep,numPoints);  
  sumBase<-0  
  sumEach<-0*x;
  for(iii in 1:numPoints){
    prob<-probCalc(rands[,iii],Agreements,Q,N)
    sumBase <- sumBase+prob;
    sumEach<-sumEach+prob*rands[,iii];
    print(iii)
  }      
  
  sumEach<- sumEach/sumBase;
  sumBase<- sumBase/numPoints;  
  
  return(list(sumEach,sumBase) )
}


probCalc(y,Agreements,Q,N)
install.packages("optim")
library(optim)
y<- runif(length(x))
probCalc(y,Agreements,Q,N)/

Solv<-nlm(probCalc,y,Agreements,Q,N,print.level = 2)
Solv$estimate

Solve3<-QMCIntProbs(y,Agreements,Q,N)
