CalculateSalience <-
function(mydata, Order="Order",Subj="Subj",Norm=FALSE,Salience="Salience"){
  ##This is a script which, given a dataset containing a list of subjects,
  ##and an ordering of responses will compute the "salience" of each response.
  ##Salience is effectively "What fraction of the way up the list are we?" 
  ##If an individual gives five responses to a freelisting question,
  ##the first will have salience 1, the second 4/5, the third 3/5 and so on.  
 ##  If the "Normalise" value is true, saliences will be normalised such that each individual has
  ## a total salience of 1. Otherwise, the set will be normalised so they have a maximum salience of 1.
  
  mydata[,Salience]<- 0

  badSubjects=list()
  
  if(!(Order %in% colnames(mydata))){    
     stop('Specified "Order" column not valid.')
  }
  
  if(!(Subj %in% colnames(mydata))){    
    stop('Specified "Subj" column not valid.')
  }
  
  if(!(Norm %in% list(T,F))){    
    stop('Specified "Norm" not a logical.')
  }
  
  
  for( iii in unique(mydata[,Subj])){    
    ## for now I will assume that data is good and clean (no missing elements) and ordered. 
    ## Will probably want to come back and deal with borked cases as needed later. 
    OrderList<-mydata[which(mydata[,Subj]==iii), Order]
    if(!all(sort(OrderList)==seq(length(OrderList))) ){
      warning(paste('Subject',iii,'has bad order data and can not have salience calculated') )       
      mydata[which(mydata[,Subj]==iii), Salience]<-NA
    }else{        
      mydata[which(mydata[,Subj]==iii), Salience]<- (max(mydata[which(mydata[,Subj]==iii), Order]) - mydata[which(mydata[,Subj]==iii), Order]+1)/max(mydata[which(mydata[,Subj]==iii), Order])        
      if(Norm){
        mydata[which(mydata[,Subj]==iii), Salience]<-mydata[which(mydata[,Subj]==iii), Salience]/sum(mydata[which(mydata[,Subj]==iii), Salience])
      }
    }
  }
    
  return(mydata)
}
