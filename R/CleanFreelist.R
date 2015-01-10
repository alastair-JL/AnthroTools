#' CleanFreeList
#'
#' Given a Free list dataset with Codes, Ordering and subject numbers, tidy it up, detect duplicates, and just generally make it nicer.
#' @usage CleanData<-CleanFreeList(mydata, Order="Order",Subj="Subj",CODE="CODE",ejectBadSubj=T,deleteDoubleCode=T,ConsolidateOrder=T,RemoveMissingData=T)
#' @param mydata The Freelist data. This should be a data frame, where each row contains a single response from a single respondent. For each such response, you need to know the subj number (or some form of unique identifier), the response (or "CODE" of the response **Explain more??**) and the the ranking/order of the response (What was this respondents the first response? The second? The Seventeenth?) 
#' @param Order This is the name of the column which contains the "Order" information. For each subject responses should be ordered uniquely from 1 to N, where N is the number of responses. There should be no gaps or double ups. Defaults to "Order"
#' @param Subj This is the name of the column containing your subject names/numbers. Each subject should have a unique identifier in this column (Is this confusing, given that each subject may take several rows?). Defaults to "Subj".
#' @param CODE This is the name of the column containing your subject names/numbers. Each subject should have a unique identifier in this column (Is this confusing, given that each subject may take several rows?). Defaults to "Subj".
#' @param ejectBadSubj Do you want to eject all subjects who have (for whatever reason) bad data? For example duplicates or missing order entries. defaults to true.
#' @param deleteDoubleCode If someone says "Apple" twice, do you want to drop all but the first instance? defaults to true.
#' @param ConsolidateOrder Do you want to "consolidate" order data. EG: 1 2 5 7-> 1 2 3 4. Useful if you intend to be removing some rows but... use with caution. NOTE: BEN, do I need to give more options here? Get function to only consolidate its own gaps perhaps? 
#' @param RemoveMissingData Not currently implemented. General idea was to just drop lines whenever you have things like blank CODE, or Order==NA. Or you could have it flag these as concerns and pass them back perhaps.
#' @return A new free list dataframe, without duplicates and such.
#' @keywords Freelist
#' @export
#' @examples
#' fakeData<- GenerateFakeFreeListData()
#' niceFakeData<- CalculateSalience(fakeData)
#' 
CleanFreeList <-
function(mydata, Order="Order",Subj="Subj",CODE="CODE",ejectBadSubj=T,deleteDoubleCode=T,ConsolidateOrder=T,RemoveMissingData=T){
      
  if(!(Order %in% colnames(mydata))){    
     stop('Specified "Order" column not valid.')
  }
  
  if(!(Subj %in% colnames(mydata))){    
    stop('Specified "Subj" column not valid.')
  } 
  
  if(!(CODE %in% colnames(mydata))){    
    stop('Specified "CODE" column not valid.')
  } 
  
  if(deleteDoubleCode & !ConsolidateOrder){    
    warning('Giving this function authority to remove rows without allowing Order relabelling may give strange results!')
  } 
  
    newData<-mydata[1,]
    newData<-newData[-1,]
  
  for( iii in sort(unique(mydata[,Subj]))) {    
    good=T
    
    NextFrame<- mydata[which(mydata[,Subj]==iii),]
    NextFrame <- NextFrame[order(NextFrame[,Order], decreasing=FALSE),]
    if(deleteDoubleCode){
      NextFrame <- NextFrame[!duplicated(NextFrame[,CODE]),]    
      ##This removes all duplicated codes.
    }
    
    if(any(duplicated(NextFrame[,Order]))){      
      good=F
      if(ejectBadSubj){
        warning(paste('Subject',iii,"has multiple entries with the same Order. I'm not really sure how that happend. Will be ejected from clean data.") )       
      }else{
        warning(paste('Subject',iii,"has multiple entries with the same Order. I'm not really sure how that happend. Will be kept. Please consider carefully what this means.") )         
      }
    }else{
      if(ConsolidateOrder){        
        NextFrame[,Order]<-seq(length(NextFrame[,Order]))
        ##Relabel "ORDER" so that it is a nice continuous list. Horray.
      }          
    }
       
    if(good | !ejectBadSubj){
      newData= rbind(newData,NextFrame)      
    }    
    
  }
    
  return(newData)
}
