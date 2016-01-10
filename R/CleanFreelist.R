#' CleanFreeList
#'
#' Given a Free list dataset with Codes, Ordering and subject numbers, remove incorrectly entered subjects, detect duplicate codes, remove blank codes, and enforce sequential ranking.
#' @usage CleanData<-CleanFreeList(mydata, Order="Order",Subj="Subj",CODE="CODE",ejectBadSubj=T,deleteDoubleCode=T,ConsolidateOrder=T,RemoveMissingData=T)
#' @param mydata The free-list data. This should be a data frame, where each row contains a single response from a single respondent. For each such response, you need to know the subject number (or some form of unique identifier), the response (or "CODE" of the response and the ranking/order of the response (What was this respondents the first response? The second? The Seventeenth?). The "CODE" of the response is the target variable. We use "code" here as some free-list data gets coded into another coding scheme.
#' @param Order This is the name of the column which contains the "Order" information. For each subject responses should be ordered uniquely from 1 to N, where N is the number of responses. There should be no gaps or double ups. Defaults to "Order"
#' @param Subj This is the name of the column containing your subject names/numbers. Each subject should have a unique identifier in this column (Is this confusing, given that each subject may take several rows?). Defaults to "Subj".
#' @param CODE This is the name of the column containing your subject names/numbers. Each subject should have a unique identifier in this column (Is this confusing, given that each subject may take several rows?). Defaults to "Subj".
#' @param ejectBadSubj Do you want to eject all subjects who have (for whatever reason) bad data? For example duplicates or missing order entries. defaults to true.
#' @param deleteDoubleCode If someone says "Apple" twice, do you want to drop all but the first instance? Defaults to False.
#' @param ConsolidateOrder Do you want to "consolidate" order data. EG: 1 2 5 7-> 1 2 3 4. Useful if you intend to be removing some rows.
#' @param RemoveMissingData Remove any row where Code is NA, or blank.
#' @return A new free list dataframe, with all the requested error types removed. (NOTE, the effect that such removal will have on your statistics is unknown. We believe this method makes sensible modifications, but advise the use of caution.)
#' @keywords Freelist
#' @export
#' @examples
#' data(UglyList)
#' View(CleanFreeList(UglyList))
#' View(CleanFreeList(UglyList,deleteDoubleCode=T))
#' View(CleanFreeList(UglyList,ejectBadSubj=F))
#' 
CleanFreeList <-
function(mydata, Order="Order",Subj="Subj",CODE="CODE",ejectBadSubj=T,deleteDoubleCode=F,ConsolidateOrder=T,RemoveMissingData=T){
      
  if(!(Order %in% colnames(mydata))){    
     stop('Specified "Order" column not valid.')
  }
  
  if(!(Subj %in% colnames(mydata))){    
    stop('Specified "Subj" column not valid.')
  } 
  
  if(!(CODE %in% colnames(mydata))){    
    stop('Specified "CODE" column not valid.')
  } 
  
  if( (deleteDoubleCode|RemoveMissingData) & !ConsolidateOrder){    
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
    
    if(RemoveMissingData){
      NextFrame <- NextFrame[!is.na(NextFrame[,CODE]),]    
      NextFrame <- NextFrame[!is.na(NextFrame[,Order]),]    
      NextFrame <- NextFrame[NextFrame[,CODE]!="",]    
      NextFrame <- NextFrame[NextFrame[,CODE]!=" ",]    
      ##This removes rows with NA or empty values.
    }
    
    
    if(any(duplicated(NextFrame[,Order]))){      
      good=F
      if(ejectBadSubj){
        warning(paste('Subject',iii,"has multiple entries with the same Order. Subject",iii, "Will be ejected from clean data. If you want to keep it, set 'ejectBadSubj=F' (not recommended).") )       
      }else{
        warning(paste('Subject',iii,"has multiple entries with the same Order. The function has been told not to Eject bad subjects, so please consider carefully what it means for a single subject to have multiple values with the same ranking.") )         
      }
    }else{
      if(ConsolidateOrder){        
        NextFrame[,Order]<-seq(length(NextFrame[,Order]))
        ##Relabel "Order" so that it is a nice continuous list. Horray.
      }          
    }
       
    if(good | !ejectBadSubj){
      newData= rbind(newData,NextFrame)      
    }    
    
  }
    
  return(newData)
}
