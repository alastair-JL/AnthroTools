#' CalculateSalience
#'
#' Given a Free list dataset with Codes, Ordering and subject numbers, determine the "Salience" of each response. 
#' @usage CalculateSalience(mydata, Order = "Order", Subj = "Subj", Norm = FALSE, Salience = "Salience")
#' @param mydata The free list data. This should be a data frame, where each row contains a single response from a single respondent. For each such response, you need to know the subj number (or some form of unique identifier), the response (or "CODE" of the response **Explain more??**) and the the ranking/order of the response (What was this respondents the first response? The second? The Seventeenth?) 
#' @param Order This is the name of the column which contains the "Order" information. For each subject responses should be ordered uniquely from 1 to N, where N is the number of responses. There should be no gaps or double ups. Defaults to "Order"
#' @param Subj This is the name of the column containing your subject names/numbers. Each subject should have a unique identifier in this column (Is this confusing, given that each subject may take several rows?). Defaults to "Subj".
#' @param Norm How are you going to normalise salience? If set to false, then the Salience scores will be calculated in such a way that each response gets a score \eqn{ (N+1-k)/(N)}- thus the top score will have salience one. If Norm is true then the above calculation will be done, and then all scores will be normalised such that each respondants total salience is one (Thus preventing respondents with unusually long answer lists from dominating the analysis). This operation will keep the relative size of salience scores the same within each subject.
#' @param Salience This is the name of the column that you wish to have the salience scores stored in. Using the default setting is strongly recommended as it will make other functions in this package easier to use.
#' @return Returned will be a dataframe identical to your original data frame, but with an additional column containing the salience value of each response.
#' @keywords FreeList
#' @note This function produces several warning messages, most minor and self explanatory. If your inputs for Norm, Subj or Order seem wrong, the function will stop, and request better inputs. The one warning of interest is "Subject X has bad order data and can not have salience calculated". This indicates that the order data on one of your subjects contains either a missing entry, or a double up, or perhaps a decimal. Whatever the cause, this data is considered bad, and no salience is calculated.
#' @export
#' @examples
#' fakeData<- GenerateFakeFreeListData()
#' fakeSalData<- CalculateSalience(fakeData)
#' fakeNormData<- CalculateSalience(fakeData,Salience="NormSalience",Norm=T)
#' fakeBothData<- CalculateSalience(fakeSalData,Salience="NormSalience",Norm=T)
#' 
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
