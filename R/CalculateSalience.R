#' CalculateSalience
#'
#' Given a Free list dataset with Codes, Ordering and subject numbers, determine the "salience" of each response by item and/or by category. 
#' @usage CalculateSalience(mydata, Order="Order",Subj="Subj",CODE="CODE",GROUPING=NA,Rescale=FALSE,Salience="Salience")
#' @param mydata The free-list data. This should be a data frame, where each row contains a single response from a single respondent. For each such response, you need to know the subject number (or some form of unique identifier), the response (or "CODE" of the response) and the ranking/order of the response (What was this respondents the first response? The second? The Seventeenth?). The "CODE" of the response is the target variable. We use "code" here as some free-list data gets coded into another coding scheme.
#' @param Order This is the name of the column which contains the "Order" information. For each subject responses should be ordered uniquely from 1 to N, where N is the number of responses. There should be no gaps or double-ups. Defaults to "Order"
#' @param Subj This is the name of the column containing your subject names/numbers. Each subject should have a unique identifier in this column. Since each participant is likely to list multiple items, subject numbers will repeat. Defaults to "Subj". Subject numbers need to be unique within each grouping, but may be repeated if you have data from multiple groupings (multiple test sites for example).
#' @param CODE What column is your CODE stored in. <NA> values in this column will be excluded from the analysis.
#' @param GROUPING Do your subjects come from multiple test sites or subject groups? If so, you may wish to specify the column that specifies this - especially important if subject identification number is only unique within each group. Defaults to <NA>.
#' @param Rescale How are you going to scale salience? If set to false, then the Salience scores will be calculated in such a way that each response gets a score (N+1-k)/(N). Thus, the top score will have salience of 1.00. If Rescale is true then the above calculation will be done, and then all scores will be scaled such that each respondent's total salience is 1.00 (Thus preventing respondents with unusually long answer lists from dominating the analysis). This operation will keep the relative size of salience scores the same within each subject.
#' @param Salience This is the name of the column that you wish to have the salience scores stored in. Using the default setting is strongly recommended as it will make other functions in this package easier to use.
#' @return Returned will be a dataframe identical to your original data frame, but with an additional column containing the salience value of each response.
#' @keywords FreeList
#' @note This function produces several warning messages, most minor and self-explanatory. If your inputs for Rescale, Subj or Order seem wrong, the function will stop, and request better inputs. The one warning of interest is "Subject X has bad order data and cannot have salience calculated". This indicates that the order data on one of your subjects contains either a missing entry, or a double up, or perhaps a decimal. Whatever the cause, this data is considered bad, and no salience is calculated. 

#' @export
#' @examples
#' fakeData<- GenerateFakeFreeListData()
#' fakeSalData<- CalculateSalience(fakeData)
#' fakeRescaleData<- CalculateSalience(fakeData,Salience="RescaleSalience",Rescale=T)
#' fakeBothData<- CalculateSalience(fakeSalData,Salience="RescaleSalience",Rescale=T)
#' 
#' fakeData1<- GenerateFakeFreeListData()
#' fakeData1[,"Group"]="Mainland"
#' fakeData2<- GenerateFakeFreeListData()
#' fakeData2[,"Group"]="Island"
#' fakeDataCombo=rbind(fakeData1,fakeData2)
#' CalculateSalience(fakeDataCombo,GROUPING="Group")
#' 
CalculateSalience <-
function(mydata, Order="Order",Subj="Subj",CODE="CODE",GROUPING=NA,Rescale=FALSE,Salience="Salience"){
  ##This is a script which, given a dataset containing a list of subjects,
  ##and an ordering of responses will compute the "salience" of each response.
  ##Salience is effectively "What fraction of the way up the list are we?" 
  ##If an individual gives five responses to a freelisting question,
  ##the first will have salience 1, the second 4/5, the third 3/5 and so on.  
 ##  If the "Rescale" value is true, saliences will be rescale such that each individual has
  ## a total salience of 1. Otherwise, the set will be rescale so they have a maximum salience of 1.
  
  
  if(!(class(mydata)=="data.frame" )){    
    stop('"mydata" is not a data frame. That will make things not work.')
  }
  
  mydata[,Salience]<- 0

  badSubjects=list()
  
  
  
  if(!(Order %in% colnames(mydata))){    
     stop('Specified "Order" column not valid.')
  }
  
  if(!(Subj %in% colnames(mydata))){    
    stop('Specified "Subj" column not valid.')
  }
  
  
  if(!(CODE %in% colnames(mydata))){    
    stop('Specified "CODE" column not valid.')
  }
  
  if(!(Rescale %in% list(T,F))){    
    stop('Specified "Rescale" not a logical (IE, T or F).')
  }
  
  if(Subj==Order){    
    stop("Your subject number and order number are the same column. Cute, but you ain't fooling anyone into thinking that's valid.")
  }
  
    
  if(!(GROUPING %in% colnames(mydata)) && !is.na(GROUPING)){    
    warning("Grouping column not found. Continuing without grouping.")
    GROUPING=NA
  }
  
  
  if(Subj==CODE){    
    stop("Subj equals CODE. I'm not really sure how you expected this to work.")
  }
  
  if(Order==CODE){    
    stop("Order equals CODE. That seems a little bit silly to me.")
  }

  if (is.na(GROUPING)){
    
    if(Subj==GROUPING){    
      stop("Your subject number and grouping column are the same column. This will inevitably lead to the madness place. Aborting programme in order to avoid summoning the ancient ones that never sleep.")
    }
    if(GROUPING==CODE){    
      stop("GROUPING equals CODE. That does not seem like a good plan.")
    }
    if(Order==GROUPING){    
      stop("Order and GROUPING are the same thing! While I commend your ingenuity, this is clearly not a good plan, and I refuse to take any part in it.")
    }
    
    for( iii in unique(mydata[,Subj])){    
      ## for now I will assume that data is good and clean (no missing elements) and ordered. 
      ## Will probably want to come back and deal with borked cases as needed later. 
      OrderList<-mydata[which(mydata[,Subj]==iii), Order]
      if(!all(sort(OrderList)==seq(length(OrderList))) ){
        warning(paste('Subject',iii,'has bad order data and can not have salience calculated') )       
        mydata[which(mydata[,Subj]==iii), Salience]<-NA
      }else if( length(which( (mydata[,Subj]==iii) & !is.na(mydata[,CODE])))==0 ){
               warning(paste('Subject',iii,'did not list anything. They will get no salience scores.') )       
               mydata[which(mydata[,Subj]==iii), Salience]<-NA             
      }else{        
        mydata[which( (mydata[,Subj]==iii) & !is.na(mydata[,CODE])), Salience]<- (max(mydata[which( (mydata[,Subj]==iii) & !is.na(mydata[,CODE])), Order]) - mydata[which( (mydata[,Subj]==iii) & !is.na(mydata[,CODE])), Order]+1)/max(mydata[which( (mydata[,Subj]==iii) & !is.na(mydata[,CODE])), Order])        
        if(Rescale){
          mydata[which( (mydata[,Subj]==iii) & !is.na(mydata[,CODE])), Salience]<-mydata[which( (mydata[,Subj]==iii) & !is.na(mydata[,CODE])), Salience]/sum(mydata[which( (mydata[,Subj]==iii) & !is.na(mydata[,CODE])), Salience])
        }
      }
    }##End for loop
    
    ##End "No grouping" version.
  }else{ 
  
  for(ggg in unique(mydata[,GROUPING] ) ){  
    for( iii in unique(mydata[which(mydata[,GROUPING]==ggg),Subj])){    
      ## for now I will assume that data is good and clean (no missing elements) and ordered. 
      ## Will probably want to come back and deal with borked cases as needed later. 
      OrderList<-mydata[which(mydata[,GROUPING]==ggg & mydata[,Subj]==iii), Order]
      if(!all(sort(OrderList)==seq(length(OrderList))) ){
        warning(paste('Subject',iii,'has bad order data and can not have salience calculated. Perhaps "CleanFreeList" can help you.') )       
        mydata[which(mydata[,GROUPING]==ggg & mydata[,Subj]==iii), Salience]<-NA
      }else if( length(which(mydata[,GROUPING]==ggg & (mydata[,Subj]==iii) & !is.na(mydata[,CODE])))==0 ){
        warning(paste('Subject',iii,'did not list anything. They will get no salience scores.') )       
        mydata[which(mydata[,GROUPING]==ggg & mydata[,Subj]==iii), Salience]<-NA             
      }else{        
        mydata[which(mydata[,GROUPING]==ggg & mydata[,Subj]==iii & !is.na(mydata[,CODE])), Salience]<- (max(mydata[which(mydata[,GROUPING]==ggg &  (mydata[,Subj]==iii) & !is.na(mydata[,CODE])), Order]) - mydata[which(mydata[,GROUPING]==ggg &  (mydata[,Subj]==iii) & !is.na(mydata[,CODE])), Order]+1)/max(mydata[which(mydata[,GROUPING]==ggg &  (mydata[,Subj]==iii) & !is.na(mydata[,CODE])), Order])        
        if(Rescale){
          mydata[which(mydata[,GROUPING]==ggg &  (mydata[,Subj]==iii) & !is.na(mydata[,CODE])), Salience]<-mydata[which(mydata[,GROUPING]==ggg &  (mydata[,Subj]==iii) & !is.na(mydata[,CODE])), Salience]/sum(mydata[which(mydata[,GROUPING]==ggg &  (mydata[,Subj]==iii) & !is.na(mydata[,CODE])), Salience])
        }
      }
    }##End subj for loop
  }##End GROUP for loop.
    
  }  
  return(mydata)
}
