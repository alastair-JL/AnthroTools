#' SalienceByCode
#'
#' Generate a table containing salience details for each code, including mean Salience, and total salience over the entire survey set. This function should be preceded by  \code{\link{Calculate Salience}}.
#' 
#' @usage SalienceByCode(mydata, CODE = "CODE", Salience = "Salience", Subj = "Subj", dealWithDoubles = "DEFAULT")
#' 
#' @param mydata A table containing a list of free-list responses. Each response gets one row, containing the subject number, CODE, and calculated Salience of their response.
#' @param CODE The name of the column containing the "CODE" of the response. This is your free-list data. Since some free-list data will get lumped using some form of coding rubric, we have called this "CODE" by default (of course, you can call it whatever you wish, but you must include this in the syntax).
#' @param Salience The name of the Column containing the "Salience" of a response.
#' @param subj The name of the column containing the subject number. 
#' @param dealWithDoubles How you would like the function to deal with cases where a single survey respondent gave the same answer multiple times. On the default setting, the function will assume that no such cases arise, and will throw an error if it encounters such a case. Aside from DEFAULT, you also have the options MAX, MEAN, SUM and IGNORE. MAX indicates that for each code, you want the computer to pay note the first time a respondent lists a particular CODE, and ignore subsequent mentions. For SUM, you are asking the computer to determine each respondents TOTAL salience with respect to a given code. IGNORE tells the computer not to think about it, and is merely a way of suppressing errors. IGNORE is not recommended.
#' @keywords FreeList
#' @return Upon running this function, you will receive a data frame, where each row is one of you possible response codes, and each column contains a different statistic about the code. MeanSalience is the average salience of a code, averaged over the number of responses that referenced that code. Sum Salience just gives the total amount of salience of a given code, and SmithsS gives this sum, divided by the total number of respondents.
#' @references Quinlan, M. (2005). Considerations for collecting freelists in the field: Examples from ethobotany. Field Methods, 17(3), 219-234. http://doi.org/10.1177/1525822X05277460 
#' @export
#' @author Alastair Jamieson Lane. <aja107@@math.ubc.ca>
#' @author Benjamin Grant Purzycki. <bgpurzycki@@alumni.ubc.ca>
#' @examples
#' data(FruitList)
#' test<- CalculateSalience(FruitList)
#' SalienceByCode(test,dealWithDoubles="MAX")
#' 
SalienceByCode <-
function(mydata,CODE="CODE",Salience="Salience", Subj="Subj", dealWithDoubles="DEFAULT"){
    
  if(!(CODE %in% colnames(mydata))){    
    stop('Specified "CODE" column not valid.')
  }
  
  if(!(Salience %in% colnames(mydata))){    
    stop('Specified "Salience" column not valid.')
  }
  
  if(any(is.na(mydata[, Salience] ) )){
    warning("Some of the Salience data's are missing. Consider cleaning your data before trusting these results.")    
  }              
  
  if(!dealWithDoubles %in% c("DEFAULT" ,"IGNORE" ,"MAX","SUM") ) {
    stop(' dealWithDoubles must take a value of "DEFAULT","MAX", "SUM" or "IGNORE". ')    
  }
  
  SalienceByCode <- data.frame(CODE=unique(mydata[,CODE]),
                               MeanSalience= 0, ##The mean of intensity GIVEN that a code showed up.
                               SumSalience=0,  ## the SUM of all intensities for a given code.
                               SmithsS=0  ## the SUM of all intensities for a given code.
  ) 
  
  for( iii in SalienceByCode$CODE){  
    
    if( anyDuplicated(mydata[which(mydata[,CODE]==iii), Subj])>0 && dealWithDoubles=="DEFAULT" ) {
      stop('Some subjects have multiple entries with same code. Set "dealWithDoubles" to "SUM", "MAX" or "IGNORE" to deal with this.')      
    }
    
      if(dealWithDoubles=="MAX" || dealWithDoubles=="SUM"||dealWithDoubles=="MEAN" ){
        if(dealWithDoubles=="MAX"){
          doThing<-max
        }else if(dealWithDoubles=="SUM"){
          doThing<-sum
        }else{
          doThing<-mean          
        }        
        for( jjj in unique(mydata[,Subj])){    
          witch<-which(mydata[,CODE]==iii & mydata[,Subj]==jjj ) #           
          if (length(witch)>1){            
            mydata[witch[1],Salience]<-doThing(mydata[witch,Salience])
            mydata[witch[-1],Salience]<-NA
            if(!is.na(mydata[witch[1],Salience])){
              if(mydata[witch[1],Salience]> 1.0000001){
                warning('Use of "SUM" has resulted in subjects wih salience greater than one. Consider returning to "Calculate Salience" and using normalised Salience instead.')
              }
            }
          }
          
        }
      }
    
      SalienceByCode[which(SalienceByCode$CODE==iii), "MeanSalience"]<- mean(mydata[which(mydata[,CODE]==iii), Salience],na.rm=T)
      SalienceByCode[which(SalienceByCode$CODE==iii), "SumSalience"]<- sum(mydata[which(mydata[,CODE]==iii), Salience],na.rm=T)
      SalienceByCode[which(SalienceByCode$CODE==iii), "SmithsS"]<- SalienceByCode[which(SalienceByCode$CODE==iii), "SumSalience"]/length(unique(mydata[,Subj]))  
  }
  
  
  return(SalienceByCode)
}
