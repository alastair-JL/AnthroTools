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
#' @param dealWithDoubles How you would like the function to deal with cases where a single survey respondent gave the same answer multiple times. On the default setting, the function will assume that no such cases arise, and will throw an error if it encounters such a case. Aside from DEFAULT, you also have the options MAX, SUM and IGNORE. MAX indicates that for each code, you want the computer to note the first time a respondent lists a particular CODE, and ignore subsequent mentions. For SUM, you are asking the computer to determine each respondent's TOTAL salience with respect to a given code, while MEAN calculates their average salience (This option is considered unusual, on the grounds that mentioning additional times on the list will actively reduce its mean salience. This seems counter to the intention here, but is still an avaliable option). IGNORE tells the computer not to think about it, and is merely a way of suppressing errors. IGNORE is not recommended.
#' @keywords FreeList
#' @return Upon running this function, you will receive a data frame, where each row is one of you possible response codes, and each column contains a different statistic about the code. MeanSalience is the average salience of a code, averaged over the number of responses that referenced that code. Sum Salience just gives the total amount of salience of a given code, and SmithsS gives this sum, divided by the total number of respondents.
#' @references Quinlan, M. (2005). Considerations for collecting freelists in the field: Examples from ethobotany. Field Methods, 17(3), 219-234. http://doi.org/10.1177/1525822X05277460 
#' @export
#' @author Alastair Jamieson Lane. <aja107@@math.ubc.ca>
#' @author Benjamin Grant Purzycki. <bgpurzycki@@cas.au.dk>
#' @examples
#' data(FruitList)
#' test<- CalculateSalience(FruitList)
#' SalienceByCode(test,dealWithDoubles="MAX")
#' 
#' data(WorldList)
#' test<- CalculateSalience(WorldList,GROUPING="GROUPING")
#' SalienceByCode(test,dealWithDoubles="MAX",GROUPING="GROUPING")
#' 
#' 
SalienceByCode <-
function(mydata,CODE="CODE",Salience="Salience", Subj="Subj", dealWithDoubles="DEFAULT",GROUPING=NA){  
  sumWarn<-F
  
  if(!(CODE %in% colnames(mydata))){    
    stop('Specified "CODE" column not valid.')
  }
  
  if(!(Salience %in% colnames(mydata))){    
    stop('Specified "Salience" column not valid.')
  }
  
      
  if(any(mydata[, Salience]==0) ){
    warning("Some of the Salience data is equal to zero, probably because those rows have an NA code. These rows we are going to remove before doing our analysis.")
    mydata<- mydata[-which(mydata[, Salience]==0),]    
    print(unique(mydata[,CODE]))
  } 
  
  if(any(is.na(mydata[, CODE]) ) ){
    warning("Some of the CODEs are NA. These have been removed..")
    mydata<- mydata[-which(is.na(mydata[, CODE]) ),]    
    print(unique(mydata[,CODE]))
  } 
  
  
  if(!dealWithDoubles %in% c("DEFAULT" ,"IGNORE" ,"MAX","SUM","MEAN") ) {
    stop(' dealWithDoubles must take a value of "DEFAULT","MAX", "SUM","MEAN" or "IGNORE". ')    
  }
  
  
  if(!is.na(GROUPING)) {
    if(!(GROUPING %in% colnames(mydata))){    
      stop('Specified "GROUPING" column not valid.')
    }

    L <- list(unique(mydata[,GROUPING]),unique(mydata[,CODE]))
    L.pairs <- combn(seq_along(L), 2, simplify = FALSE, FUN = function(i)L[i])    
    L.Hand.On<-do.call(rbind, lapply(L.pairs, expand.grid))    
    SalienceByCode <- data.frame(GROUPING=L.Hand.On[,1],CODE=L.Hand.On[,2],
                                 MeanSalience= 0, ##The mean of intensity GIVEN that a code showed up.
                                 SumSalience=0,  ## the SUM of all intensities for a given code.
                                 SmithsS=0  ## the SUM of all intensities for a given code.
    ) 
    
    
    for( ggg in unique(SalienceByCode$GROUPING)){              
      numInGroup<-length(unique(mydata[which(mydata[,GROUPING]==ggg) ,Subj]))      
    for( iii in unique(SalienceByCode$CODE)){  
  #    SutropCount=0;
   #   sutropRankSum=0;
      
      if( anyDuplicated(mydata[which(mydata[,CODE]==iii & mydata[,GROUPING]==ggg), Subj])>0 & dealWithDoubles=="DEFAULT" ) {
        stop('Some subjects have multiple entries with same code. Set "dealWithDoubles" to "SUM", "MAX" or "IGNORE" to deal with this.')      
      }
      print(iii)
      if(dealWithDoubles=="MAX" || dealWithDoubles=="SUM"||dealWithDoubles=="MEAN" ){
        if(dealWithDoubles=="MAX"){
          doThing<-max
        }else if(dealWithDoubles=="SUM"){
          doThing<-sum
        }else{
          doThing<-mean          
        }          
        for( jjj in unique(mydata[,Subj])){    
          witch<-which((mydata[,CODE]==iii & mydata[,GROUPING]==ggg) & mydata[,Subj]==jjj) # 
          
          if (length(witch)>1){            
            mydata[witch[1],Salience]<-doThing(mydata[witch,Salience])
            mydata[witch[-1],Salience]<-NA
            if(!is.na(mydata[witch[1],Salience])){
              if(mydata[witch[1],Salience]> 1.0000001){
                sumWarn<-T                
              }
            }
          }
          
        }        
      }
            SalienceByCode[which(SalienceByCode[,"CODE"]==iii &SalienceByCode[,"GROUPING"]==ggg), "MeanSalience"]<- mean(mydata[which(mydata[,CODE]==iii & mydata[,GROUPING]==ggg), Salience],na.rm=T)
            SalienceByCode[which(SalienceByCode[,"CODE"]==iii &SalienceByCode[,"GROUPING"]==ggg), "SumSalience"]<- sum(mydata[which(mydata[,CODE]==iii & mydata[,GROUPING]==ggg), Salience],na.rm=T)  
            SalienceByCode[which(SalienceByCode[,"CODE"]==iii &SalienceByCode[,"GROUPING"]==ggg), "SmithsS"]<- SalienceByCode[which(SalienceByCode$CODE==iii & SalienceByCode$GROUPING==ggg), "SumSalience"]/numInGroup            
    } ##End CODE For loop.
    } ##End Group For Loop.
  
  }else{ 
  
  SalienceByCode <- data.frame(CODE=unique(mydata[,CODE]),
                               MeanSalience= 0, ##The mean of intensity GIVEN that a code showed up.
                               SumSalience=0,  ## the SUM of all intensities for a given code.
                               SmithsS=0  ## the SUM of all intensities for a given code.
  ) 
  
    
  for( iii in unique(SalienceByCode$CODE)){      
    if( anyDuplicated(mydata[which(mydata[,CODE]==iii), Subj])>0 & dealWithDoubles=="DEFAULT" ) {
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
          
          witch<-which(mydata[,CODE]==iii & mydata[,Subj]==jjj) #           
          if (length(witch)>1){
            mydata[witch[1],Salience]<-doThing(mydata[witch,Salience])
            mydata[witch[-1],Salience]<-NA
            if(!is.na(mydata[witch[1],Salience])){
              if(mydata[witch[1],Salience]> 1.0000001){
                sumWarn<-T
              }
            }
          }
          
        }
      }
    
      SalienceByCode[which(SalienceByCode$CODE==iii), "MeanSalience"]<- mean(mydata[which(mydata[,CODE]==iii), Salience],na.rm=T)
      SalienceByCode[which(SalienceByCode$CODE==iii), "SumSalience"]<- sum(mydata[which(mydata[,CODE]==iii), Salience],na.rm=T)
      SalienceByCode[which(SalienceByCode$CODE==iii), "SmithsS"]<- SalienceByCode[which(SalienceByCode$CODE==iii), "SumSalience"]/length(unique(mydata[,Subj]))  
    } ##End Subj For loop.
  }## END "If Grouping is NA.
  
  
    print(SalienceByCode)
  
  if(sumWarn){
    warning('Use of "SUM" has resulted in subjects wih salience greater than one. Consider returning to "Calculate Salience" and using rescaled Salience instead.')
  }
  
  return(SalienceByCode)
}
