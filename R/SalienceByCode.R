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
    
      if(dealWithDoubles=="MAX" || dealWithDoubles=="SUM" ){
        if(dealWithDoubles=="MAX"){
          doThing<-max
        }else{
          doThing<-sum
        }        
        for( jjj in unique(mydata[,Subj])){    
          witch<-which(mydata[,CODE]==iii & mydata[,Subj]==jjj ) #           
          if (length(witch)>1){            
            mydata[witch[1],Salience]<-doThing(mydata[witch,Salience])
            mydata[witch[-1],Salience]<-NA
            if(!is.na(mydata[witch[1],Salience])){
              if(mydata[witch[1],Salience]> 1.0000001){
                warning('Use of "SUM" has resulted in subjects wih salience greater than one. Consider using normalised Salience.')
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
