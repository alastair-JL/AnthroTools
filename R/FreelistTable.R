#' A Cat Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()
#' 
FreelistTable <-
function(mydata,CODE="CODE",Salience="Salience", Subj="Subj", tableType="DEFAULT"){      
  switch(tableType,
         "PRESENCE"={
           doThing<-FreelistTable.Present 
         },
         "MAX_SALIENCE"={
           doThing<-FreelistTable.MaxSal
         },
         "SUM_SALIENCE"={
           doThing<-FreelistTable.SumSal
         },         
         "FREQUENCY"={
           doThing<-FreelistTable.freq
         },         
         stop(' tableType must take a value of "PRESENCE","SUM_SALIENCE","FREQUENCY" or "MAX_SALIENCE". ')             
  )
  
  
    
  subjList<- unique(mydata[,Subj])
  CODEList<- unique(mydata[,CODE])
  df <- data.frame(matrix(ncol = length(CODEList), nrow = length(subjList)))
  colnames(df)<-CODEList
  rownames(df)<-subjList
  for( iii in subjList){
    for( jjj in CODEList){
      df[iii,jjj]<-doThing(mydata,CODE=CODE,Salience=Salience,Subj=Subj,subjNum=iii,CODEnum=jjj)      
    }
  }
return(df)
}
