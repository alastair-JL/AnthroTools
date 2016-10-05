#' FreeListTable
#' 
#' Given a Free list data frame (with columns for subject number, order of responses and response code), produce a table comparing each subjects responses to each code. This table can be a simple "presence/absence" table for determining if a subject mentioned a particular code, or something more complex. \code{\link{colSums}} can be applied in order to get summaries (as opposed to person by person breakdown).
#' @usage FreeListTable(mydata, CODE = "CODE", Salience = "Salience", Subj = "Subj", tableType = "DEFAULT")
#' @param mydata This is your free-list data, stored as a data frame. 
#' @param CODE The name of the column in which your "CODE" (eg, the subjects' individual responses) is stored.
#' @param Salience The name of the column in which each responses Salience is stored. If you wish to ask questions of salience, it is important you calculate salience before using this function. (using \code{\link{CalculateSalience}}).
#' @param Subj The name of the column where your subjects names/subject numbers are stored.
#' @param GROUPING The name of the column where your subjects group names are sorted. Helps distinguish between individuals from different groups with the same ID number.
#' @param tableType Currently there are five types of tables: PRESENCE, SUM_SALIENCE, MAX_SALIENCE,HIGHEST_RANKING and FREQUENCY. PRESENCE will give a "1" if the specified code is present, or "0" otherwise. If you specify FREQUENCY, then you will get a count of how often each code was mentioned by each person. SUM_SALIENCE and MAX_SALIENCE give (respectively) the total salience an individual has assigned to a particular code, and the highest salience value associated with that code. HIGHEST_RANK gives the lowest number in the order column (and is infered from salience). 
#' @keywords FreeList
#' @return The value returned is a data frame, where each row represents a subject, and each column (bar the first one or two) represents one of your free-list Codes. Depending on "tableType" the entries of the dataframe will either represent different things.
#' @author Alastair Jamieson Lane. <aja107@@math.ubc.ca>
#' @author Benjamin Grant Purzycki. <bgpurzycki@@alumni.ubc.ca>
#' @export
#' @examples
#' fakeData<- GenerateFakeFreeListData() 
#' table<- FreeListTable(fakeData, tableType="PRESENCE")
#' View(table)
#' colSums(table) ##This will give summarised results.
#' data(WorldList)
#' FreeListTable(WorldList, tableType="FREQUENCY")
#' FreeListTable(WorldList, tableType="FREQUENCY",GROUPING="GROUPING")
#' WorldList<-CalculateSalience(WorldList,GROUPING="GROUPING")
#' FreeListTable(WorldList, tableType="SUM_SALIENCE",GROUPING="GROUPING")
#' 
FreeListTable <-
function(mydata,CODE="CODE",Salience="Salience", Subj="Subj", tableType="DEFAULT",GROUPING=NA){      
  tableType=toupper(tableType)
  switch(tableType,
         "PRESENCE"={
           doThing<-FreeListTable.Present 
         },
         "MAX_SALIENCE"={
           doThing<-FreeListTable.MaxSal
           if(!(Salience %in% colnames(mydata)))
           {
             warning("Given Salience column not found. Salience Calculated using function defaults")
             mydata<-CalculateSalience(mydata,Salience=Salience,Subj=Subj, CODE=CODE)
           }
         },
         "SUM_SALIENCE"={
           doThing<-FreeListTable.SumSal
           if(!(Salience %in% colnames(mydata)))
           {
             warning("Given Salience column not found. Salience calculated using function defaults.")
             mydata<-CalculateSalience(mydata,Salience=Salience,Subj=Subj, CODE=CODE)
           }
         },         
         "FREQUENCY"={
           doThing<-FreeListTable.freq
         },
         "HIGHEST_RANK"={
           doThing<-FreeListTable.Rank
         },         
         stop(' tableType must take a value of "PRESENCE","SUM_SALIENCE","FREQUENCY" ,"HIGHEST_RANK"
or "MAX_SALIENCE". For example: FreeListTable(mydata, tableType="PRESENCE") ')             
  )
  grpYes<-FALSE;
  if (!is.na(GROUPING)){    
    if(!(GROUPING %in% colnames(mydata))){    
      stop('Specified "GROUPING" column not valid.')
    }
    mydata$SubjGrp<-paste(mydata[,GROUPING],mydata[,Subj])              
    grpYes<-TRUE;
  }else{
    mydata$SubjGrp<-mydata[,Subj]
  }
  
    
  subjList<- unique(mydata[,"SubjGrp"])
    
  CODEList<- unique(mydata[,CODE])  
  df <- data.frame(matrix(ncol = (1+grpYes), nrow = length(subjList)))
  
  if(grpYes){
    colnames(df)<-c("Subject","Group")
  }else{
    colnames(df)<-c("Subject")
  }  
  
  
  rownames(df)<-subjList
  for( iii in as.character(subjList)){
    for( jjj in CODEList){
      df[iii,jjj]<-doThing(mydata,CODE=CODE,Salience=Salience,Subj="SubjGrp",subjNum=iii,CODEnum=jjj)            
    }
    df[iii,"Subject"]<-  mydata[which(mydata$SubjGrp==iii)[1],Subj]
    if(grpYes){
      df[iii,"Group"]<-  mydata[which(mydata$SubjGrp==iii)[1],GROUPING]
    }
    
  }
  
return(df)
}
