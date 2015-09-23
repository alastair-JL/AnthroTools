#' FreeListTable
#'
#' Given a Free list data frame (with columns for subject number, order of responses and response code), produce a table comparing each subjects responses to each code. This table can be a simple "presence/absence" table for determining if a subject mentioned a particular code, or something more complex.
#' 
#' @usage FreeListTable(mydata, CODE = "CODE", Salience = "Salience", Subj = "Subj", tableType = "DEFAULT")
#' @param mydata This is your free-list data, stored as a data frame. 
#' @param CODE The name of the column in which your "CODE" (eg, the subjects' individual responses) is stored.
#' @param Salience The name of the column in which each responses Salience is stored. If you wish to ask questions of salience, it is important you calculate salience before using this function. (using \code{\link{CalculateSalience}}).
#' @param Subj The name of the column where your subjects names/subject numbers are stored.
#' @param tableType What type of data do you want in this table? Currently there are four types: "PRESENCE", "SUM_SALIENCE","MAX_ENCE" and "FREQUENCY". Using "PRESENCE" an entry will be one if the specified subject mentioned the specified code. It will be zero otherwise. If you specify Frequency, then you will get a count of how often each code was mentioned by each person. If you use "SUM_SALIENCE" then you will get the total salience each person has associated with each code. If you use "MAX_SALIENCE" then you will get the maximum salience, I.E. the salience of the code the first time it was mentioned.
#' @keywords FreeList
#' @return The value returned is a data frame, where each row represents a subject, and each column represents one of your free-list Codes. Depending on "tabeType" the entries of the dataframe will either represent different things.
#' @author Alastair Jamieson Lane. <aja107@@math.ubc.ca>
#' @author Benjamin Grant Purzycki. <bgpurzycki@@alumni.ubc.ca>
#' @note Ben, what other types of tables could be useful here? 
#' @export
#' @examples
#' fakeData<- GenerateFakeFreeListData() 
#' table<- FreeListTable(fakeData, tableType="PRESENCE")
#' View(table)
#' 
FreeListTable <-
function(mydata,CODE="CODE",Salience="Salience", Subj="Subj", tableType="DEFAULT"){      
  tableType=toupper(tableType)
  switch(tableType,
         "PRESENCE"={
           doThing<-FreeListTable.Present 
         },
         "MAX_SALIENCE"={
           doThing<-FreeListTable.MaxSal
         },
         "SUM_SALIENCE"={
           doThing<-FreeListTable.SumSal
         },         
         "FREQUENCY"={
           doThing<-FreeListTable.freq
         },         
         stop(' tableType must take a value of "PRESENCE","SUM_SALIENCE","FREQUENCY" 
or "MAX_SALIENCE". For example: FreeListTable(mydata, tableType="PRESENCE") ')             
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
