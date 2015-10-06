#' GenerateFakeFreeListData
#'
#' This function generates fake free-list data- both to act as a template, and incase anyone needs to run some sort of statistical analysis. 
#' 
#' @usage GenerateFakeFreeListData()
#' @keywords FreeList
#' @export
#' @return 
#' The function returns a single data frame, with columns "Subj",
#'  "Order" and "CODE". These column names are compatible with the 
#'  default settings for the other FreeList related functions in this
#'   package. Subj specifies the subject number, CODE lists the subject's responses to a free-listing question (in this case "List whatever fruit you can think of") and Order specifies what order each subject listed their items in. The data created is entirely fictitious, and no attempt is made to prevent the imaginary subjects from listing the same fruit multiple times.
#' @author Alastair Jamieson Lane. <aja107@@math.ubc.ca>
#' @author Benjamin Grant Purzycki. <bgpurzycki@@alumni.ubc.ca>
#' @examples
#' fakeData<-GenerateFakeFreeListData()
#' View(fakeData)
#' mainlandData<-GenerateFakeFreeListData()
#' mainlandData$GROUPING="MAINLAND"
#' islandData<-GenerateFakeFreeListData()
#' islandData$GROUPING="ISLAND"
#' moonData<-GenerateFakeFreeListData()
#' moonData$GROUPING="MOON"
#' fullData<-rbind(mainlandData,islandData,moonData) ##This method gives data with distinct groupings.
#' 
#' @note
#' Depending on what is needed oruseful, we may implement a version of this function that can take some sort of statistical input (average length of peoples lists or such) and creates fake data with a particular shape. 
#' This might be most easily implemented by having it use an existing data set and then mashing it up in some way so as to create a fake data set- allowing the user to compare their data to the usual in some way.
#' Doing this would require some amount of research, and thus I would only really want to do so if there were enough interest in it.


GenerateFakeFreeListData <-
function(){
  listOfThings<- c("apple","banana", "apple","orange","peach","lemon","apple","plum","strawberry","pear")
  is=NULL
  js=NULL
  cs=NULL
  for(iii in 1:sample(10:20, 1) ){
    for(jjj in 1:sample(2:5, 1) ){      
      is<- c(is,iii)
      js<- c(js,jjj)
      cs<- c(cs,sample(listOfThings,1)) 
    }                
  }          
  myData <- data.frame(is,js,cs)  
 colnames(myData)<-c("Subj","Order","CODE")
  
  return(myData)  
}

mainlandData<-GenerateFakeFreeListData()
mainlandData$GROUPING="MAINLAND"
islandData<-GenerateFakeFreeListData()
islandData$GROUPING="ISLAND"
moonData<-GenerateFakeFreeListData()
moonData$GROUPING="MOON"
worldData<-rbind(mainlandData,islandData,moonData)
