#' GenerateFakeFreelistData
#'
#' This function generates fake freelist data- both to act as a template, and incase anyone needs to run some sort of statistical analysis. 
#' 
#' @usage GenerateFakeFreelistData()
#' @keywords Freelist
#' @export
#' @return 
#' The Function returns a single data frame, with columns "Subj", "Order" and "CODE". These column names are compatible with the default settings for the other freelist related functions in this package.
#' Subj specifies the subject number, CODE lists the subjects responses to a freelisting question (In this case "List whatever fruit you can think of") and Order specifies what order each subject listed their items in. 
#' The data created is entirely fictitious, and no attempt is made to prevent the imaginary subjects from listing the same fruit multiple times.
#' @author Alastair Jamieson-Lane
#' @examples
#' fakeData<-GenerateFakeFreelistData()
#' View(fakeData)
#' @note
#' Depending on what is needed/useful, I may implement a version of this function that can take some sort of statistical input (average length of peoples lists or such) and creates fake data with a particular shape. 
#' This might be most easily implemented by having it use an existing data set and then mashing it up in some way so as to create a fake data set- allowing the user to compare their data to the usual in some way.
#' Doing this would require some amount of research, and thus I would only really want to do so if there were enough interest in it.


GenerateFakeFreelistData <-
function(){
  listOfThings<- c("apple","bannana", "apple","orange","peach","Lemon","apple","plum","strawberry","bannana","orange","pear")
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
