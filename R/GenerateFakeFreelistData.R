#' A Cat Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()

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
