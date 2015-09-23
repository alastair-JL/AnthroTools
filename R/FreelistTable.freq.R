#' FreeListTable._______
#'
#' This is a collection of helper functions for the function \code{\link{FreeListTable}}, that determine the relationship between a particular subject, and a particular code. For example, FreeListTable.freq counts how often an individual has responded with the specified Code. They are not intended for individual use.
#' @aliases FreeListTable.freq FreeListTable.MaxSal FreeListTable.Present FreeListTable.SumSal
#' @keywords FreeList
#' @usage FreeListTable.freq(mydata, CODE = "CODE", Salience = "Salience", Subj = "Subj", subjNum, CODEnum)
#' @param mydata Your data, as a data frame, with rows for each response gathered. For each response, you are expected to have the CODE, salience and Subj.
#' @param CODE The Column of "mydata" that the CODE of the response can be found in.
#' @param Salience The column of "mydata" that the calculated Salience is to be found in. Note that this will have been determined earlier using "CalculateSalience"
#' @param Subj The column of "mydata" in which the subject number can be found.
#' @param subjNum The subject number of interest.
#' @param CODEnum The code you desire information about.
#' @return A number, representing either: The presence/absence of the given code in the individuals responses (if "presence" was used). The total number of times the code was given in response (for "freq") The total Salience of a given code (summed over all responses with that code) The Maximum Salience (effectively the Salience levels when the Code was first used.)
#' @export
#' @author Alastair Jamieson Lane. <aja107@@math.ubc.ca>
#' @author Benjamin Grant Purzycki. <bgpurzycki@@alumni.ubc.ca>
#' @note If you are using these functions directly yourself, something has probably gone wrong.
#' @examples
#' data(FruitList)
#' test<- FruitList
#' FreeListTable.freq(test,subjNum=7,CODEnum="apple") 


FreeListTable.freq <-
function(mydata,CODE="CODE",Salience="Salience", Subj="Subj",subjNum,CODEnum){
  if(any(mydata[,Subj]==subjNum & mydata[,CODE]==CODEnum) ){    
    ret<- nrow(mydata[which(mydata[,Subj]==subjNum & mydata[,CODE]==CODEnum),])
    return(ret)
  }else{
    return(0)
  }  
}
