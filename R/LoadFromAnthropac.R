#' LoadFromAnthropac
#'
#' Given the file name of a file prepared in the anthropac format, load it in into a AnthroTools style table.
#' 
#' @usage LoadFromAnthropac(filename)
#' @param filename This is the name of the file containing your anthropac data.
#' @keywords FreeList
#' @return A three column table containing the same data, stored in a format compatible with theAnthroTools package.
#' @author Alastair Jamieson Lane. <aja107@@math.ubc.ca>
#' @author Benjamin Grant Purzycki. <bgpurzycki@@alumni.ubc.ca>
#' @export
#' 
LoadFromAnthropac <-
function(filename){      
  AnthroPac <- read.table(filename, quote="\"", blank.lines.skip=FALSE)
  Tabulated<- data.frame(subj=levels(AnthroPac[,]),ORDER=1,CODE=levels(AnthroPac[,]))
  Tabulated<-Tabulated[1,]
  Mode<- 1
  Order<-1
  CurrentSubj<-NULL  
  for(iii in 1:nrow(AnthroPac) ){
    if(Mode==1){
      Tabulated[1,1]<-AnthroPac[iii,1]
      Tabulated[1,2]<-1
      Mode=2
    }else{
      if(AnthroPac[iii,1]==""){
        Mode=1
      }else{
        Tabulated[1,3]<-AnthroPac[iii,1]
        Tabulated[nrow(Tabulated)+1,]<-Tabulated[1,]
        Tabulated[1,2]<-Tabulated[1,2]+1
      }
    }    
    
  }
  
  Tabulated=Tabulated[-1,]    
  return(Tabulated)
}
