#' LoadFromAnthropac
#'
#' Given the file name of a file prepared in the anthropac format, load it in into a AnthroTools style table.
#' 
#' @usage LoadFromAnthropac(filename)
#' @param filename This is the name of the file containing your anthropac data.
#' @keywords FreeList
#' @return A three column table containing the same data, stored in a format compatible with the AnthroTools package.
#' @author Alastair Jamieson Lane. <aja107@@math.ubc.ca>
#' @author Benjamin Grant Purzycki. <bgpurzycki@@cas.au.dk>
#' @export
#' 
LoadFromAnthropac <-
function(filename){      
  AnthroPac <- read.csv(filename, quote="\"",comment.char="%",header=F)
  Tabulated<- data.frame(subj=as.character(levels(AnthroPac[,])),ORDER=1,CODE=as.character(levels(AnthroPac[,])),stringsAsFactors=FALSE)
  Tabulated<-Tabulated[1,]  
  Order<-1
  CurrentSubj<-NULL  
  for(iii in 1:nrow(AnthroPac) ){
    if(substring(AnthroPac[iii,1] , 1, 1)=="#"){
      Tabulated[1,1]<- gsub("^\\s+|\\s+$", "",substring(AnthroPac[iii,1] , 2) ) ##Drop the initial #, then trim out leading and trailing spaces.
      Tabulated[1,2]<-1      
    }else{    
        Tabulated[1,3]<-as.character(AnthroPac[iii,1])
        Tabulated[nrow(Tabulated)+1,]<-Tabulated[1,]
        Tabulated[1,2]<-Tabulated[1,2]+1      
    }    
     
  }
  
  Tabulated<-Tabulated[-1,]    
  Tabulated<-data.frame(subj=Tabulated[,"subj"],ORDER=Tabulated[,"ORDER"],CODE=Tabulated[,"CODE"])
  #This line is to turn all the strings back into factors for reasonable handling later on.
  return(Tabulated)
}
