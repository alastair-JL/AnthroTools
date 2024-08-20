#' TriadGen
#' Create triad test instrument
#' @alias TriadGen
#' @usage TriadGen(items)
#' @examples
#' @param items This is a list of the items you'll use in your instrument.
#' @return The output of this function is a randomize ready-to-use triad test.
#' @author Benjamin Grant Purzycki. <bgpurzycki@@cas.au.dk>
#' RVS <- c("Ben", "Uffe", "Jesper", "Lene", "Jorn", "Marianne Q.-F.", "Thomas", "Martin", "Mark") # your items
#' triadmat <- TriadGen(RVS)
#' View(triadmat)
#' @export
#' @examples
#' RVS <- c("Ben", "Uffe", "Jesper", "Lene", "Jorn", "Marianne Q.-F.", "Thomas", "Martin", "Mark") # your items
#' triadmat <- TriadGen(RVS)
#' View(triadmat)
#' 

TriadGen <- function(items){
  triadlist <- data.frame(t(combn(items, 3))) # all possible triad combs
  triadlist$NUM <- seq(1, nrow(triadlist), 1) # assign triad a number
  triadlist <- triadlist[, c(4, 1, 2, 3)] # rearrange variable order
  triadlist$rand <- sample(1:nrow(triadlist), nrow(triadlist), replace = F)
  triadlist <- triadlist[order(triadlist$rand),] # randomize!
  return(triadlist)
}
