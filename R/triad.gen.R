#' TriadGenerator
#' Create triad test instrument
#' @alias TriadGenerator
#' @usage triad.gen(items)
#' @examples
#' @param dat Make a triad test instrument with only a vector of items.
#' @return The output of this function is a randomize ready-to-use triad test.
#' @author Benjamin Grant Purzycki. <bgpurzycki@@cas.au.dk>
#' @export
#' @examples
#' RVS <- c("Ben", "Uffe", "Jesper", "Lene", "Jorn", "Marianne Q.-F.", "Thomas", "Martin", "Mark") # your items
#' triadmat <- triad.gen(RVS)
#' View(triadmat)
#' 

triad.gen <- function(items){
  triadlist <- data.frame(t(combn(items, 3))) # all possible triad combs
  triadlist$NUM <- seq(1, nrow(triadlist), 1) # assign triad a number
  triadlist <- triadlist[, c(4, 1, 2, 3)] # rearrange variable order
  triadlist$rand <- sample(1:nrow(triadlist), nrow(triadlist), replace = F)
  triadlist <- triadlist[order(triadlist$rand),] # randomize!
  return(triadlist)
}
