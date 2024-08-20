#' Triadtest
#' Convert triad test data to a similarity matrix
#' @alias TriadTest
#' @usage triad.test(data)
#' @examples
#' @param dat A data set where three columns--denoted by ``item1'', ``item2'', and ``item3'' are the triads of the test. Other columns including participants should be called ``part1'', ``part2'', ``part3'', until ``partn''. The function identifies triad content and participants this way.
#' @return Upon running this function, you'll get a similarity matrix of triad decisions. This should be converted to a dissimilarity matrix before analysis.
#' @author Benjamin Grant Purzycki. <bgpurzycki@@alumni.ubc.ca>
#' @author Aaron Lightner. <alightner2@@gmail.com>
#' data(RVSfaculty)
#' d.diff <- triad.test(RVSfaculty) # similarity matrix
#' d.sim <- as.dist(1 - d.diff) # dissimilarity matrix
#' plot(hclust(d.sim), main = NULL)
#' @export
#' @examples
#' data(RVSfaculty)
#' d.diff <- triad.test(RVSfaculty) # similarity matrix
#' d.sim <- as.dist(1 - d.diff) # dissimilarity matrix
#' plot(hclust(d.sim), main = NULL)
#' 

triad.test <- function(dat) {
  item_cols = grep("item", tolower(colnames(dat)))
  items = unique(unlist(dat[item_cols]))
  combo = as.matrix(dat[item_cols])
  
  sim_matrix = matrix(0, nrow = length(items), ncol = length(items))
  rownames(sim_matrix) <- items
  colnames(sim_matrix) <- items
  key = t(combn(items, 2))
  
  cols = grep("part", tolower(colnames(dat)))
  
  totals = matrix(0, nrow = nrow(key), ncol = 2)
  colnames(totals) = c("numer", "denom")
  
  for(i in cols) {   
    odd = as.numeric(unlist(dat[i]))
    sim_pairs = rbind(combo[odd == 1, 2:3], combo[odd == 2, c(1, 3)], combo[odd == 3, 1:2])
    
    for(j in 1:nrow(key)) {
      pair_calc = j
      totals[j,"denom"] = totals[j,"denom"] + sum(apply(combo, MARGIN = 1, FUN = function(x) key[pair_calc, 1] %in% x & key[pair_calc, 2] %in% x))
      totals[j ,"numer"] = totals[j ,"numer"] + sum(apply(sim_pairs, MARGIN = 1, FUN = function(x) key[pair_calc, 1] %in% x & key[pair_calc, 2] %in% x))
    }
  }
  
  for(i in 1:nrow(key)) {
    sim_matrix[key[i,1], key[i,2]] <- totals[i,"numer"] / totals[i,"denom"]
    sim_matrix[key[i,2], key[i,1]] <- totals[i,"numer"] / totals[i,"denom"]
  }
  return(sim_matrix)
}
