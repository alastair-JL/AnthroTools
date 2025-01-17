#' fstmatrix
#'
#' Calculate cultural FST across multiple groups
#' @usage   fstmatrix(d, matrixtype = NULL) 
#' @param d A matrix where the first column is sample size, and the second column is trait frequency. Ideally, the row names are group identifiers.
#' @param matrixtype If unspecified, this function will return a full group x group square matrix. If "upper", this returns an upper triangular matrix, "lower" returns a lower traiangular matrix.
#' @return This function returns the Cultural FST value across multiple groups.
#' @keywords fstmatrix
#' @note
#' @export
#' @examples
#' # across 8 groups, we have sample size, and total number of participants who listed things coded as "moral" when asked to list things that anger a locally-respected god
#' data(moralgod)
#' fstmatrix(moralgod)
#' fstdist <- as.dist(fstmatrix(moralgod, matrixtype = "lower"))
#' plot(hclust(fstdist), ylab = NA, xlab = NA, main = NA) # dendrogram

cultFST <- function(ni, nj, xi, xj){ 
  pi <- xi/ni
  pj <- xj/nj
  pbar <- (xi + xj)/(ni + nj)
  num <- (ni/(ni + nj))*(pi - pbar)^2 + (nj/(ni + nj))*(pj - pbar)^2
  denom <- pbar*(1 - pbar)
  FST <- num/denom
  return(FST)
}

fstmatrix <- function(d, matrixtype = NULL) {
  m <- matrix(NA, nrow = nrow(d), ncol = nrow(d))
  rownames(m) <- colnames(m) <- rownames(d)
  for(i in 1:nrow(m)) {
    for(j in 1:ncol(m)) {
      m[i, j] <- cultFST(d[i, 1], d[j, 1], d[i, 2], d[j, 2])   
    }
  }
  if(!is.null(matrixtype)) {
    if(matrixtype == "upper") {
      m[lower.tri(m)] <- 0
    }
    if(matrixtype == "lower") {
      m[upper.tri(m)] <- 0
    }
  }
  return(m)
}