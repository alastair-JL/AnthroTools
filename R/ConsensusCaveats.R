#' @name ConsensusCaveats
#' @title Consensus Analysis: warnings and caveats  
#' @description When using the \code{\link{ConsensusPipeline}}, there are a few warnings and caveats to keep in mind. Firstly, the code is still in beta. Secondly, the mathematical technique we implement here, Consensus analysis, is in some sense still in beta. 
#'
#'  Consensus analysis relies on solid Baysian reasoning to calculate the probability of each answer given the competence data,
#'  competence is only found APPROXIMATELY- and with a large number of assumptions tacked on. These are reasonable assumptions, with a good approximation method, but the approximations none the less.
#'  It is possible that the competence approximation found using Comrey iteration will give values above one or below zero. Mathematically it is unclear what this means; in practice the programme simply
#'  forces all competence values within the range [0,1]. From a theoretical point of view, it is unclear what this will do exactly. 
#'  
#'  Another issue is that the iteration method used to calculate competence may not even give ANY answer. While experience suggests this is rare, 
#'  it can happen. If this occurs, your function will happily close itself down and give an appropriate error messaage.
#'  
#'  It would also be nice to have some automated way for R to determine if the assumptions of the model have
#'  been violated (in particular, the "single subject of interest" assumption.) The "Usual" rule of thumb for this is to
#'  check that the ratio of the first and second factor when determining competence is over three, and as such these numbers are reported by the programe.
#'  We emphisize here the use of ``factor'' rather than ``Eigenvalue''. While the factors found using Comrey itteration to find minimal residuals are
#'  like Eigenvalues in many ways, they are not in fact the same thing (see ``example(ConsensusCaveats)''). 
#'  The use of the word Eigenvector and Eigenvalue in the literature appears to be widespread (dating back to Romney et al's original 1986 paper)
#'  but (it is suspected), technically incorrect. Given the use of the minimum residual method, it seems factor magnitudes are a more approrpiate measure 
#'  (And, potentially the measure most commonly used, even if a different word is used). 
#'  
#'  Unfortunately, simulation and experimentation suggests that the factor ratio is not in fact a good
#'  indicator of whether the assumptions of the model have been satisfied- in particular, when
#'  simulating data that does match all assumptions, the 3:1 ratio is rarely satisfied.
#'  
#'  Finally, we note that, in our own experimentation using \code{\link{ConsensusStressTest}}, the function consistantly OVERESTIMATES its own
#'  error rate- usually by a factor of three or four. While this is in some sense better than having it underestimate
#'  its own error rate, it is strong evidence that our understanding of the algorithm is not complete. This also, we would like to address.
#'  
#'  Research suggest more recent methods, using fully baysian reasoning exist- in particular (Oravecz, 2014) and (Anders, 2014).
#'  Our own experience suggests that the computational time involved in using either of these tools is significantly higher, but
#'  both rest on fewer assumptions and approximations than the methods given here. (And yes, we were both pleased 
#'  and disappointed to discover such tools already exist. Oh well.)
#' 
#' @references 
#' \itemize{
#' \item Weller, S. (2007) Cultural Consensus Theory:Applications and Frequently Asked Questions   
#' \item Oravecz, Z., Vandekerckhove, J., & Batchelder, W. H. (2014). Bayesian Cultural Consensus Theory. Field Methods, 1525822X13520280. http://doi.org/10.1177/1525822X13520280     
#' \item Anders, R. 2014 "CCTPack: Cultural Consensus TheoryApplications to Data" R package Version 1.4.  
#'  \item Romney, A. K., Weller, S. C., & Batchelder, W. H. (1986). Culture as Consensus: A Theory of Culture and Informant Accuracy. American Anthropologist, 88(2), 313-338.  
#'  }
#' @author Alastair Jamieson Lane. <aja107@@math.ubc.ca>
#' @author Benjamin Grant Purzycki. <bgpurzycki@@alumni.ubc.ca>
#' 
#' @examples
#' x<-runif(5,0,1) ##some artificial competence levels.
#' y<-0.1*runif(5,-2,2)  ##some secondary interference term.
#' M<-x %*% t(x)+ y %*% t(y)
#' M[1,1]<-1 
#' M[2,2]<-1
#' M[3,3]<-1 ##Everyone always agrees with themselves.
#' M[4,4]<-1
#' M[5,5]<-1
#' result<-ComreySolve(M,precision=0.000001)
#' print(paste("Factor ratio",result$ratio))
#' eigs<-eigen(M)
#' print(paste("Eigenvalue Ratio",eigs[[1]][1]/eigs[[1]][2]))
#' x ##The true value of x
#' result$main ##Comrey solves estimate of x
#' eigs[[2]][1,] ##The primary eigenvector.
#' y ## the true interference vector
#' result$second ##Comrey solves estimate of y - often a bit off, but usually right magnitude.
#' ##Note that the estimate of y is frequently not so great- this indicates that the ratio test is also limited.
#' 
NULL