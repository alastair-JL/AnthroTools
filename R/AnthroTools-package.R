#' @name AnthroTools
#' @title AnthroTools
#' @docType package
#' @description
#' A couple statistical tools for cultural and cognitive anthropological research. In particular, some tools for Salience Analysis, and some functions for Consensus Analysis.
#' 
#' @details
#' \tabular{ll}{
#' Package: \tab AnthroTools\cr
#' Type: \tab Package\cr
#' Version: \tab 0.6\cr
#' Date: \tab 2014-23-10\cr
#' License: \tab GPL 2\cr
#' }
#' @note If you wish to do salience analysis, the most important function will be \code{\link{CalculateSalience}}, probably followed by \code{\link{SalienceByCode}}. If you want to do consensus analysis, we recommend \code{\link{ConsensusPipeline}}. For examples of the data structures the functions would like as input please looke at \code{\link{FruitList}} and \code{\link{ConsensusTestData}}. \code{\link{FreelistTable}} is also a useful function.
#' 
#' @keywords package
#' @examples
#' data(FruitList)
#' test<- FruitList
#' test<- CalculateSalience(test)
#' SalienceByCode(test,dealWithDoubles="MAX")
#' 
#' FakeData<- GenerateConsensusData(8,8,4)
#' Survey <- FakeData$Survey
#' ConsensusResult <- ConsensusPipeline(Survey,4)
#' View(ConsensusResult$Answers)
#' View(FakeData$Answers)
#' @references 
#' Oravecz, Z., Vandekerckhove, J., & Batchelder, W. H. (2014). Bayesian Cultural Consensus Theory. Field Methods, 1525822X13520280. http://doi.org/10.1177/1525822X13520280 
#' @references 
#' Romney, A. K., Weller, S. C., & Batchelder, W. H. (1986). Culture as Consensus: A Theory of Culture and Informant Accuracy. American Anthropologist, 88(2), 313-338.
#' @references 
#' Smith, J. J., & Borgatti, S. P. (1997). Salience Counts-And So Does Accuracy: Correcting and Updating a Measure for Free-List-Item Salience. Journal of Linguistic Anthropology, 7(2), 208-209. http://doi.org/10.1525/jlin.1997.7.2.208
#' @references 
#' Smith, J. J., Furbee, L., Maynard, K., Quick, S., & Ross, L. (1995). Salience Counts: A Domain Analysis of English Color Terms. Journal of Linguistic Anthropology, 5(2), 203-216. http://doi.org/10.1525/jlin.1995.5.2.203.
#' 
#' @author Alastair Jamieson Lane. <aja107@@math.ubc.ca>
#' @author Benjamin Grant Purzycki. <bgpurzycki@@alumni.ubc.ca>
NULL