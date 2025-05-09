#' @name AnthroTools
#' @title AnthroTools
#' @docType package
#' @description
#' AnthroTools: Some custom tools for cultural anthropology.
#' 
#' @details
#' \tabular{ll}{
#' Package: \tab AnthroTools\cr
#' Type: \tab Package\cr
#' Version: \tab 2.0\cr
#' Date: \tab 2025-30-04\cr
#' License: \tab GPL 2\cr
#' }
#' 
#' @section Freelist Analysis:
#' Freelist analysis allows you to collect answers to such questions as "What fruit can you think of?", and analyse what answers are most salient (come to mind first). 
#' If you wish to do salience analysis, the most important function will be \code{\link{CalculateSalience}}, probably followed by \code{\link{SalienceByCode}}. For examples of the data structures the functions would like as input please look at \code{\link{FruitList}}, or generate fake data of your own using \code{\link{GenerateFakeFreeListData}}. \code{\link{FreeListTable}} is also a useful function.
#' If your data was previously prepared for Anthropac, then \code{\link{LoadFromAnthropac}} provides compatibility.
#' 
#' @section Consensus Analysis:
#' Consensus analysis lets you collect a group's answers to multi choice questions (all related to a single topic). By first estimating the competence of each person, the method will than allow you to calculate the ``consensus response''.
#' If you want to do consensus analysis, we recommend \code{\link{ConsensusPipeline}}. For examples of the data structures the functions would like as input please look at \code{\link{ConsensusTestData}}, or generate your own data using \code{\link{GenerateConsensusData}}
#' 
#' @section Triad Test:
#' Triad tests examine (dis)similarities of all triplet-combinations of a wordbank (e.g., "For each triplet, please select the one that is the least like the others"). Check out the walkthrough of a real example using \code{\link{triad.test}}.
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
#' @author Alastair Jamieson Lane. <aja107@@math.ubc.ca>  Benjamin Grant Purzycki. <bgpurzycki@@cas.au.dk>
NULL
