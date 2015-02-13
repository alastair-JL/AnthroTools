#' @name AnthroTools
#' @title AnthroTools- some statistical tools for Dr Ben.
#' @docType package
#' @description
#' A couple statistacal tools for Dr Ben, and anyone else who wants to use them. In particular, some tools for Salience analysis, and some functions for Consensus analysis. Still in early development.
#' 
#' @details
#' \tabular{ll}{
#' Package: \tab AnthroTools\cr
#' Type: \tab Package\cr
#' Version: \tab 0.6\cr
#' Date: \tab 2014-23-10\cr
#' License: \tab GPL 2\cr
#' }
#' @note If you wish to do salience analysis, the most important function will be \code{\link{CalculateSalience}}, probably followed by \code{\link{SalienceByCode}}. If you want to do consensus analysis, might I recommend \code{\link{ConsensusPipeline}}.
#' 
#' @keywords package
#' @examples
#' test<- data(FruitList)
#' test<- CalculateSalience(test)
#' SalienceByCode(test,dealWithDoubles="MAX")
#' 
#' FakeData<- GenerateConsensusData(8,8,4)
#' Survey <- FakeData$Survey
#' ConsensusResult <- ConsensusPipeline(Survey,4)
#' View(ConsensusResult$Answers)
#' View(FakeData$Answers)
#' @references 
#' Culture as consensus: a Theory of Culture and Informant Accuracy
#' A. Kimball Romney, Susan C. Weller, William H. Batchelder.
#' American Anthropologist, New Series, Volume 88, No 2 (Jun 1986) pp 313-338
#' 
#' @author Alastair Jamiesone Lane. <aja107@@math.ubc.ca>
NULL