#' FlowerPlotIntervals
#' 
#' Plotting function to generate flower-plots of Smith's S estimates to top 8 items with highest cultural salience, adding uncertainty intervals into the 'petals'.
#' @usage FlowerPlotIntervals(SmithsS_noUncert, SmithsS_uncert, label, lower_int = "2.5%", upper_int = "97.5%")
#' @param SmithsS This is a dataset of Smith's S values for all items in the data set, normally used as input to the 'FlowerPlot()' command (i.e., the output of the 'SalienceByCode()' command). Note that the first column of this dataset must be the item codes. 
#' @param S_uncert This is a dataset summarising the uncertainty estimates across a range of items in the dataset (e.g., the output of the 'SalienceEstimateSummary()' command on the top 8 items with highest Smith's S values). Note that a minimum of 8 items is necessary for the flower-plot to display correctly.
#' @param label The label for the centre of the flower-plot.
#' @param lower_int The column name for the lower uncertainty interval in the 'S_uncert' object (default = "2.5%").
#' @param upper_int The column name for the upper uncertainty interval in the 'S_uncert' object (default = "97.5%").
#' @keywords FreeList, Smith's S, Cultural salience
#' @return A flowerplot with uncertainty estimates in the 'petals'.
#' @author Daniel Major-Smith. <dan.major-smith@@cas.au.dk>
#' @author Benjamin Grant Purzycki. <bgpurzycki@@cas.au.dk>
#' @export
#' @examples
#' ## Generate fake free-list data about fruits
#' set.seed(41)
#' fakeData <- GenerateFakeFreeListData() 
#' 
#' ## Calculate item salience
#' fakeData.s <- CalculateSalience(fakeData, Subj = "Subj", Order = "Order", CODE = "CODE", Salience = "CODE.S")
#' 
#' ## Convert to data frame with maximum item saliences for each item as separate rows, and including 0s
#' fakeData.sal0 <- FreeListTable(fakeData.s, Subj = "Subj", Order = "Order", CODE = "CODE", Salience = "CODE.S", tableType = "MAX_SALIENCE")
#' head(fakeData.sal0)
#' 
#' ## Calculate uncertainty in Smith's S via boot-strapping for top 8 items in terms of Smith's S, using 1,000 iterations for each item
#' S_boot <- SalienceBoot(fakeData.sal0, var_sel = "TOP", top = 8, iterations = 1000, seed = 182, IDs_first = TRUE)
#' 
#' ## Summarise results, displaying median Smith's S estimate and 95% percentile intervals
#' res <- SalienceEstimateSummary(S_boot, quantiles = c(0.025, 0.5, 0.975))
#' 
#' ## Calculate Smith's S using the 'fakeData.s' dataset (this is the output used for standard flower-plots in AnthroTools)
#' fake.S <- SalienceByCode(fakeData.s, Subj = "Subj", CODE = "CODE", Salience = "CODE.S", dealWithDoubles = "MAX")
#' 
#' ## Create flowerplot with uncertainty estimates - First, make margins wider so looks nicer
#' par(mar = c(0, 0, 0, 0))
#' FlowerPlotIntervals(SmithsS = fake.S, S_uncert = res, label = "Fruits")
#' 
#' ## Or, if wanted to display 89% uncertainty intervals
#' res <- SalienceEstimateSummary(S_boot, quantiles = c(0.055, 0.5, 0.945))
#' par(mar = c(0, 0, 0, 0))
#' FlowerPlotIntervals(SmithsS = fake.S, S_uncert = res, label = "Fruits", lower_int = "5.5%", upper_int = "94.5%")
#'
FlowerPlotIntervals <- function(SmithsS, S_uncert, label, lower_int = "2.5%", upper_int = "97.5%") {
  
  # If fewer than 8 items, add a warning saying that the 'flowerPlot' commands expects a minimum of 8 items
  if (length(S_uncert) < 8) {
    stop("There are less than 8 items with uncertainty intervals. This command expects a minimum of 8 items, else the flower plot does not display properly.")
  }
  
  # Make sure all variables with uncertainty intervals are in the original Smith's S summary dataset
  items_noUncert <- SmithsS[, 1]
  items_uncert <- rownames(S_uncert)
  
  if(any(!items_uncert %in% items_noUncert) == TRUE) {
    stop("One or more item names in the 'S_uncert' dataset is/are not in the 'SmithsS' dataset - function aborted. Please check datasets carefully and try again.")
  }
  
  # Edit the Smith's S output without uncertainty to include intervals
  for (i in 1:length(items_uncert)) {
    name <- items_uncert[i]
    SmithsS$CODE[SmithsS$CODE == name] <- paste0(name, "\n[", format(S_uncert[i, lower_int], nsmall = 2), "-", 
                                                 format(S_uncert[i, upper_int], nsmall = 2), "]")
  }
  
  return(FlowerPlot(SmithsS, label))
}
