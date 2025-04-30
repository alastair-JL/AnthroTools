#' SalienceContrastSummary
#' 
#' Given a list of vectors of contrasts of Smith's S estimates, summarise each vector in the list to calculate uncertainty intervals (e.g., percentile intervals from boot-strapping, or credible intervals from Bayesian models). The input to this function is the output of the 'SalienceContrastGen()' command.
#' @usage SalienceContrastSummary(data, target, quantiles = c(0, 0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975, 1))
#' @param data This is a list of vectors of contrasts of Smith's S estimates (i.e., the output of the 'SalienceContrastGen()' command).
#' @param target The baseline item level to compare contrasts against.
#' @param quantiles The specific quantiles/uncertainty intervals to estimate (default = c(0, 0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975, 1)).
#' @keywords FreeList, Smith's S, Cultural salience
#' @return A data frame of contrasts of Smith's S estimates and uncertainty intervals for each item, relative to the target baseline item.
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
#' ## Calculate uncertainty in Smith's S via boot-strapping for top 6 items in terms of Smith's S, using 1,000 iterations for each item
#' S_boot <- SalienceBoot(fakeData.sal0, var_sel = "TOP", top = 6, iterations = 1000, seed = 182, IDs_first = TRUE)
#' 
#' ## Contrasts between each item (on absolute difference scale)
#' S_contrasts <- SalienceContrastGen(S_boot, contrast = "absolute_diff")
#' 
#' ## Summary of contrasts for 'apple'
#' SalienceContrastSummary(S_contrasts, target = "apple", quantiles = c(0.025, 0.5, 0.975))
#'
SalienceContrastSummary <- function(data, target, quantiles = c(0, 0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975, 1)) {
  
  # Generate uncertainty intervals for all contrasts
  con_temp <- round(t(as.data.frame(lapply(data, quantile, probs = quantiles), check.names = FALSE)), 2)
  con_temp <- as.data.frame(con_temp)
  
  # Now just take those where the target is the baseline
  names_temp <- as.data.frame(do.call(rbind, strsplit(rownames(con_temp), split = "X_X")))
  con_temp <- cbind(names_temp, con_temp)
  colnames(con_temp)[colnames(con_temp) == "V1"] <- "base_level"
  colnames(con_temp)[colnames(con_temp) == "V2"] <- "contrast_level"
  res <- con_temp[con_temp$base_level == target, ]
  
  # Remove row names and tidy output
  rownames(res) <- NULL
  
  return(res)
}
