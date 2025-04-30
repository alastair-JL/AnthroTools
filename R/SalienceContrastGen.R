#' SalienceContrastGen
#' 
#' Given a list of vectors of Smith's S estimates, calculate contrasts between each item. Contrasts are either on the absolute difference scale (e.g., a 0.2 unit difference in Smith's S estimates) or on the ratio scale (e.g., item x has double the Smith's S cultural salience of item y). The input to this function is the output to commands 'SalienceBoot()', 'SalienceZOIB()' or 'SalienceOrdBeta()'.
#' @usage SalienceContrastGen(data, contrast = "absolute_diff")
#' @param data This is a list of vectors of Smith's S estimates for a range of items items (i.e., commands 'SalienceBoot()', 'SalienceZOIB()' or 'SalienceOrdBeta()'). 
#' @param contrast String to denote whether to calculate contrasts on the absolute difference scale ("absolute_diff"; e.g., a 0.2 unit difference in Smith's S estimates) or on the ratio scale ("ratio_diff"; e.g., item x has double the Smith's S cultural salience of item y). Absolute difference is the default.
#' @keywords FreeList, Smith's S, Cultural salience
#' @return A list of vectors for the contrasts in Smith's S between each item.
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
SalienceContrastGen <- function(data, contrast = "absolute_diff") {
  
  # List to store results in
  res_list <- list()
  
  # Keep a counter of number of contrasts
  counter <- 0
  
  # Loop over each of the variables
  for (i in 1:length(names(data))) { 
    
    # Take each variable combination
    var_i <- names(data)[i] 
    for (j in 1:length(names(data))) {
      var_j <- names(data)[j] 
      
      # Skip if same pairing
      if (var_i == var_j) next
      
      print(paste0("On variables ", i, ": ", var_i, " and ", j, ": ", var_j))
      
      # Vector to store results in
      diff <- rep(NA, length(data[[var_i]]))
      
      # If want results in terms of difference in values vs difference in ratio of values
      for (k in 1:length(data[[var_i]])) {
        if (contrast == "absolute_diff") {
          diff[k] <- data[[var_j]][k] - data[[var_i]][k]
        } else if (contrast == "ratio_diff") {
          diff[k] <- data[[var_j]][k] / data[[var_i]][k]
        }
      }
      
      counter <- counter + 1
      
      # Store results in list
      res_list[[counter]] <- diff
      names(res_list)[counter] <- paste0(var_i, "X_X", var_j)
    }
  }
  return(res_list)
}
