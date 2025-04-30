#' SalienceEstimatePlot
#' 
#' Plotting function to generate density plots of Smith's S estimates over a range of items. The input to this function is the output to commands 'SalienceBoot()', 'SalienceZOIB()' or 'SalienceOrdBeta()'; i.e., a list of vectors of Smith's S estimates. Note that this command relies on the 'tidyverse', 'ggplot2' and 'ggdist' packages - Make sure that these are installed and loaded.
#' @usage SalienceEstimatePlot(data, order = "high-low", manual_order)
#' @param data This is a list of vectors of Smith's S estimates (i.e., commands 'SalienceBoot()', 'SalienceZOIB()' or 'SalienceOrdBeta()').
#' @param order String to denote the order in which to display items on the plot. Options are: "high-low" (from highest to lowest; the default), "low-high" (from lowest to highest), "alpha" (alphabetically), and "manual" (manually specify order - see 'manual_order' option).
#' @param manual_order A vector of variable names specifying the order in which to display items on the plot. Only to be used if 'order = "manual"' (else this option is ignored).
#' @keywords FreeList, Smith's S, Cultural salience
#' @return A series of distributions of Smith's S estimates for each item, along with 80% and 95% uncertainty intervals.
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
#' ## Plot of these results
#' # Ranked from highest to lowest
#' SalienceEstimatePlot(S_boot, order = "high-low")
#' 
#' # And using manual specification
#' SalienceEstimatePlot(S_boot, order = "manual", manual_order = c("apple", "banana", "pear", "orange", "peach", "plum", "lemon"))
#'
SalienceEstimatePlot <- function(data, order = "high-low", manual_order) {
  
  # Make sure that 'tidyverse', 'ggplot2' and 'ggdist' packages are loaded
  if (!"tidyverse" %in% .packages()) {
    stop("Package tidyverse is not loaded into library. Please load (or install then load, if necessary) and try again.")
  }
  if (!"ggplot2" %in% .packages()) {
    stop("Package ggplot2 is not loaded into library. Please load (or install then load, if necessary) and try again.")
  }
  if (!"ggdist" %in% .packages()) {
    stop("Package ggdist is not loaded into library. Please load (or install then load, if necessary) and try again.")
  }
  
  # Convert list of samples to data frame
  res_full <- as.data.frame(data, check.names = FALSE)
  
  # Specify the levels - Default = from highest Smith's S to lowest
  if (order == "high-low") {
    x <- colMeans(res_full)
    x <- x[order(x, decreasing = TRUE)]
    levels <- attributes(x)$names
  } else if (order == "low-high") {
    x <- colMeans(res_full)
    x <- x[order(x, decreasing = FALSE)]
    levels <- attributes(x)$names
  } else if (order == "alpha") {
    levels <- names(data)[order(names(data))]
  } else if (order == "manual") {
    levels <- manual_order
  }
  
  # Make long format
  res_full <- res_full %>%
    pivot_longer(cols = everything(), names_to = "item", values_to = "S") %>%
    mutate(item = factor(item, levels = levels)) %>%
    arrange(item)
  
  # Plot of results
  p_dist <- ggplot(res_full, aes(x = S, y = forcats::fct_rev(item), fill = forcats::fct_rev(item))) +
      stat_halfeye(.width = c(0.8, 0.95), point_interval = "median_qi") +
      scale_fill_viridis_d(option = "plasma", begin = 0.3, end = 0.9) +
      guides(fill = "none") +
      labs(x = expression(italic(hat(S)) ~ "cultural salience"), y = "Item") +
      theme_bw() +
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 18),
            axis.line = element_line(colour = "black"),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())
  
  return(p_dist)
}
