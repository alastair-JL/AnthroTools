#' SalienceContrastPlot
#' 
#' Plotting function to generate density plots of contrasts in Smith's S estimates over a range of items. The input to this function is the output of the 'SalienceContrastGen()' command; i.e., a list of vectors of Smith's S contrasts between items. Note that this command relies on the 'tidyverse', 'ggplot2' and 'ggdist' packages - Make sure that these are installed and loaded.
#' @usage SalienceContrastPlot(data, target, order = "high-low", manual_order)
#' @param data This is a list of vectors of contrasts in Smith's S estimates (i.e., the output of the 'SalienceContrastGen()' command). 
#' @param target The baseline item level to compare contrasts against.
#' @param order String to denote the order in which to display items on the plot. Options are: "high-low" (from highest to lowest; the default), "low-high" (from lowest to highest), "alpha" (alphabetically), and "manual" (manually specify order - see 'manual_order' option)
#' @param manual_order A vector of variable names specifying the order in which to display items on the plot. Only to be used if 'order = "manual"' (else is ignored).
#' @keywords FreeList, Smith's S, Cultural salience
#' @return A series of distributions for each item contrast by Smith's S estimates, along with 80% and 95% uncertainty intervals.
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
#' ## Plot of these results
#' # Ranked from highest to lowest
#' SalienceContrastPlot(data = S_contrasts, target = "apple", order = "high-low")
#' 
#' # And using manual specification
#' SalienceContrastPlot(data = S_contrasts, target = "apple", order = "manual", manual_order = c("banana", "orange", "pear", "plum", "peach"))
#'
SalienceContrastPlot <- function(data, target, order = "high-low", manual_order) {
  
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
  res_diff <- as.data.frame(data, check.names = FALSE)
  
  # Make long format and separate comparison levels
  res_diff <- res_diff %>% 
    pivot_longer(cols = everything(), names_to = "contrast", values_to = "S") %>%
    separate(contrast, c("base_level", "contrast_level"), sep = "X_X", remove = FALSE) 
  
  # Keep just target as the base level
  res_diff_target <- res_diff[res_diff$base_level == target, ]
  
  # Specify the levels - Default = from highest Smith's S to lowest
  if (order == "high-low") {
    x <- res_diff_target %>%
      group_by(contrast_level) %>%
      summarise(median = median(S)) %>%
      arrange(-median)
    levels <- x$contrast_level
  } else if (order == "low-high") {
    x <- res_diff_target %>%
      group_by(contrast_level) %>%
      summarise(median = median(S)) %>%
      arrange(median)
    levels <- x$contrast_level
  } else if (order == "alpha") {
    levels <- unique(res_diff$base_level)[order(unique(res_diff$base_level))]
    levels <- levels[-which(levels == target)]
  } else if (order == "manual") {
    levels <- manual_order
  }
  
  # Order the contrast levels
  res_diff_target <- res_diff_target %>%
    mutate(contrast_level = factor(contrast_level, levels = levels)) %>%
    arrange(contrast_level)
  
  # Plot of results
  p_dist <- ggplot(res_diff_target, aes(x = S, y = forcats::fct_rev(contrast_level), 
                                        fill = forcats::fct_rev(contrast_level))) +
      stat_halfeye(.width = c(0.8, 0.95), point_interval = "median_qi") +
      scale_fill_viridis_d(option = "plasma", begin = 0.3, end = 0.9) +
      guides(fill = "none") +
      labs(x = expression("Difference in" ~ italic(hat(S)) ~ "cultural salience"), y = "Item") +
      theme_bw() +
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 18),
            axis.line = element_line(colour = "black"),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())
  
  return(p_dist)
}
