#' SalienceBoot
#' 
#' Given a data frame of item saliences, including 0s and with no missing values, for each item listed in the sample (i.e., the output of 'FreeListTable(... , tableType = "MAX_SALIENCE")' function), performing bootstrapping on a variable/range of variables. This will randomly sample, with replacement, the observed item saliences x (default = 1,000) times, and calculate Smith's S cultural salience in each boot-strapped sample for each item. Methods to select variables are: i) Manually select single or multiple variables; ii) select items on the top x (default = 8) variables with the highest Smith's S values; and iii) any items with Smith's S values above the specified threshold (default = 0.1). This function returns a list of vectors of these boot-strapped Smith's S estimates from each sample, for each variable selected.
#' @usage SalienceBoot(data, var_sel, variables, top, threshold, iterations = 1000, seed, IDs_first = TRUE)
#' @param data This is your data frame of item saliences (i.e., the output of 'FreeListTable(... , tableType = "MAX_SALIENCE")' function). Make sure there are no missing values.
#' @param var_sel Method to select variables to perform bootstrapping on. Options are: i) Manually select single or multiple variables ("MANUAL"); ii) select items on the top x (default = 8) variables with the highest Smith's S values ("TOP"); and iii) any items with Smith's S values above the specified threshold (default = 0.1; "THRESH").
#' @param variables The manually-specified variables to perform boot-strapping on. Either a single variable name or a vector of variable names. Only to be used when var_sel = "MANUAL".
#' @param top The number of variables to perform boot-strapping on (i.e., the x variables with the highest Smith's S values; default = 8). Only to be used when var_sel = "TOP".
#' @param threshold The minimum Smith's S threshold for inclusion in the boot-strapping process (default = 0.1). Only to be used when var_sel = "THRESH".
#' @param iterations The number of iterations to perform bootstrapping (default = 1,000).
#' @param seed Specify a seed so that results are reproducible.
#' @param IDs_first Specifies whether the first row of the dataset is a list of IDs (default = TRUE).
#' @keywords FreeList, Smith's S, Cultural salience, Bootstrapping
#' @return The value returned is a list of vectors of these boot-strapped Smith's S estimates from each sample, for each variable selected.
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
#' 
#' ### Example of different combinations of options
#' 
#' ## First, calculate uncertainty in Smith's S via boot-strapping for item 'pear' using 1,000 iterations
#' S_boot <- SalienceBoot(fakeData.sal0, var_sel = "MANUAL", variables = "pear", iterations = 1000, seed = 182, IDs_first = TRUE)
#' 
#' ## Summarise results
#' hist(S_boot$pear)
#' summary(S_boot$pear)
#' quantile(S_boot$pear, c(0.025, 0.5, 0.975))
#' 
#' 
#' ## Next, calculate uncertainty in Smith's S via boot-strapping for items 'pear', 'peach' and 'lemon' using 1,000 iterations for each item
#' S_boot <- SalienceBoot(fakeData.sal0, var_sel = "MANUAL", variables = c("pear", "peach", "lemon"), iterations = 1000, seed = 182, IDs_first = TRUE)
#' 
#' ## Summarise results
#' quantile(S_boot$pear, c(0.025, 0.5, 0.975))
#' quantile(S_boot$peach, c(0.025, 0.5, 0.975))
#' quantile(S_boot$lemon, c(0.025, 0.5, 0.975))
#' 
#' 
#' ## Next, calculate uncertainty in Smith's S via boot-strapping for top 3 items in terms of Smith's S, using 1,000 iterations for each item
#' S_boot <- SalienceBoot(fakeData.sal0, var_sel = "TOP", top = 3, iterations = 1000, seed = 182, IDs_first = TRUE)
#' 
#' ## Summarise results
#' names(S_boot)
#' quantile(S_boot$apple, c(0.025, 0.5, 0.975))
#' quantile(S_boot$banana, c(0.025, 0.5, 0.975))
#' quantile(S_boot$peach, c(0.025, 0.5, 0.975))
#' 
#' 
#' ## Finally, calculate uncertainty in Smith's S via boot-strapping for any items with a Smith's S value above 0.2, using 1,000 iterations for each item
#' S_boot <- SalienceBoot(fakeData.sal0, var_sel = "THRESH", threshold = 0.2, iterations = 1000, seed = 182, IDs_first = TRUE)
#' 
#' ## Summarise results
#' names(S_boot)
#' quantile(S_boot$apple, c(0.025, 0.5, 0.975))
#' quantile(S_boot$banana, c(0.025, 0.5, 0.975))
#' quantile(S_boot$peach, c(0.025, 0.5, 0.975))
#' quantile(S_boot$plum, c(0.025, 0.5, 0.975))
#'
SalienceBoot <- function(data, var_sel, variables, top, threshold, iterations = 1000, seed, IDs_first = TRUE) {
  
  # Exit if variable selection method does not match option selected
  if (var_sel == "MANUAL" & missing(variables)) {
    stop("Variable selection method selected as 'MANUAL', but no variables provided.")
  }
  
  if (var_sel == "TOP" & missing(top)) {
    stop("Variable selection method selected as 'TOP', but no 'top' option provided.")
  }
  
  if (var_sel == "THRESH" & missing(threshold)) {
    stop("Variable selection method selected as 'THRESH', but no 'threshold' option provided.")
  }
  
  # Some sanity checks and warnings if combinations of variable selection method and other options are misspecified
  if (var_sel != "MANUAL" & var_sel != "TOP" & var_sel != "THRESH") {
    stop("Incorrect 'var_sel' option specified. Please check and try again.")
  }
  
  # Sanity check that the specified variables are in the data frame (if specified manually). Exit if not.
  if (var_sel == "MANUAL") {
    if (any(!variables %in% colnames(data)) == TRUE) {
      stop("One or more specified variables is not in the dataset - Function aborted.")
    }
  }
  
  # For 'top' option, make sure number of items selected is same or smaller than the number of items in the dataset
  if (var_sel == "TOP") {
    if (IDs_first == TRUE) {
      items <- length(colnames(data[, -1]))
    } else if (IDs_first == FALSE) {
      items <- length(colnames(data))
    }
    
    if (top >= items) {
      print(paste0("The number of 'top' variables requested is larger than the number of variables in the dataset. changing 'top' from ", top, " to ", items))
      top <- items
    }
  }
  
  # For 'threshold' option, make sure is between 0 and 1
  if (var_sel == "THRESH") {
    if (threshold < 0 | threshold >= 1) {
      stop("Value for 'threshold' is < 0 or >= 1 - This is impossible!")
    }
  }
  
  # Sanity check that the variables in the dataset have no missing data, or values < 0 and/or > 1. Exit if any do. Vary depending on whether first column is IDs or not
  if (IDs_first == TRUE) {
    for (i in 2:ncol(data)) { 
      var <- colnames(data)[i]
      if (any(is.na(data[[var]])) == TRUE) {
        stop("One or more specified variables have missing data - Function aborted.")
      }
      if (min(data[[var]]) < 0 | max(data[[var]]) > 1) {
        stop("One or more specified variables have values less than zero and/or greater than one - Function aborted.")
      }
    }
  } else if (IDs_first == FALSE) {
    for (i in 1:ncol(data)) { 
      var <- colnames(data)[i]
      if (any(is.na(data[[var]])) == TRUE) {
        stop("One or more specified variables have missing data - Function aborted.")
      }
      if (min(data[[var]]) < 0 | max(data[[var]]) > 1) {
        stop("One or more specified variables have values less than zero and/or greater than one - Function aborted.")
      }
    }
  }
  
  # Make a vector of all the variables to perform analyses on
  if (var_sel == "MANUAL") {
    
    # If "MANUAL", just use user-given string or vector
    vars <- variables
    
  } else if (var_sel == "TOP") {
    
    # Select the top X variables in terms of Smith's S. Vary depending on whether IDs are in first column or not
    if (IDs_first == TRUE) {
      x <- colMeans(data[, -1])
    } else if (IDs_first == FALSE) {
      x <- colMeans(data)
    }
    
    x <- x[order(x, decreasing = TRUE)] # Order by Smith's S
    x <- x[1:top] # Take top X variables
    vars <- attributes(x)$names # Extract variable names
    
  } else if (var_sel == "THRESH") {
    
    # Select variables above given Smith's S threshold. Vary depending on whether IDs are in first column or not
    if (IDs_first == TRUE) {
      x <- colMeans(data[, -1])
    } else if (IDs_first == FALSE) {
      x <- colMeans(data)
    }
    
    x <- x[order(x, decreasing = TRUE)] # Order by Smith's S
    x <- x[x >= threshold] # Keep variables if greater than threshold
    vars <- attributes(x)$names # Extract variable names
    
    # Make sure are some variables above the threshold value
    if (length(vars) == 0) {
      stop("Threshold value of ", threshold, " is too high - There are no variables with Smith's S above this value. Function aborted.")
    }
  }
  
  # Set seed
  set.seed(seed)
  
  # List to store results in
  res_list <- list()
  
  # Loop over each variable specified
  for (i in 1:length(vars)) { 
    
    S_boot <- rep(NA, iterations) # Empty vector to store results in
    var <- vars[i] # Take each variable
    
    print(paste0("Item ", i, " of ", length(vars), " is: ", var))
    
    # Perform bootstrapping
    for (j in 1:iterations) {
      boot <- sample(data[[var]], size = nrow(data), replace = TRUE)
      S_boot[j] <- mean(boot)
    }
    
    # Store results in list
    res_list[[i]] <- S_boot
    names(res_list)[i] <- var
  }
  return(res_list)
}
