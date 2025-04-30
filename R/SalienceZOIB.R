#' SalienceZOIB
#' 
#' Given a data frame of item saliences, including 0s and no missing data, for each item listed in the sample (i.e., the output of 'FreeListTable(... , tableType = "MAX_SALIENCE")' function), run Bayesian zero-one inflated beta (ZOIB) model on a variable/range of variables. This will estimate the observed item saliences using a ZOIB model, then calculate Smith's S cultural salience in each of the posterior samples. Methods to select variables are: i) Manually select single or multiple variables; ii) select items on the top x variables with the highest Smith's S values; and iii) any items with Smith's S values above the specified threshold. Note that if there are no zeroes and/or no ones in the data, the function will adapt the ZOIB model accordingly (e.g., if there are no zeroes, then hard-coding the one-inflated term to '0', meaning "of all the zeroes and ones in the dataset, there are no ones"). This function returns a list of vectors of these boot-strapped Smith's S estimates from each sample, for each variable selected. This command uses the 'brms' package, and so requires both 'brms' and 'stan' to be installed. Note also that by default this function simply uses the default non-/weakly-informative priors associated with the 'brms' package.
#' @usage SalienceZOIB(data, var_sel, variables, top, threshold, print_model = TRUE, seed, 
#'    chains = 4, iterations = 2000, warmup = 1000, cores = 4, priors, IDs_first = TRUE)
#' @param data This is your data frame of item saliences (i.e., the output of 'FreeListTable(... , tableType = "MAX_SALIENCE")' function). Make sure there are no missing values.
#' @param var_sel Method to select variables to perform ZOIB model on. Options are: i) Manually select single or multiple variables ("MANUAL"); ii) select items on the top x variables with the highest Smith's S values ("TOP"); and iii) any items with Smith's S values above the specified threshold ("THRESH").
#' @param variables The manually-specified variables to perform ZOIB model on. Either a single variable name or a vector of variable names. Only to be used when var_sel = "MANUAL".
#' @param top The number of variables to perform ZOIB model on (i.e., the x variables with the highest Smith's S values). Only to be used when var_sel = "TOP".
#' @param threshold The minimum Smith's S threshold for inclusion in the ZOIB model process. Only to be used when var_sel = "THRESH".
#' @param print_model Whether to print the model output or not (default = TRUE).
#' @param seed Specify a seed so that results are reproducible.
#' @param chains The number of chains for the model to run on (default = 4).
#' @param iterations The total number of iterations per chain (default = 2,000).
#' @param warmup The total number of warmup iterations per chain (default = 1,000).
#' @param cores The number of cores to run analyses on in parallel (default = 4).
#' @param priors Specification of priors. If no priors are specified, the default non/weakly-informative priors are used.
#' @param IDs_first Specifies whether the first row of the dataset is a list of IDs (default = TRUE).
#' @keywords FreeList, Smith's S, Cultural salience, Bayesian, brms, Zero-one inflated Beta
#' @return The value returned is a list of vectors of Smith's S estimates from each of the posterior samples, for each variable selected.
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
#' ## First, calculate uncertainty in Smith's S using ZOIB model for item 'apple' (using default options)
#' S_ZOIB <- SalienceZOIB(fakeData.sal0, var_sel = "MANUAL", variables = "apple", seed = 182, IDs_first = TRUE)
#' 
#' ## Summarise results
#' hist(S_ZOIB$apple)
#' summary(S_ZOIB$apple)
#' quantile(S_ZOIB$apple, c(0.025, 0.5, 0.975))
#' 
#' 
#' ## Same example as above, but specifying more informative priors
#' S_ZOIB_priors <- SalienceZOIB(fakeData.sal0, var_sel = "MANUAL", variables = "apple", seed = 182, IDs_first = TRUE, priors = c(prior(normal(0, 1.5), class = Intercept), prior(normal(0, 1), class = Intercept, dpar = phi), prior(normal(0, 1.5), class = Intercept, dpar = zoi), prior(normal(0, 1.5), class = Intercept, dpar = coi)))
#' 
#' ## Summarise results
#' hist(S_ZOIB_priors$apple)
#' summary(S_ZOIB_priors$apple)
#' quantile(S_ZOIB_priors$apple, c(0.025, 0.5, 0.975))
#' 
#' 
#' ## Note that care needs to be taken when specifying priors, as if there are no 1s and/or 0s, and the model specification changes (i.e., some elements of the ZOIB model are fixed), specifying priors will result in the model not working - Will demonstrate using example of 'pear', where there are no item saliences of '1'.
#' S_ZOIB_pear <- SalienceZOIB(fakeData.sal0, var_sel = "MANUAL", variables = "pear", seed = 182, IDs_first = TRUE, priors = c(prior(normal(0, 1.5), class = Intercept), prior(normal(0, 1), class = Intercept, dpar = phi), prior(normal(0, 1.5), class = Intercept, dpar = zoi), prior(normal(0, 1.5), class = Intercept, dpar = coi)))
#' 
#' # But the following would work, if the prior for the COI term was removed
#' S_ZOIB_pear <- SalienceZOIB(fakeData.sal0, var_sel = "MANUAL", variables = "pear", seed = 182, IDs_first = TRUE, priors = c(prior(normal(0, 1.5), class = Intercept), prior(normal(0, 1), class = Intercept, dpar = phi), prior(normal(0, 1.5), class = Intercept, dpar = zoi)))
#' 
#' # Care therefore needs to be taken when manually specifying priors, particularly when looping over multiple items as different priors may be needed for different models if the parameters differ.
#' 
#' 
#' ## Next, calculate uncertainty in Smith's S using ZOIB model for items 'apple', 'peach' and 'lemon' (using default options)
#' S_ZOIB <- SalienceZOIB(fakeData.sal0, var_sel = "MANUAL", variables = c("apple", "peach", "lemon"), seed = 182, IDs_first = TRUE)
#' 
#' ## Summarise results
#' quantile(S_ZOIB$apple, c(0.025, 0.5, 0.975))
#' quantile(S_ZOIB$peach, c(0.025, 0.5, 0.975))
#' quantile(S_ZOIB$lemon, c(0.025, 0.5, 0.975))
#' 
#' 
#' ## Next, calculate uncertainty in Smith's S using ZOIB model for top 3 items in terms of Smith's S (using default options)
#' S_ZOIB <- SalienceZOIB(fakeData.sal0, var_sel = "TOP", top = 3, seed = 182, IDs_first = TRUE)
#' 
#' ## Summarise results
#' names(S_ZOIB)
#' quantile(S_ZOIB$apple, c(0.025, 0.5, 0.975))
#' quantile(S_ZOIB$banana, c(0.025, 0.5, 0.975))
#' quantile(S_ZOIB$peach, c(0.025, 0.5, 0.975))
#' 
#' 
#' ## Finally, calculate uncertainty in Smith's S using ZOIB model for any items with a Smith's S value above 0.2 (using default options)
#' S_ZOIB <- SalienceZOIB(fakeData.sal0, var_sel = "THRESH", threshold = 0.2, seed = 182, IDs_first = TRUE)
#' 
#' ## Summarise results
#' names(S_ZOIB)
#' quantile(S_ZOIB$apple, c(0.025, 0.5, 0.975))
#' quantile(S_ZOIB$banana, c(0.025, 0.5, 0.975))
#' quantile(S_ZOIB$peach, c(0.025, 0.5, 0.975))
#' quantile(S_ZOIB$plum, c(0.025, 0.5, 0.975))
#'
SalienceZOIB <- function(data, var_sel, variables, top, threshold, print_model = TRUE, seed, chains = 4, iterations = 2000, warmup = 1000, cores = 4, priors, IDs_first = TRUE) {
  
  # Make sure that 'brms' package is loaded
  if (!"brms" %in% .packages()) {
    stop("Package brms is not loaded into library. Please load (or install then load, if necessary) and try again.")
  }
  
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
  
  # Loop over each variable specified, first checking for 0s and 1s, then running the appropriate ZOIB model
  for (i in 1:length(vars)) { 
    
    var <- vars[i] # Take each variable
    print(paste0("On variable ", i, ": ", var))
    
    # Make variable into a dataframe
    df_temp <- as.data.frame(cbind(v1 = data[[var]]))
    
    # See if 0s and 1s, to run appropriate ZOIB model
    any_0s <- ifelse(min(df_temp$v1) == 0, TRUE, FALSE)
    any_1s <- ifelse(max(df_temp$v1) == 1, TRUE, FALSE)
    
    model_type <- ifelse(any_0s == TRUE & any_1s == TRUE, "Standard", 
                         ifelse(any_0s == TRUE & any_1s == FALSE, "No 1s",
                                ifelse(any_0s == FALSE & any_1s == TRUE, "No 0s", "No 0s or 1s")))
    
    ## Run appropriate ZOIB model
    if (model_type == "Standard") {
      
      print(paste0("There are both 0s and 1s for variable ", var, ". Running standard ZOIB."))
      
      # Extract default priors, if not manually specified
      if (missing(priors)) {
        p <- get_prior(formula = bf(v1 ~ 1, phi ~ 1, zoi ~ 1, coi ~ 1), 
                            data = df_temp,
                            family = zero_one_inflated_beta())
      } else if (!missing(priors)) {
        p <- priors
      }
      
      # Run the model
      zoib_mod <- brm(formula = bf(v1 ~ 1, phi ~ 1, zoi ~ 1, coi ~ 1), 
                      data = df_temp,
                      prior = p,
                      chains = chains, iter = iterations, warmup = warmup, cores = cores,
                      family = zero_one_inflated_beta())
      
    } else if (model_type == "No 1s") {
      
      print(paste0("There are 0s but no 1s for variable ", var, ". Running ZOIB with coi term fixed to 0 (i.e., a zero-inflated beta model)."))
      
      # Extract default priors, if not manually specified
      if (missing(priors)) {
        p <- get_prior(formula = bf(v1 ~ 1, phi ~ 1, zoi ~ 1, coi = 0), 
                            data = df_temp,
                            family = zero_one_inflated_beta())
      } else if (!missing(priors)) {
        p <- priors
      }
      
      # Run the model
      zoib_mod <- brm(formula = bf(v1 ~ 1, phi ~ 1, zoi ~ 1, coi = 0), 
                      data = df_temp,
                      prior = p,
                      chains = chains, iter = iterations, warmup = warmup, cores = cores,
                      family = zero_one_inflated_beta())
      
    } else if (model_type == "No 0s") {
      
      print(paste0("There are 1s but no 0s for variable ", var, ". Running ZOIB with coi term fixed to 1 (i.e., a reversed zero-inflated beta model)."))
      
      # Extract default priors, if not manually specified
      if (missing(priors)) {
        p <- get_prior(formula = bf(v1 ~ 1, phi ~ 1, zoi ~ 1, coi = 1), 
                            data = df_temp,
                            family = zero_one_inflated_beta())
      } else if (!missing(priors)) {
        p <- priors
      }
      
      # Run the model
      zoib_mod <- brm(formula = bf(v1 ~ 1, phi ~ 1, zoi ~ 1, coi = 1), 
                      data = df_temp,
                      prior = p,
                      chains = chains, iter = iterations, warmup = warmup, cores = cores,
                      family = zero_one_inflated_beta())
      
    } else if (model_type == "No 0s or 1s") {
      
      print(paste0("There are no 0s and no 1s for variable ", var, ". Running ZOIB with zoi and coi terms fixed to 0 (i.e., a standard beta model)."))
      
      # Extract default priors, if not manually specified
      if (missing(prior)) {
        p <- get_prior(formula = bf(v1 ~ 1, phi ~ 1, zoi = 0, coi = 0), 
                            data = df_temp,
                            family = zero_one_inflated_beta())
      } else if (!missing(priors)) {
        p <- priors
      }
      
      # Run the model
      zoib_mod <- brm(formula = bf(v1 ~ 1, phi ~ 1, zoi = 0, coi = 0), 
                      data = df_temp,
                      prior = p,
                      chains = chains, iter = iterations, warmup = warmup, cores = cores,
                      family = zero_one_inflated_beta())
      
    }
    
    if (print_model == TRUE) {
      print(zoib_mod)
    }
    
    # Posterior predictions from this model
    post_zoib <- predict(zoib_mod, summary = FALSE)
    
    # Smith's S in each posterior sample
    S_post_zoib <- rep(NA, nrow(post_zoib))
    for (j in 1:nrow(post_zoib)) {
      S_post_zoib[j] <- mean(post_zoib[j, ])
    }
    
    # Store results in list
    res_list[[i]] <- S_post_zoib
    names(res_list)[i] <- var
  }
  return(res_list)
}
