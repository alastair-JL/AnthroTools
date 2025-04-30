\name{SalienceOrdBeta}
\alias{SalienceOrdBeta}
\title{SalienceOrdBeta}
\usage{
SalienceOrdBeta(data, var_sel, variables, top, threshold, cut_no0s = -10, cut_no1s = 10, 
		print_model = TRUE, seed, chains = 4, iterations = 2000, warmup = 1000, cores = 4, 
		priors, IDs_first = TRUE)
}
\arguments{
\item{data}{This is your data frame of item saliences (i.e., the output of 'FreeListTable(... , tableType = "MAX_SALIENCE")' function). Make sure there are no missing values.}

\item{var_sel}{Method to select variables to perform ordered Beta model on. Options are: i) Manually select single or multiple variables ("MANUAL"); ii) select items on the top x variables with the highest Smith's S values ("TOP"); and iii) any items with Smith's S values above the specified threshold ("THRESH").}

\item{variables}{The manually-specified variables to perform ordered Beta model on. Either a single variable name or a vector of variable names. Only to be used when var_sel = "MANUAL".}

\item{top}{The number of variables to perform ordered Beta model on (i.e., the x variables with the highest Smith's S values). Only to be used when var_sel = "TOP".}

\item{threshold}{The minimum Smith's S threshold for inclusion in the ordered Beta model process. Only to be used when var_sel = "THRESH".}

\item{cut_no0s}{If there are no zeroes observed in the data, this is the probability (on the logit scale) of observing a 0 (default = -10; i.e., effectively zero).}

\item{cut_no1s}{If there are no ones observed in the data, this is the probability (on the logit scale) of observing a 1 (default = 10; i.e., effectively zero).}

\item{print_model}{Whether to print the model output or not (default = TRUE).}

\item{seed}{Specify a seed so that results are reproducible.}

\item{chains}{The number of chains for the model to run on (default = 4).}

\item{iterations}{The total number of iterations per chain (default = 2,000).}

\item{warmup}{The total number of warmup iterations per chain (default = 1,000).}

\item{cores}{The number of cores to run analyses on in parallel (default = 4).}

\item{priors}{Specification of priors. If no priors are specified, the default non/weakly-informative priors are used.}

\item{IDs_first}{Specifies whether the first row of the dataset is a list of IDs (default = TRUE).}
}
\value{
The value returned is a list of vectors of Smith's S estimates from each of the posterior samples, for each variable selected.
}
\description{
Given a data frame of item saliences, including 0s and with no missing values, for each item listed in the sample (i.e., the output of 'FreeListTable(... , tableType = "MAX_SALIENCE")' function), run Bayesian ordered Beta model on a variable/range of variables. This will estimate the observed item saliences using an ordered Beta model, then calculate Smith's S cultural salience in each of the posterior samples. 

Methods to select variables are: i) Manually select single or multiple variables; ii) select items on the top x variables with the highest Smith's S values; and iii) any items with Smith's S values above the specified threshold. 

Note that if there are no zeroes and/or no ones in the data, the function will adapt the ordered Beta model accordingly (e.g., if there are no zeroes, then hard-coding the zero cut-point to a probability of effectively zero). 

This function returns a list of vectors of these boot-strapped Smith's S estimates from each sample, for each variable selected. This command uses the 'brms' and 'ordbetareg' packages, and so requires 'brms', 'ordbetareg' and 'stan' to be installed. Note also that by default this function simply uses the default non-/weakly-informative priors associated with the 'ordbetareg' package.}
\examples{
## Generate fake free-list data about fruits
set.seed(41)
fakeData <- GenerateFakeFreeListData() 

## Calculate item salience
fakeData.s <- CalculateSalience(fakeData, Subj = "Subj", Order = "Order", 
		CODE = "CODE", Salience = "CODE.S")

## Convert to data frame with maximum item saliences for each item as 
## separate rows, and including 0s
fakeData.sal0 <- FreeListTable(fakeData.s, Subj = "Subj", Order = "Order", 
		CODE = "CODE", Salience = "CODE.S", tableType = "MAX_SALIENCE")
head(fakeData.sal0)


### Example of different combinations of options

## First, calculate uncertainty in Smith's S using ordered Beta model 
## for item 'apple' (using default options)
S_ordBeta <- SalienceOrdBeta(fakeData.sal0, var_sel = "MANUAL", 
		variables = "apple", seed = 182, IDs_first = TRUE)

## Summarise results
hist(S_ordBeta$apple)
summary(S_ordBeta$apple)
quantile(S_ordBeta$apple, c(0.025, 0.5, 0.975))


## Same example as above, but specifying more informative priors
S_ordBeta_priors <- SalienceOrdBeta(fakeData.sal0, var_sel = "MANUAL", 
		variables = "apple", seed = 182, IDs_first = TRUE, 
		priors = c(prior(normal(0, 1.5), class = Intercept), 
				prior(normal(0, 1), class = Intercept, dpar = phi), 
				prior(normal(0, 1), class = cutzero), 
				prior(normal(0, 1), class = cutone)))

## Summarise results
hist(S_ordBeta_priors$apple)
summary(S_ordBeta_priors$apple)
quantile(S_ordBeta_priors$apple, c(0.025, 0.5, 0.975))


## Note that care needs to be taken when specifying priors, as if there 
## are no 1s and/or 0s, and the model specification changes (i.e., some 
## cut-points of the ordered Beta model are fixed), specifying priors 
## will result in the model not working - Will demonstrate using 
## example of 'pear', where there are no item saliences of '1'.
S_ordBeta_pear <- SalienceOrdBeta(fakeData.sal0, var_sel = "MANUAL", 
		variables = "pear", seed = 182, IDs_first = TRUE, 
		priors = c(prior(normal(0, 1.5), class = Intercept), 
				prior(normal(0, 1), class = Intercept, dpar = phi), 
				prior(normal(0, 1), class = cutzero), 
				prior(normal(0, 1), class = cutone)))

# But the following would work, if the prior for the cutone' term was removed
S_ordBeta_pear <- SalienceOrdBeta(fakeData.sal0, var_sel = "MANUAL", 
		variables = "pear", seed = 182, IDs_first = TRUE, 
		priors = c(prior(normal(0, 1.5), class = Intercept), 
				prior(normal(0, 1), class = Intercept, dpar = phi), 
				prior(normal(0, 1), class = cutzero)))

# Care therefore needs to be taken when manually specifying priors, 
## particularly when looping over multiple items as different priors 
## may be needed for different models if the parameters differ.


## Next, calculate uncertainty in Smith's S using ordered Beta model for 
## items 'apple', 'peach' and 'lemon' (using default options)
S_ordBeta <- SalienceOrdBeta(fakeData.sal0, var_sel = "MANUAL", 
		variables = c("apple", "peach", "lemon"), 
		seed = 182, IDs_first = TRUE)

## Summarise results
quantile(S_ordBeta$apple, c(0.025, 0.5, 0.975))
quantile(S_ordBeta$peach, c(0.025, 0.5, 0.975))
quantile(S_ordBeta$lemon, c(0.025, 0.5, 0.975))


## Next, calculate uncertainty in Smith's S using ordered Beta model for 
## top 3 items in terms of Smith's S (using default options)
S_ordBeta <- SalienceOrdBeta(fakeData.sal0, var_sel = "TOP", 
		top = 3, seed = 182, IDs_first = TRUE)

## Summarise results
names(S_ordBeta)
quantile(S_ordBeta$apple, c(0.025, 0.5, 0.975))
quantile(S_ordBeta$banana, c(0.025, 0.5, 0.975))
quantile(S_ordBeta$peach, c(0.025, 0.5, 0.975))

 
## Finally, calculate uncertainty in Smith's S using ordered Beta model 
## for any items with a Smith's S value above 0.2 (using default options)
S_ordBeta <- SalienceOrdBeta(fakeData.sal0, var_sel = "THRESH", 
		threshold = 0.2, seed = 182, IDs_first = TRUE)

## Summarise results
names(S_ordBeta)
quantile(S_ordBeta$apple, c(0.025, 0.5, 0.975))
quantile(S_ordBeta$banana, c(0.025, 0.5, 0.975))
quantile(S_ordBeta$peach, c(0.025, 0.5, 0.975))
quantile(S_ordBeta$plum, c(0.025, 0.5, 0.975))

}
\author{
Daniel Major-Smith. <dan.major-smith@cas.au.dk>

Benjamin Grant Purzycki. <bgpurzycki@cas.au.dk>
}
\keyword{FreeList, Smith's S, Cultural salience, Bayesian, brms, ordered Beta}